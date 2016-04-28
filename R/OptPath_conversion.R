
#' Convert the given OptPath object to a data.frame backed OptPath.
#'
#' If \code{op} is already an \code{OptPathDF}, this is a Noop and will NOT
#' make a deep copy of \code{op}.
#'
#' @template arg_op
#' @return [\code{OptPathDF}]
#' @export
#' @family optpath
as.OptPathDF = function(op) {
  UseMethod("as.OptPathDF")
}

#' Convert the given OptPath object to a database backed OptPath.
#'
#' If \code{op} is already an \code{OptPathDB} and \code{file} is missing or the
#' same file as the backend file of \code{op}, this is a Noop and will not make
#' a deep copy of \code{op}. If \code{file} is given and differs from the
#' backend file of \code{op}, a second \code{OptPathDB} object will be created.
#' Updates to this second update will not propagate to the original OptPath and
#' vice versa.
#'
#' @template arg_op
#' @param file [\code{character(1)]\cr
#'   File name to use as database backend. Will use a temporary file in the
#'   default temp file path by default.
#' @return [\code{OptPathDF}]
#' @export
#' @family optpath
as.OptPathDB = function(op, file) {
  UseMethod("as.OptPathDB")
}

#' @export
as.OptPathDF.OptPathDF = identity

#' @export
as.OptPathDF.OptPathDB = function(op) {
  newOp = cloneOptPathObject(op, "DF")

  ensureDbConnected(db)
  beginTransactionInternal(op)
  on.exit(endTransactionInternal(op))

  fullFrame = dbGetQuery(x$dbenv$db, "select * from optpath")
  colnames(fullFrame) = unescapeRowid(colnames(fullFrame))

  if (op$options$include.extra) {
    newOp$env$extra = lapply(fullFrame[, "extra"], unserialize)
  }
  if (op$options$include.exec.time) {
    newOp$env$exec.time = as.numeric(fullFrame[, "exec.time"])
  }
  if (op$options$include.error.message) {
    newOp$env$error.message = as.character(fullFrame[, "error.message"])
  }
  newOp$env$dob = as.integer(fullFrame[, "dob"])
  newOp$env$eol = as.integer(fullFrame[, "eol"])

  xycols = setdiff(colnames(fullFrame),
      c("extra", "exec.time", "error.message", "dob", "eol"))
  newOp$env$path = fullFrame[, xycols, drop = FALSE]

  newOp
}

#' @export
as.OptPathDB.OptPathDF = function(op, file) {
  newOp = cloneOptPathObject(op, "DB", file = file)
  
  frame = op$env$path
  frame = cbind(frame, dob = op$env$dob, eol = op$env$eol)
  if (op$options$include.error.message) {
    frame = cbind(frame, error.message = op$env$error.message)
  }
  if (op$options$include.exec.time) {
    frame = cbind(frame, exec.time = op$env$exec.time)
  }
  if (op$options$include.extra) {
    serialExtra = lapply(op$env$extra, serialize, connection = NULL)
    frame = cbind(frame, extra = I(serialExtra))
  }
  dbGetPreparedQuery(newOp$dbenv$db, newOp$insertquery, frame)
  newOp
}

#' @export
as.OptPathDB.OptPathDB = function(op, file) {
  if (missing(file) || op$file == file) {
    return(op)
  }
  newOp = cloneOptPathObject(op, "DB", file = file)
  dbGetPreparedQuery(newOp$dbenv$db, newOp$insertquery,
      dbGetQuery(op$dbenv$db, "select * from optpath"))
  newOp
}


cloneOptPathObject = function(op, newbase, ...) {
  assertChoice(newbase, "DF", "DB")
  switch(newbase,
      DF = makeOptPathDF,
      DB = makeOptPathDB)(op$par.set, op$y.names, op$minimize,
      op$add.transformed.x, op$options$include.error.message,
      op$options$include.exec.time, op$options$include.extra, ...)
}