
#' @export
getOptPathLength.OptPathDB = function(op) {
  ensureDbConnected(op)
  dbGetQuery(op$dbenv$db, "select count(*) from optpath")[[1]]
}

#' @export
getOptPathEl.OptPathDB = function(op, index) {
  index = asInt(index)
  # if there is ever a way to remove rows we will need to put this into a
  # transaction.
  n = getOptPathLength(op)
  if (!(index >= 1 && index <= n)) {
    stop("Index must be between 1 and ", n, "!")
  }

  row = dbGetQuery(op$dbenv$db,
      paste("select * from optpath limit 1 offset", index - 1))
  colnames(row) = unescapeRowid(colnames(row))
  nonXYNames = c("extra", "exec.time", "error.message", "dob", "eol")
  res = as.list(row[, intersect(colnames(row, nonXYNames))])
  res$y = unlist(row[op$y.names])
  res$x = dfRowToList(row[, setdiff(colnames(row), c(op$y.names, nonXYNames)),
          drop = FALSE])
  if (!is.null(res$extra)) {
    res$extra = unserialize(res$extra[[1]])
  }
  res
}

#' @export
getOptPathX.OptPathDB = function(op, dob, eol) {
  return(as.data.frame(op, include.x = TRUE, include.y = FALSE,
          include.rest = FALSE, dob = dob, eol = eol))
}

#' @export
getOptPathY.OptPathDB = function(op, names, dob, eol, drop = TRUE) {
  if (missing(names))
    names = op$y.names
  else
    assertSubset(names, op$y.names, empty.ok = FALSE)
  assertFlag(drop)
  
  frame = as.data.frame(op, include.x = FALSE, include.y = TRUE,
      include.rest = FALSE, dob = dob, eol = eol)
  y = as.matrix(frame[, names, drop = FALSE])
  if (drop && length(names) == 1L)
    y = as.numeric(y)
  return(y)
}

#' @export
getOptPathDOB.OptPathDB = function(op, dob, eol) {
  getOptPathDBCol(op, "dob", dob, eol)
}

#' @export
getOptPathEOL.OptPathDB = function(op, dob, eol) {
  getOptPathDBCol(op, "eol", dob, eol)
}

#' @export
getOptPathErrorMessages.OptPathDB = function(op, dob, eol) {
  if (op$options$include.error.message) {
    getOptPathDBCol(op, "error.message", dob, eol)
  }
}

#' @export
getOptPathExecTimes.OptPathDB = function(op, dob, eol) {
  if (op$options$include.exec.time) {
    getOptPathDBCol(op, "exec.time", dob, eol)
  }
}

getOptPathExtras.OptPathDB = function(op, dob, eol) {
  if (op$options$include.extra) {
    lapply(getOptPathDBCol(op, "extra", dob, eol), unserialize)
  }
}

#' @export
getOptPathCol.OptPathDB = function(op, name, dob, eol) {
  assertString(name)
  # if there is ever a way to remove rows we will need to put this into a
  # transaction. In that case we would need to take the
  # begin/endTransactionInternal OUT of 'getOptPathDBCol' and put it into each
  # of the getOptPathXXX.OptPathDB functions separately.
  if (getOptPathLength(op) == 0L)
    stopf("Trying to return a col from an empty opt.path")
  if (name %in% op$baseColNames)
    return(getOptPathDBCol(op, dbQuoteIdentifier(op$dbenv$db, escapeRowid(name)),
            dob, eol))
  if (name == "dob")
    return(getOptPathDOB(op, dob, eol))
  if (name == "eol")
    return(getOptPathEOL(op, dob, eol))
  if (name == "exec.time")
    return(getOptPathExecTimes(op, dob, eol))
  if (name == "error.message")
    return(getOptPathErrorMessages(op, dob, eol))
  firstExtra = getOptPathExtras(op)[[1]]
  if (name %in% names(firstExtra))
    return(extractSubList(getOptPathExtras(op, dob, eol), name))
  stop("The column you specified is not present in the opt.path.")
}

#' @export
getOptPathCols.OptPathDB = function(op, names, dob, eol, row.names = NULL) {
  assertCharacter(names, any.missing = FALSE)
  d = as.data.frame(op, dob = dob, eol = eol, row.names = row.names)
  return(d[, names, drop = FALSE])
}


