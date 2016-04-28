#' @rdname OptPath
#' @aliases OptPathDB
#' @export
makeOptPathDB = function(par.set, y.names, minimize, add.transformed.x = FALSE,
      include.error.message = FALSE, include.exec.time = FALSE,
      include.extra = FALSE, file) {
  n.y = length(y.names)
  obj = makeOptPath(par.set, y.names, minimize, add.transformed.x,
      include.error.message, include.exec.time, include.extra)

  addClasses(obj, "OptPathDB")

  if (missing(file)) {
    file = tempfile()
  }
  assertString(file)
  if (file %nin% c("", ":memory:") && file.exists(file)) {
    stopf("File %s already exists and hence can not be used as db backend.",
        file)
  }
  db = dbConnect(SQLite(), file)
  
  obj$baseColNames = c(getParamIds(par.set, repeated = TRUE, with.nr = TRUE),
      y.names)

  table = makeDataFrame(nrow = 0, ncol = getParamNr(par.set, devectorize = TRUE)
      + n.y + error.message + exec.time + extra + 2,
      col.types = c(getParamTypes(par.set, df.cols = TRUE,
              df.discretes.as.factor = FALSE),
          rep("numeric", n.y),
          if (error.message) "character",
          if (exec.time) "numeric"),
      col.names = c(escapeRowid(obj$baseColNames), "dob", "eol",
          if (error.message) "error.message",
          if (exec.time) "exec.time")
  )

  dbWriteTable(db, "optpath", table)

  if (extra) {
    dbGetQuery("alter table optpath add column extra BLOB")
  }

  extendedColNames = c(colnames(table), if (extra) "extra") 

  obj$insertquery = paste0("insert into optpath values (",
      paste0(":", extendedColNames, collapse = ","), ")")
  
  een = escapeRowid(extendedColNames)
  obj$updatequery = paste("update optpath set",
      paste0(dbQuoteIdentifier(db, een), een, sep = "=:", collapse = ","),
      "where rowid=:rowid")

  dbGetQuery(db, "create table object (object blob)")
  dbGetPreparedQuery(db, "insert into object values (:object)",
      data.frame(object = I(list(serialize(saveObject, NULL)))))

  # we detect changes to the database by incrementing a value whenever a write
  # access happens.
  dbGetQuery(db, "create table access (count int)")
  dbGetQuery(db, "insert into access values (0)")
  triggerstr = paste("create trigger %scount after %s on optpath begin",
      "update access set count = count + 1;",
      "end")
  for (trigger in c("insert", "delete", "update")) {
    dbGetQuery(db, sprintf(triggerstr, trigger, trigger))
  }

  obj$file = file
  obj$dbenv = new.env(parent = emptyenv())
  obj$dbenv$db = db
  obj$dbenv$transaction = FALSE
  obj$dbenv$writelock = FALSE
  obj$dbenv$lockMessage = getDbLockedMessage()
  obj
}

#' load OptPathDB from database file
#'
#' @param file [\code{character(1)}]\cr
#'   File name of a database used previously by an \code{OptPathDB}.
#' 
#' @export
makeOptPathDBFromFile = function(file) {
  db = dbConnect(SQLite(), file)
  obj = unserialize(sqliteQuickColumn(db, "object", "object")[[1]])
  obj$dbenv = new.env(parent = emptyenv())
  obj$dbenv$db = db
  obj$file = file
  obj
}
