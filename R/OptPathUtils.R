

# return logical index for selection via dob and / or eol
getOptPathDFDobAndEolIndex = function(op, dob = op$env$dob, eol = op$env$eol) {
  op$env$dob %in% dob & op$env$eol %in% eol
}


checkAddOptPath = function(op, x, y, error.message, exec.time, extra,
    check.feasible) {
  assertList(x, len = length(op$par.set$pars))
  assertNumeric(y, len = length(op$y.names))
  assertString(error.message, na.ok = TRUE)
  assertNumber(exec.time, lower = 0, na.ok = TRUE)
  if (!is.null(extra)) {
    if (!op$options$include.extra)
      stopf("Trying to add extra info to opt path, without enabling that option!")
    assertList(extra)
    if (!isProperlyNamed(extra))
      stopf("'extra' must be properly named!")
    if (!all(sapply(extra, isScalarValue)))
      stopf("'extra' can currently only contain scalar values!")
  } else if (op$options$include.extra) {
    stopf("Option `extra` is enabled, but no extras provided!")
  }
  if (!is.na(error.message) && !op$options$include.error.message)
    stopf("Trying to add error.message to opt path, without enabling that option!")
  if (!is.na(exec.time) && !op$options$include.exec.time)
    stopf("Trying to add exec.time to opt path, without enabling that option!")
  
  if (check.feasible && !isFeasible(op$par.set, x)) {
    stop("Trying to add infeasible x values to opt path: ", convertToShortString(x))
  }
  
  # scalar_na -> single_NA, disc --> names, ints --> make sure int
  recode = function(ps, x)  {
    Map(function(p, v) {
          if (isScalarNA(v))
            v = getParamNA(p, repeated = TRUE)
          else if (p$type %in% c("discrete", "discretevector"))
            discreteValueToName(p, v)
          # we need to make sure cols in df do not get converted to num
          else if (p$type %in% c("integer", "integervector"))
            as.integer(v)
          else
            v
        }, ps$pars, x)
  }
}


escapeRowid = function(c) {
  gsub("rowid", "Xrowid", c)
}

unescapeRowid = function(c) {
  gsub("Xrowid", "rowid", c)
}

getOPDBDobAndEolIndex = function(op, dob, eol) {
  if (missing(dob) && missing(eol)) {
    # no db query necessary
    return(TRUE)
  }
  
  data = dbGetQuery(op$dbenv$db, "select dob, eol from optpath")
  if (missing(dob)) {
    dobSat = TRUE
  } else {
    dobSat = data$dob %in% dob
  }
  if (missing(eol)) {
    eolSat = TRUE
  } else {
    eolSat = data$eol %in% eol
  }
  dobSat & eolSat
}

getOptPathDBCol = function(op, col, dob, eol) {
  ensureDbConnected(op)
  beginTransactionInternal(op)
  on.exit(endTransactionInternal(op))
  frame = dbGetQuery(op$dbenv$db, sprintf("select %s from optpath", col))
  frame[getOPDBDobAndEolIndex(op, dob, eol), ]
}


buildOptPathDBRow = function(op, x, y, dob, eol = NA_integer_,
    error.message = NA_character_, exec.time = NA_real_, extra = NULL) {
  
  dob = asInt(dob, na.ok = TRUE)
  eol = asInt(eol, na.ok = TRUE)
  
  # add x and y
  x = recode(op$par.set, x)
  el = do.call(cbind, lapply(x, function(v) as.data.frame(t(v), stringsAsFactors = FALSE)))
  el = cbind(el, as.data.frame(as.list(y), stringsAsFactors = FALSE))
  colnames(el) = op$baseColNames
  el = cbind(el, dob = dob, eol = eol)
  
  # add extra
  if (!is.null(extra)) {
    extraOne = dbGetQuery(op$dbenv$db, "select extra from optpath limit 1")
    if (nrow(extraOne) > 0L) {
      if (!all(names(extra) == names(unserialize(extraOne[[1]][[1]]))))
        stopf("Trying to add unknown extra(s): %s!", paste(symdiff(names(extra), names(env$extra[[1L]])), collapse = ","))
    }
    el = cbind(el, list(extra = I(list(serialize(extra, NULL)))))
  }
  
  # potentially add errmsg and time
  if (op$option$include.error.message)
    el = cbind(el, error.message = error.message)
  if (op$option$include.exec.time)
    el = cbind(el, exec.time = exec.time)
}


ensureDbConnected = function(op) {
  if (!dbIsValid(op$dbenv$db)) {
    op$dbenv$db = dbConnect(SQLite(), op$file)
    op$dbenv$transaction = FALSE
    op$dbenv$writelock = FALSE
    op$dbenv$lockMessage = getDbLockedMessage()
  }
}

# Begin a transaction, block and wait for write access if 'needWriteLock'.
# if 'internal' == FALSE, this is essentially the functionality of 
# beginTransactionOptPathDB. If 'internal' == TRUE, the transaction is only
# started if the user has not started it already. The transaction MUST be
# finished with endTransactionInternal with 'internal' == TRUE before the flow
# of execution goes back outside ParamHelpers.
beginTransactionInternal = function(op, internal = TRUE,
    needWriteLock = FALSE) {
  
  if (internal && op$dbenv$transaction) {
    # already inside a (user initiated) transaction, so nothing to do
    if (needWriteLock && !op$dbenv$writelock) {
      stop("Write operation inside readonly transaction.")
    }
    return(NULL)
  }

  if (needWriteLock) {
    pollDbOperation(op$dbenv,
        dbGetQuery(op$dbenv$db, "begin immediate transaction"))
  } else {
    dbBegin(op$dbenv$db)
  }

  op$dbenv$writelock = needWriteLock
  op$dbenv$transaction = !internal
}

# commit or roll back transaction.
endTransactionInternal = function(op, commit = TRUE, internal = TRUE) {
  if (internal && op$dbenv$transaction) {
    # inside a user initiated transaction, the user will terminate it.
    return(NULL)
  }

  if (commit) {
    pollDbOperation(op$dbenv, dbCommit(op$dbenv$db))
  } else {
    # not sure whether rollback can ever fail, but why chance it
    pollDbOperation(op$dbenv, dbRollback(op$dbenv$db))
  }

  # writelock goes to FALSE, independently of internal being TRUE or FALSE
  op$dbenv$writelock = FALSE
  # transaction goes to FALSE only if internal is FALSE, but
  # internal == transaction == TRUE exits before we get here.
  op$dbenv$transaction = FALSE
}

# we are not sure the error message given by the database is the same in all
# locales and in all versions of the sqlite library. Therefore we generate the
# message here.
getDbLockedMessage = function() {
  file = tempfile()
  conn1 = dbConnect(SQLite(), file)
  conn2 = dbConnect(SQLite(), file)
  on.exit({
        try(dbDisconnect(conn1))
        try(dbDisconnect(conn2))
        try(file.remove(file))
      })

  dbGetQuery(conn1, "begin immediate transaction")
  err = try(dbGetQuery(conn2, "begin immediate transaction"), silent = TRUE)
  dbRollback(conn1)
  conditionMessage(attr(err, "condition"))
}

# poll `operation` while the db is locked. Will only catch 'db locked' errors,
# other errors will be thrown.
pollDbOperation = function(dbenv, operation) {
  waitInterval = 0.1
  repeat {
    success = tryCatch({
          result = eval.parent(substitute(operation))
          TRUE
        }, error = function(e) {
          if (conditionMessage(e) != op$dbenv$lockMessage) {
            stop(e)
          }
          FALSE
        })
    if (success) {
      return(result)
    }
    Sys.sleep(waitInterval)
    # if we increase the wait interval by a factor of x, the total wait time
    # will be more than the minimal (optimal) wait time by a factor of (x-1)
    # asymptotically.
    # Here we chose to wait no more than 10% more than the minimally required
    # waiting time. 
    waitInterval = min(waitInterval * 1.1, 2)
    # maximum wait interval 2 seconds
  }
}






