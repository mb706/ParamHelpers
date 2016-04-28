#' @export
addOptPathEl.OptPathDB = function(op, x, y, dob, eol = NA_integer_,
      error.message = NA_character_, exec.time = NA_real_, extra = NULL,
      check.feasible = !op$add.transformed.x) {

  ensureDbConnected(op)
  
  beginTransactionInternal(op, needWriteLock = TRUE)
  # if we exit due to error, roll back.
  on.exit(endTransactionInternal(op, commit = FALSE))
  if (missing(dob)) {
    dob = getOptPathLength(op) + 1L
  }

  checkAddOptPath(op, x, y, error.message, exec.time, extra, check.feasible)


  el = buildOptPathDBRow(op, x, y, dob, eol, error.message, exec.time, extra)

  dbGetPreparedQuery(op$dbenv$db, op$insertquery, el)
  # overwrite old on.exit with 'commit'
  on.exit(endTransactionInternal(op, commit = TRUE))
  return(invisible(NULL))
}

#' @export
setOptPathElDOB.OptPathDF = function(op, index, dob) {
  index = asInteger(index)
  dob = asInteger(dob)
  ensureDbConnected(op)
  beginTransactionInternal(op)
  on.exit(endTransactionInternal(op))
  dbGetPreparedQuery(op$dbenv$db, "update optpath set dob=:n where rowid=:row",
      data.frame(n = dob, row = index))
  return(invisible(NULL))
}

#' @export
setOptPathElEOL.OptPathDF = function(op, index, eol) {
  index = asInteger(index)
  eol = asInteger(eol)
  ensureDbConnected(op)
  beginTransactionInternal(op)
  on.exit(endTransactionInternal(op))
  dbGetPreparedQuery(op$dbenv$db, "update optpath set eol=:n where rowid=:row",
      data.frame(n = eol, row = index))
  return(invisible(NULL))
}

#' @export
updateOptPathEl.OptPathDB = function(op, index, x, y, dob, eol, error.message,
    exec.time, extra, check.feasible = !op$add.transformed.x) {

  ensureDbConnected(op)
  beginTransactionInternal(op, needWriteLock = TRUE)
  # if we exit due to error, roll back.
  on.exit(endTransactionInternal(op, commit = FALSE))

  # fetch everything that is not being changed 
  orig = getOptPathEl(op, index)
  for (arg in c("x", "y", "dob", "eol", "extra")) {
    assign(arg, tryCatch(get(arg), error = function(e) orig[[arg]]))
  }
  if (missing(exec.time)) {
    exec.time = NA_real_
  }
  if (missing(error.message)) {
    error.message = NA_character_
  }

  checkAddOptPath(op, x, y, error.message, exec.time, extra, check.feasible)
  
  el = buildOptPathDBRow(op, x, y, dob, eol, error.message, exec.time, extra)

  colnames(el) = escapeRowid(colnames(el))
  cbind(el, rowid = index)
  dbGetPreparedQuery(op$dbenv$db, op$updatequery, el)
  # overwrite old on.exit with 'commit'
  on.exit(endTransactionInternal(op, commit = TRUE))
  return(invisible(NULL))
}

#' Begin transaction
#'
#' By default, OptPathDB is in auto-commit mode.
#' \code{beginTransactionOptPathDB} turns this off;
#' \code{commitTransactionOptPathDB} and \code{rollbackTransactionOptPathDB}
#' turn it on again.
#'
#' @param op [\code{OptPathDF}]\cr
#'   The \code{OptPathDF} that is concerned.
#' @parem needWriteLock [\code{logical(1)]\cr
#'   Indicate whether the function should wait until a transaction is started
#'   that is allowed to write.
#'
#' @export
beginTransactionOptPathDB = function(op, needWriteLock = FALSE) {
  ensureDbConnected(op)
  beginTransactionInternal(op, internal = FALSE, needWriteLock = needWriteLock)
}

#' Commit transaction
#' @rdname beginTransactionOptPathDB
#' @export
commitTransactionOptPathDB = function(op) {
  # no ensureDbConnected(op) since commit would fail anyways.
  endTransactionInternal(op, commit = TRUE, internal = FALSE)
}

#' Roll back transaction
#' @rdname beginTransactionOptPathDB
#' @export
rollbackTransactionOptPathDB = function(op) {
  # no ensureDbConnected(op) since rollback would fail anyways.
  endTransactionInternal(op, commit = FALSE, internal = FALSE)
}

#' Commit and wait for OptPathDB to change
#'
#' Poll the \code{OptPathDB} in a loop until a write access happened to it.
#'
#' @param op [\code{OptPathDB}]\cr
#'   The opt.path to poll.
#' @param startPollFreq [\code{numeric(1)}]\cr
#'   Start polling at this frequency, in Hz.
#' @param pollFactor [\code{numeric(1)}]\cr
#'   Increase polling interval by this factor every round. The total waiting
#'   time will asymptotically be less than \code{pollFactor} times the minimal
#'   waiting time.
#' @param minPollFreq [\code{numeric(1)}]\cr
#'   Minimal polling frequency.
#' @param maxTime [\code{numeric(1)}]\cr
#'   Maximum time to block.
#' @return [\code(logical(1)]
#' \code{TRUE} if a write access to \code{OptPathDB} was recognized,
#' \code{FALSE} if \code{maxTime} passed without a write access happening.
#' @export
commitAndWaitOnOptPathDB = function(op, startPollFreq = 2, pollFactor = 1,
    minPollFreq, maxTime = Inf) {
  assertNumber(startPollFreq, lower = .Machine$double.eps, finite = TRUE)
  assertNumber(pollFactor, lower = .Machine$double.eps, finite = TRUE)
  assertNumber(minPollFreq, lower = .Machine$double.eps, finite = TRUE)
  assertNumber(maxTime, lower = .Machine$double.eps)
  assertClass(op, "OptPathDB")

  waitInterval = 1 / startPollFreq
  startTime = proc.time()[3]
  exiting = FALSE
  beforeCommitCount = sqliteQuickColumn(tmp, "access", "count")
  commitTransactionOptPathDB(op)
  repeat {
    if (sqliteQuickColumn(tmp, "access", "count") != beforeCommitCount) {
      return(TRUE)
    }
    if (exiting) {
      return(FALSE)
    }
    remaining = startTime + maxTime - proc.time()[3]
    exiting = remaining <= waitInterval
    Sys.sleep(min(remaining, waitInterval))
    waitInterval = min(waitInterval * pollFactor, 1 / minPollFreq)
  }
}





