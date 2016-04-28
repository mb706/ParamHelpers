#' @export
addOptPathEl.OptPathDF = function(op, x, y, dob = getOptPathLength(op)+1L, eol = NA_integer_,
  error.message = NA_character_, exec.time = NA_real_, extra = NULL,
  check.feasible = !op$add.transformed.x) {

  dob = asInt(dob, na.ok = TRUE)
  eol = asInt(eol, na.ok = TRUE)

  checkAddOptPath(op, x, y, error.message, exec.time, extra, check.feasible)

  env = op$env

  # add extra
  if (!is.null(extra)) {
    if (length(env$extra) > 0L) {
      if (!all(names(extra) == names(env$extra[[1L]])))
        stopf("Trying to add unknown extra(s): %s!", paste(symdiff(names(extra), names(env$extra[[1L]])), collapse = ","))
    }
    env$extra[[length(env$extra) + 1L]] = extra
  }

  # add x and y
  x = recode(op$par.set, x)
  el = do.call(cbind, lapply(x, function(v) as.data.frame(t(v), stringsAsFactors = FALSE)))
  el = cbind(el, as.data.frame(as.list(y), stringsAsFactors = FALSE))

  colnames(el) = colnames(env$path)
  env$path = rbind(env$path, el)

  # add dob and eol
  k = length(env$dob) + 1
  env$dob[k] = dob
  env$eol[k] = eol

  # potentially add errmsg and time
  if (!is.null(env$error.message))
    env$error.message[k] = error.message
  if (!is.null(env$exec.time))
    env$exec.time[k] = exec.time

  return(invisible(NULL))
}

#' @export
setOptPathElDOB.OptPathDF = function(op, index, dob) {
  index = asInteger(index)
  dob = asInteger(dob)
  op$env$dob[index] = dob
  return(invisible(NULL))
}

#' @export
setOptPathElEOL.OptPathDF = function(op, index, eol) {
  index = asInteger(index)
  eol = asInteger(eol)
  op$env$eol[index] = eol
  return(invisible(NULL))
}



