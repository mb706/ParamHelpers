#' @rdname as.data.frame.OptPathDF
#' @export
as.data.frame.OptPathDB = function(x, row.names = NULL, optional = FALSE,
      include.x = TRUE, include.y = TRUE, include.rest = TRUE, dob, eol, ...) {
  
  assertFlag(include.x)
  assertFlag(include.y)
  assertFlag(include.rest)
  
  ensureDbConnected(db)
  beginTransactionInternal(op)
  on.exit(endTransactionInternal(op))
  
  if (!include.x && !include.y && !include.rest)
    stopf("Not able to create data.frame from opt.path. You need to include something!")
  
  ind = getOPDBDobAndEolIndex(x, dob, eol)
  if (!any(ind))
    stopf("No elements where selected (via 'dob' and 'eol')!")
  
  res = makeDataFrame(nrow = sum(ind), ncol = 0)
  
  fullFrame = dbGetQuery(x$dbenv$db, "select * from optpath")
  colnames(fullFrame) = unescapeRowid(colnames(fullFrame))
  nonXYNames = c("dob", "eol",
      if (x$options$include.error.message) "error.message",
      if (x$options$include.exec.time) "exec.time",
      if (x$options$include.extra) "extra")

  xNames = setdiff(colnames(fullFrame), c(op$y.names, nonXYNames))
  yNames = op$y.names
  
  if (include.x || include.y) {
    cols = c(if (include.x) xNames, if (include.y) yNames)
    res = cbind(res, fullFrame[ind, cols, drop = FALSE])
    res = convertDataFrameCols(res, chars.as.factor = TRUE)
  }
  if (include.rest) {
    res = cbind(res, fullFrame[ind, setdiff(nonXYNames, "extra"), drop = FALSE])
    if (x$options$include.extra) {
      exlist = lapply(fullFrame[ind, "extra"], unserialize)
      res = cbind(res, convertListOfRowsToDataFrame(exlist))
    }
  }
  if (!is.null(row.names)) {
    assertCharacter(row.names, len = nrow(res), any.missing = FALSE)
    rownames(res) = row.names
  }
  return(res)
}



