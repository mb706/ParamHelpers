#' @rdname OptPath
#' @aliases OptPathDF
#' @export
makeOptPathDF = function(par.set, y.names, minimize, add.transformed.x = FALSE,
  include.error.message = FALSE, include.exec.time = FALSE, include.extra = FALSE) {

  n.y = length(y.names)
  obj = makeOptPath(par.set, y.names, minimize, add.transformed.x, include.error.message,
    include.exec.time, include.extra)

  ee = new.env()
  ee$dob = ee$eol = integer(0)
  
  # potentially init error.message and exec.time in env
  ee$error.message = if (include.error.message) character(0L) else NULL
  ee$exec.time = if (include.exec.time) numeric(0L) else NULL
  ee$extra = if (include.extra) list() else NULL
  
  obj$env = ee

  obj$env$path = makeDataFrame(nrow = 0, ncol = getParamNr(par.set, devectorize = TRUE) + n.y,
    col.types = c(getParamTypes(par.set, df.cols = TRUE, df.discretes.as.factor = FALSE), rep("numeric", n.y)),
    col.names = c(getParamIds(par.set, repeated = TRUE, with.nr = TRUE), y.names)
  )
  return(addClasses(obj, "OptPathDF"))
}


