
filter.mids <- function(obj, include = NULL){
  if (!is.mids(obj)) {
    stop("Object should be of type mids.")
  }
  
  if (is.null(include)) {
    include <- rep(TRUE, nrow(obj$data))
  } else if (!is.vector(include, mode = "logical")) {
    stop("Argument 'include' must be logical vector.")
  } else if (length(include) != nrow(obj$data)) {
    stop("Argument 'include' must be of same length as data.")
  }
  
  # Components that stay the same after filtering
  m <- obj$m
  call <- obj$call
  blocks <- obj$blocks
  method <- obj$method
  predictorMatrix <- obj$predictorMatrix
  visitSequence <- obj$visitSequence
  formulas <- obj$formulas
  blots <- obj$blots
  post <- obj$post
  seed <- obj$seed
  iteration <- obj$iteration
  lastSeedValue <- obj$lastSeedValue
  loggedEvents <- obj$loggedEvents
  
  # Components that need to be subset
  data <- obj$data[include, ]
  ignore <- obj$ignore[include]
  where <- obj$where[include, ]
  
  imp <- vector("list", length(obj$imp))
  names(imp) <- names(obj$imp)
  for (i in names(obj$imp)) {
    wy <- obj$where[, i]
    iy <- obj$where[, i] & include
    
    imp[[i]] <- obj$imp[[i]][iy[wy], ]
  }
  
  # Components that need to be recalculated
  nmis <- colSums(is.na(data))
  chainMean <- NULL
  chainVar <- NULL
  
  midsobj <- list(
    data = data, imp = imp, m = m,
    where = where, blocks = blocks,
    call = call, nmis = nmis,
    method = method,
    predictorMatrix = predictorMatrix,
    visitSequence = visitSequence,
    formulas = formulas,
    post = post,
    blots = blots,
    ignore = ignore,
    seed = seed,
    iteration = iteration,
    lastSeedValue = lastSeedValue,
    chainMean = chainMean,
    chainVar = chainVar,
    loggedEvents = loggedEvents,
    version = packageVersion("mice"),
    date = Sys.Date()
  )
  oldClass(midsobj) <- "mids"
  midsobj
}
