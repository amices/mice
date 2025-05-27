rbind.mids <- function(x, y = NULL, ...) {
  call <- match.call()
  if (is.mids(y)) {
    return(rbind.mids.mids(x, y, call = call))
  }

  # Combine y and dots into data.frame
  if (is.null(y)) {
    y <- rbind.data.frame(...)
  } else {
    y <- rbind.data.frame(y, ...)
  }

  if (is.data.frame(y)) {
    if (ncol(y) != ncol(x$data)) {
      stop("datasets have different number of columns")
    }
  }

  varnames <- colnames(x$data)

  # Call is a vector, with first argument the mice statement and second argument the call to cbind.mids.
  call <- c(x$call, call)

  # The data in x (x$data) and y are combined together.
  data <- rbind(x$data, y)
  blocks <- x$blocks

  # where argument: code all values as observed, including NA
  wy <- matrix(FALSE, nrow = nrow(y), ncol = ncol(y))
  rownames(wy) <- rownames(y)
  where <- rbind(x$where, wy)

  # ignore argument: include all new values
  ignore <- c(x$ignore, rep(FALSE, nrow(y)))

  # The number of imputations in the new midsobject is equal to that in x.
  m <- x$m

  # count the number of missing data in y and add them to x$nmis.
  nmis <- x$nmis + colSums(is.na(y))

  # The listelements method, post, predictorMatrix, visitSequence will be copied from x.
  method <- x$method
  post <- x$post
  formulas <- x$formulas
  calltype <- x$calltype
  blots <- x$blots
  predictorMatrix <- x$predictorMatrix
  visitSequence <- x$visitSequence

  # Only x contributes imputations
  imp <- x$imp

  # seed, lastSeedValue, number of iterations, chainMean and chainVar is taken as in mids object x.
  seed <- x$seed
  lastSeedValue <- x$lastSeedValue
  iteration <- x$iteration
  chainMean <- x$chainMean
  chainVar <- x$chainVar
  loggedEvents <- x$loggedEvents

  midsobj <- mids(
    data = data,
    imp = imp,
    m = m,
    where = where,
    blocks = blocks,
    call = call,
    nmis = nmis,
    method = method,
    predictorMatrix = predictorMatrix,
    visitSequence = visitSequence,
    formulas = formulas,
    calltype = calltype,
    post = post,
    blots = blots,
    ignore = ignore,
    seed = seed,
    iteration = iteration,
    lastSeedValue = lastSeedValue,
    chainMean = chainMean,
    chainVar = chainVar,
    loggedEvents = loggedEvents)
  return(midsobj)
}

rbind.mids.mids <- function(x, y, call) {
  if (!is.mids(y)) stop("argument `y` not a mids object")

  if (ncol(y$data) != ncol(x$data)) {
    stop("datasets have different number of columns")
  }
  if (!identical(colnames(x$data), colnames(y$data))) {
    stop("datasets have different variable names")
  }
  if (!identical(sapply(x$data, is.factor), sapply(y$data, is.factor))) {
    stop("datasets have different factor variables")
  }
  if (x$m != y$m) {
    stop("number of imputations differ")
  }
  varnames <- colnames(x$data)

  # Call is a vector, with first argument the mice statement and second argument the call to rbind.mids.
  call <- match.call()
  call <- c(x$call, call)

  # The data in x (x$data) and y are combined together.
  data <- rbind(x$data, y$data)

  # Where argument
  where <- rbind(x$where, y$where)

  # The number of imputations in the new midsobject is equal to that in x.
  m <- x$m

  # count the number of missing data in y and add them to x$nmis.
  nmis <- x$nmis + y$nmis

  # The listelements method, post, predictorMatrix, visitSequence will be copied from x.
  blocks <- x$blocks
  method <- x$method
  post <- x$post
  formulas <- x$formulas
  calltype <- x$calltype
  blots <- x$blots
  ignore <- c(x$ignore, y$ignore)
  predictorMatrix <- x$predictorMatrix
  visitSequence <- x$visitSequence

  # The original data of y will be binded into the multiple imputed dataset
  # including the imputed values of y.
  imp <- vector("list", ncol(x$data))
  for (j in seq_len(ncol(x$data))) {
    if (!is.null(x$imp[[j]]) || !is.null(y$imp[[j]])) {
      imp[[j]] <- rbind(x$imp[[j]], y$imp[[j]])
    }
  }
  names(imp) <- varnames

  # seed, lastSeedValue, number of iterations
  seed <- x$seed
  lastSeedValue <- x$lastSeedValue
  iteration <- x$iteration

  if (x$iteration != y$iteration) {
    warning("iterations differ, so no convergence diagnostics calculated",
      call. = FALSE
    )
    chainMean <- NULL
    chainVar <- NULL
  } else {
    w <- colSums(x$where) / colSums(where)
    chainMean <- x$chainMean * w + y$chainMean * (1 - w)
    chainVar <- x$chainVar * w + y$chainVar * (1 - w)
  }

  loggedEvents <- x$loggedEvents
  midsobj <- mids(
    data = data,
    imp = imp,
    m = m,
    where = where,
    blocks = blocks,
    call = call,
    nmis = nmis,
    method = method,
    predictorMatrix = predictorMatrix,
    visitSequence = visitSequence,
    formulas = formulas,
    calltype = calltype,
    post = post,
    blots = blots,
    ignore = ignore,
    seed = seed,
    iteration = iteration,
    lastSeedValue = lastSeedValue,
    chainMean = chainMean,
    chainVar = chainVar,
    loggedEvents = loggedEvents)
  return(midsobj)
}
