cbind.mids <- function(x, y = NULL, ...) {
  call <- match.call()
  dots <- list(...)
  if (is.mids(y)) {
    return(cbind.mids.mids(x, y, call = call))
  }

  if ((is.null(y) || length(y) == 0L) && length(dots) == 0L) {
    return(x)
  }
  n <- nrow(x$data)
  if (length(y) == 1L) y <- rep(y, n)
  if (length(y) == 0L && length(dots) > 0L) {
    y <- cbind.data.frame(dots)
  } else if (length(y) > 0L && length(dots) == 0L) {
    y <- cbind.data.frame(y)
  } else {
    y <- cbind.data.frame(y, dots)
  }

  # Call is a vector, with first argument the mice statement
  # and second argument the call to cbind.mids.
  call <- c(x$call, call)

  if (nrow(y) != nrow(x$data)) {
    stop(
      "arguments imply differing number of rows: ",
      c(nrow(x$data), ", ", nrow(y))
    )
  }
  data <- cbind(x$data, y)
  varnames <- make.unique(colnames(data))
  colnames(data) <- varnames

  # where argument
  where <- cbind(x$where, matrix(FALSE, nrow = nrow(x$where), ncol = ncol(y)))
  colnames(where) <- varnames

  # blocks: no renaming needed because all block definition will
  # refer to varnames[1:ncol(x$data)] only, and are hence unique
  # but we do need to rename duplicate block names
  yblocks <- vector("list", length = ncol(y))
  blocks <- c(x$blocks, yblocks)
  xynames <- c(names(x$blocks), colnames(y))
  blocknames <- make.unique(xynames)
  names(blocknames) <- xynames
  names(blocks) <- blocknames
  ct <- c(attr(x$blocks, "calltype"), rep("pred", ncol(y)))
  names(ct) <- blocknames
  attr(blocks, "calltype") <- ct

  m <- x$m

  # count the number of missing data in y
  nmis <- c(x$nmis, colSums(is.na(y)))
  names(nmis) <- varnames

  # imp: original data of y will be copied into the multiple imputed dataset,
  # including the missing values of y.
  r <- (!is.na(y))
  f <- function(j) {
    m <- matrix(NA,
                nrow = sum(!r[, j]),
                ncol = x$m,
                dimnames = list(row.names(y)[!r[, j]], seq_len(m))
    )
    as.data.frame(m)
  }
  imp <- lapply(seq_len(ncol(y)), f)
  imp <- c(x$imp, imp)
  names(imp) <- varnames

  # The imputation method for (columns in) y will be set to ''.
  method <- c(x$method, rep.int("", ncol(y)))
  names(method) <- blocknames

  # The variable(s) in y are included in the predictorMatrix.
  # y is not used as predictor as well as not imputed.
  predictorMatrix <- rbind(
    x$predictorMatrix,
    matrix(0,
           ncol = ncol(x$predictorMatrix),
           nrow = ncol(y)
    )
  )
  predictorMatrix <- cbind(
    predictorMatrix,
    matrix(0,
           ncol = ncol(y),
           nrow = nrow(x$predictorMatrix) + ncol(y)
    )
  )
  rownames(predictorMatrix) <- blocknames
  colnames(predictorMatrix) <- varnames

  visitSequence <- x$visitSequence
  formulas <- x$formulas
  post <- c(x$post, rep.int("", ncol(y)))
  names(post) <- varnames
  blots <- x$blots
  ignore <- x$ignore

  # seed, lastSeedValue, number of iterations, chainMean and chainVar
  # is taken as in mids object x.
  seed <- x$seed
  lastSeedValue <- x$lastSeedValue
  iteration <- x$iteration
  chainMean <- x$chainMean
  chainVar <- x$chainVar

  loggedEvents <- x$loggedEvents

  ## save, and return
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


cbind.mids.mids <- function(x, y, call) {
  if (!is.mids(y)) stop("Argument `y` not a mids object")

  if (nrow(y$data) != nrow(x$data)) {
    stop("The two datasets do not have the same length\n")
  }
  if (x$m != y$m) {
    stop("The two mids objects should have the same number of imputations\n")
  }

  # Call is a vector, with first argument the mice statement
  # and second argument the call to cbind.mids.
  call <- c(x$call, call)

  # The data in x$data and y$data are combined together.
  # make variable names unique
  data <- cbind(x$data, y$data)
  xynames <- c(colnames(x$data), colnames(y$data))
  varnames <- make.unique(xynames)
  names(varnames) <- xynames
  names(data) <- varnames

  where <- cbind(x$where, y$where)
  colnames(where) <- varnames

  # rename variable names within each x$blocks and y$blocks
  xnew <- varnames[1:ncol(x$data)]
  ynew <- varnames[-(1:ncol(x$data))]
  xblocks <- x$blocks
  yblocks <- y$blocks
  for (i in names(xblocks)) xblocks[[i]] <- unname(xnew[xblocks[[i]]])
  for (i in names(yblocks)) yblocks[[i]] <- unname(ynew[yblocks[[i]]])
  blocks <- c(xblocks, yblocks)
  xynames <- c(names(xblocks), names(yblocks))
  blocknames <- make.unique(xynames)
  names(blocknames) <- xynames
  names(blocks) <- blocknames
  ct <- c(attr(xblocks, "calltype"), attr(yblocks, "calltype"))
  names(ct) <- blocknames
  attr(blocks, "calltype") <- ct

  m <- x$m
  nmis <- c(x$nmis, y$nmis)
  names(nmis) <- varnames
  imp <- c(x$imp, y$imp)
  names(imp) <- varnames
  method <- c(x$method, y$method)
  names(method) <- blocknames

  # Concatenate formulas, rename variables if needed
  if (all(names(ynew) == unname(ynew)) && all(names(xnew) == unname(xnew))) {
    formulas <- c(x$formulas, y$formulas)
  } else {
    xformulas <- x$formulas
    yformulas <- y$formulas
    for (i in names(xformulas)) {
      xformulas[[i]] <- renf(xformulas[[i]], xnew)
    }
    for (i in names(yformulas)) {
      yformulas[[i]] <- renf(yformulas[[i]], ynew)
    }
    formulas <- c(xformulas, yformulas)
  }
  names(formulas) <- blocknames

  # The predictorMatrices of x and y are combined with zero matrices
  # on the off diagonal blocks.
  predictorMatrix <- rbind(
    x$predictorMatrix,
    matrix(0,
           ncol = ncol(x$predictorMatrix),
           nrow = nrow(y$predictorMatrix)
    )
  )
  predictorMatrix <- cbind(
    predictorMatrix,
    rbind(
      matrix(0,
             ncol = ncol(y$predictorMatrix),
             nrow = nrow(x$predictorMatrix)
      ),
      y$predictorMatrix
    )
  )
  rownames(predictorMatrix) <- blocknames
  colnames(predictorMatrix) <- varnames

  # As visitSequence is taken first the order for x and after that from y.
  # take care that duplicate names need to be renamed
  xnew <- blocknames[1:length(x$blocks)]
  ynew <- blocknames[-(1:length(x$blocks))]
  visitSequence <- unname(c(xnew[x$visitSequence], ynew[y$visitSequence]))
  post <- c(x$post, y$post)
  names(post) <- varnames
  blots <- c(x$blots, y$blots)
  names(blots) <- blocknames
  ignore <- x$ignore

  # For the elements seed, lastSeedValue and iteration the values
  # from midsobject x are copied.
  seed <- x$seed
  lastSeedValue <- x$lastSeedValue
  iteration <- x$iteration

  # the chainMean and chainVar vectors for x and y are combined.
  chainMean <- array(
    data = NA,
    dim = c(dim(x$chainMean)[1] + dim(y$chainMean)[1], iteration, m),
    dimnames = list(
      c(
        dimnames(x$chainMean)[[1]],
        dimnames(y$chainMean)[[1]]
      ),
      dimnames(x$chainMean)[[2]],
      dimnames(x$chainMean)[[3]]
    )
  )
  chainMean[seq_len(dim(x$chainMean)[1]), , ] <- x$chainMean

  if (iteration <= dim(y$chainMean)[2]) {
    chainMean[(dim(x$chainMean)[1] + 1):dim(chainMean)[1], , ] <- y$chainMean[, seq_len(iteration), ]
  } else {
    chainMean[(dim(x$chainMean)[1] + 1):dim(chainMean)[1], seq_len(dim(y$chainMean)[2]), ] <- y$chainMean
  }

  chainVar <- array(
    data = NA, dim = c(dim(x$chainVar)[1] + dim(y$chainVar)[1], iteration, m),
    dimnames = list(
      c(
        dimnames(x$chainVar)[[1]],
        dimnames(y$chainVar)[[1]]
      ),
      dimnames(x$chainVar)[[2]],
      dimnames(x$chainVar)[[3]]
    )
  )
  chainVar[seq_len(dim(x$chainVar)[1]), , ] <- x$chainVar

  if (iteration <= dim(y$chainVar)[2]) {
    chainVar[(dim(x$chainVar)[1] + 1):dim(chainVar)[1], , ] <- y$chainVar[, seq_len(iteration), ]
  } else {
    chainVar[(dim(x$chainVar)[1] + 1):dim(chainVar)[1], seq_len(dim(y$chainVar)[2]), ] <- y$chainVar
  }

  loggedEvents <- x$loggedEvents

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

renf <- function(f, nn) {
  # rename variables in formula f
  z <- as.character(f)
  for (i in seq_along(nn)) {
    z <- gsub(names(nn)[i], unname(nn)[i], z)
  }
  nf <- formula(paste(z[2], z[1], z[3], collapse = " "))
  return(nf)
}
