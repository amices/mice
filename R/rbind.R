rbind.mids <- function(x, y = NULL, ...) {
  call <- match.call()
  if (is.mids(y)) {
    return(rbind.mids.mids(x, y, call = call))
  }

  # The code below is designed for the following cases:
  # x is a mids object
  # y is a vector, matrix, factor, or data.frame to be row-appended to the
  # mids object x. The dots are additional vectors, matrices, factors, or
  # data.frames.
  #
  # Appending a vector/matrix/factor/data.frame leaves the y-values untouched
  #
  # Appending a mids object (by rbind.mids.mids) copies that latest
  # imputations into imp structure. The impute a dataset from an existing
  # model, first convert the newdata to a mids object (see mice.mids).

  # Rowbind all new information into data.frame y by base::rbind.data.frame
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

  # Call is a vector, with first argument the mice statement and
  # second argument the call to cbind.mids.
  call <- c(x$call, call)

  # Rowbind data in x (x$data) and y
  data <- rbind(x$data, y)

  # Inherits block structure from x
  blocks <- x$blocks

  # where argument: leave y untouched (do not impute y)
  # See https://github.com/amices/mice/issues/59
  wy <- matrix(FALSE, nrow = nrow(y), ncol = ncol(y))
  where <- rbind(x$where, wy)

  # ignore: new y values are ignored by the imputation model
  ignore <- c(x$ignore, rep(TRUE, nrow(y)))

  # The collection of imputations in the new mids object is restricted to
  # entries from x.
  m <- x$m

  # recalculate number of missing values
  nmis <- x$nmis + colSums(is.na(y))

  # The listelements method, post, predictorMatrix, visitSequence are
  # copies from x.
  method <- x$method
  post <- x$post
  formulas <- x$formulas
  modeltype <- x$modeltype
  blots <- x$blots
  predictorMatrix <- x$predictorMatrix
  visitSequence <- x$visitSequence

  # Only x contributes to imputations if ignore for y is TRUE
  # Set the proper row_id for the y data
  # imp <- initialize.imp(data, m, ignore, where, blocks,
  #                       visitSequence, method, nmis, data.init = NULL,
  #                       leave.empty = TRUE)
  # for (j in names(imp)) {
  #   rows <- seq_len(nrow(x$imp[[j]]))
  #   for (i in seq_len(m)) {
  #     set(imp[[j]], i = rows, j = i, value = x$imp[[j]][[i]])
  #   }
  # }
  imp <- x$imp

  # seed, lastSeedValue, number of iterations, chainMean and chainVar
  # is taken as in mids object x.
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
    modeltype = modeltype,
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
  modeltype <- x$modeltype
  blots <- x$blots
  ignore <- c(x$ignore, y$ignore)
  predictorMatrix <- x$predictorMatrix
  visitSequence <- x$visitSequence

  # The original data of y will be binded into the multiple imputed dataset
  # including the imputed values of y.
  # Only x contributes to imputations if ignore for y is TRUE
  # Set the proper row_id for the y data
  imp <- initialize.imp(data, m, ignore, where, blocks,
                        visitSequence, method, nmis, data.init = NULL,
                        leave.empty = TRUE)
  for (j in names(imp)) {
    idx <- seq_len(nrow(imp[[j]]))
    ids <- c(x[["imp"]][[j]][["row_id"]],
             y[["imp"]][[j]][["row_id"]])
    if (length(idx) != length(ids)) {
      stop("imputation size for ", j, " differs (", length(idx),
           " vs ", length(ids), ")")
    }
    if (length(idx)) {
      for (i in seq_len(m)) {
        xyval <- c(x[["imp"]][[j]][[i]], y[["imp"]][[j]][[i]])
        xyidx <- c(x[["imp"]][[j]][["row_id"]], y[["imp"]][[j]][["row_id"]])
        val <- xyval[xyidx %in% ids]
        set(imp[[j]], i = idx, j = i, value = val)
      }
    }
  }

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
    modeltype = modeltype,
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
