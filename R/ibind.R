#' Enlarge number of imputations by combining `mids` objects
#'
#' This function combines two `mids` objects `x` and `y` into a
#' single `mids` object, with the objective of increasing the number of
#' imputed data sets. If the number of imputations in `x` and `y` are
#' `m(x)` and `m(y)`, then the combined object will have
#' `m(x)+m(y)` imputations.
#'
#' The two `mids` objects are required to
#' have the same underlying multiple imputation model and should
#' be fitted on the same data.
#'
#' @param x A `mids` object.
#' @param y A `mids` object.
#' @return An S3 object of class `mids`
#' @author Karin Groothuis-Oudshoorn, Stef van Buuren
#' @seealso [`mids()`][mids-class]
#' @keywords manip
#' @examples
#' data(nhanes)
#' imp1 <- mice(nhanes, m = 1, maxit = 2, print = FALSE)
#' imp1$m
#'
#' imp2 <- mice(nhanes, m = 3, maxit = 3, print = FALSE)
#' imp2$m
#'
#' imp12 <- ibind(imp1, imp2)
#' imp12$m
#' plot(imp12)
#' @export
ibind <- function(x, y) {
  call <- match.call()
  call <- c(x$call, call)

  if (!is.mids(y) && !is.mids(x)) {
    stop("Arguments `x` and `y` not of class `mids`")
  }
  if (!identical(is.na(x$data), is.na(y$data))) {
    stop("Differences detected in the missing data pattern")
  }
  if (!identical(x$data[!is.na(x$data)], y$data[!is.na(y$data)])) {
    stop("Differences detected in the observed data")
  }
  if (!identical(x$where, y$where)) {
    stop("Differences detected between `x$where` and `y$where`")
  }
  if (!identical(x$blocks, y$blocks)) {
    stop("Differences detected between `x$blocks` and `y$blocks`")
  }
  if (!identical(x$method, y$method)) {
    stop("Differences detected between `x$method` and `y$method`")
  }
  if (!identical(x$predictorMatrix, y$predictorMatrix)) {
    stop("Differences detected between `x$predictorMatrix` and `y$predictorMatrix`")
  }
  if (!identical(x$visitSequence, y$visitSequence)) {
    stop("Differences detected between `x$visitSequence` and `y$visitSequence`")
  }
  if (!identical(x$post, y$post)) {
    stop("Differences detected between `x$post` and `y$post`")
  }
  if (!identical(x$dots, y$dots)) {
    stop("Differences detected between `x$dots` and `y$dots`")
  }
  visitSequence <- x$visitSequence
  imp <- vector("list", ncol(x$data))
  names(imp) <- names(x$data)
  for (j in names(imp)) {
    imp[[j]] <- cbind(x$imp[[j]], y$imp[[j]])
  }

  m <- (x$m + y$m)
  iteration <- max(x$iteration, y$iteration)

  chainMean <- chainVar <- initialize.chain(names(x$data), iteration, m)
  for (j in seq_len(x$m)) {
    chainMean[, seq_len(x$iteration), j] <- x$chainMean[, , j]
    chainVar[, seq_len(x$iteration), j] <- x$chainVar[, , j]
  }
  for (j in seq_len(y$m)) {
    chainMean[, seq_len(y$iteration), j + x$m] <- y$chainMean[, , j]
    chainVar[, seq_len(y$iteration), j + x$m] <- y$chainVar[, , j]
  }

  midsobj <- list(
    data = x$data, imp = imp, m = m,
    where = x$where, blocks = x$blocks,
    call = call, nmis = x$nmis,
    method = x$method,
    predictorMatrix = x$predictorMatrix,
    visitSequence = visitSequence,
    formulas = x$formulas, post = x$post,
    dots = x$dots,
    seed = x$seed,
    iteration = iteration,
    lastSeedValue = x$lastSeedValue,
    chainMean = chainMean,
    chainVar = chainVar,
    loggedEvents = x$loggedEvents,
    version = packageVersion("mice"),
    date = Sys.Date()
  )
  oldClass(midsobj) <- "mids"
  midsobj
}
