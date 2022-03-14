#' Combine \code{mids} objects by rows
#'
#' This function combines two \code{mids} objects rowwise into a single
#' \code{mids} object, or combines a \code{mids} object with a vector, matrix,
#' factor or dataframe rowwise into a \code{mids} object.
#'
#' If \code{y} is a
#' \code{mids} object, then \code{rbind} requires that the number of
#' multiple imputations in \code{x} and \code{y} is identical. Also,
#' columns of \code{x$data} and \code{y$data} should match.
#'
#' If \code{y} is not a \code{mids} object, the columns of \code{x$data}
#' and \code{y} should match. The \code{where} matrix for \code{y} is set
#' to \code{FALSE}, signaling that any missing values
#' in \code{y} were not imputed. The \code{ignore} vector for \code{y} is
#' set to \code{FALSE}, elements of \code{y} will therefore influence
#' the parameters of the imputation model in future iterations.
#'
#' @param x A \code{mids} object.
#' @param y A \code{mids} object, or a \code{data.frame}, \code{matrix}, \code{factor}
#' or \code{vector}.
#' @param \dots Additional \code{data.frame}, \code{matrix}, \code{vector} or \code{factor}.
#' These can be given as named arguments.
#' @return An S3 object of class \code{mids}
#' @note The function construct the elements of the new \code{mids} object as follows:
#' \tabular{ll}{
#' \code{data}     \tab Rowwise combination of the (incomplete) data in \code{x} and \code{y}\cr
#' \code{imp}      \tab Equals \code{rbind(x$imp[[j]], y$imp[[j]])} if \code{y} is \code{mids} object; otherwise
#' the data of \code{y} will be copied\cr
#' \code{m}        \tab Equals \code{x$m}\cr
#' \code{where}    \tab Rowwise combination of \code{where} arguments\cr
#' \code{blocks}   \tab Equals \code{x$blocks}\cr
#' \code{call}     \tab Vector, \code{call[1]} creates \code{x}, \code{call[2]} is call to \code{rbind.mids}\cr
#' \code{nmis}     \tab \code{x$nmis} + \code{y$nmis}\cr
#' \code{method}   \tab Taken from \code{x$method}\cr
#' \code{predictorMatrix} \tab Taken from \code{x$predictorMatrix}\cr
#' \code{visitSequence}   \tab Taken from \code{x$visitSequence}\cr
#' \code{formulas}  \tab Taken from \code{x$formulas}\cr
#' \code{post}      \tab Taken from \code{x$post}\cr
#' \code{blots}     \tab Taken from \code{x$blots}\cr
#' \code{ignore}    \tab Concatenate \code{x$ignore} and \code{y$ignore}\cr
#' \code{seed}            \tab Taken from \code{x$seed}\cr
#' \code{iteration}       \tab Taken from \code{x$iteration}\cr
#' \code{lastSeedValue}   \tab Taken from \code{x$lastSeedValue}\cr
#' \code{chainMean}       \tab Set to \code{NA}\cr
#' \code{chainVar}        \tab Set to \code{NA}\cr
#' \code{loggedEvents}    \tab Taken from \code{x$loggedEvents}\cr
#' \code{version}    \tab Taken from \code{x$version}\cr
#' \code{date}       \tab Taken from \code{x$date}
#' }
#' @author Karin Groothuis-Oudshoorn, Stef van Buuren
#' @seealso \code{\link{cbind.mids}}, \code{\link{ibind}}, \code{\link[=mids-class]{mids}}
#' @references van Buuren S and Groothuis-Oudshoorn K (2011). \code{mice}:
#' Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#' Statistical Software}, \bold{45}(3), 1-67.
#' \doi{10.18637/jss.v045.i03}
#' @keywords manip
#' @examples
#' imp1 <- mice(nhanes[1:13, ], m = 2, maxit = 1, print = FALSE)
#' imp5 <- mice(nhanes[1:13, ], m = 2, maxit = 2, print = FALSE)
#' mylist <- list(age = NA, bmi = NA, hyp = NA, chl = NA)
#'
#' nrow(complete(rbind(imp1, imp5)))
#' nrow(complete(rbind(imp1, mylist)))
#'
#' nrow(complete(rbind(imp1, data.frame(mylist))))
#' nrow(complete(rbind(imp1, complete(imp5))))
rbind.mids <- function(x, y = NULL, ...) {
  call <- match.call()
  if (is.mids(y)) {
    return(rbind.mids.mids(x, y, call = call))
  }

  # Combine y and dots into data.frame
  if (is.null(y)) {
    y <- rbind.data.frame(...)
  }
  else {
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
  blots <- x$blots
  predictorMatrix <- x$predictorMatrix
  visitSequence <- x$visitSequence

  # Only x contributes imputations
  imp <- x$imp

  # seed, lastSeedvalue, number of iterations, chainMean and chainVar is taken as in mids object x.
  seed <- x$seed
  lastSeedValue <- x$lastSeedValue
  iteration <- x$iteration
  chainMean <- x$chainMean
  chainVar <- x$chainVar
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

  # Call is a vector, with first argument the mice statement and second argument the call to cbind.mids.
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

  # seed, lastSeedvalue, number of iterations
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
