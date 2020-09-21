#' Filter data and imputations contained in a \code{mids} object
#'
#' This function takes a \code{mids} object and returns a new
#' \code{mids} object that pertains to the subset of the data
#' identified by \code{include}.
#'
#' @param obj A \code{mids} object.
#' @param include A logical \code{vector} of same length as \code{nrow(obj$data)}.
#' @return An S3 object of class \code{mids}
#' @note The function construct the elements of the new \code{mids} object as follows:
#' \tabular{ll}{
#' \code{data}     \tab Select rows in \code{obj$data} for which \code{include == TRUE}\cr
#' \code{imp}      \tab Select rows each imputation \code{data.frame} in \code{obj$imp} for which \code{include == TRUE}\cr
#' \code{m}        \tab Equals \code{obj$m}\cr
#' \code{where}    \tab Select rows in \code{obj$where} for which \code{include == TRUE}\cr
#' \code{blocks}   \tab Equals \code{obj$blocks}\cr
#' \code{call}     \tab Equals \code{obj$call}\cr
#' \code{nmis}     \tab Recalculate \code{nmis} based on the selected \code{data} rows\cr
#' \code{method}   \tab Equals \code{obj$method}\cr
#' \code{predictorMatrix} \tab Equals \code{obj$predictorMatrix}\cr
#' \code{visitSequence}   \tab Equals \code{obj$visitSequence}\cr
#' \code{formulas}  \tab Equals \code{obj$formulas}\cr
#' \code{post}      \tab Equals \code{obj$post}\cr
#' \code{blots}     \tab Equals \code{obj$blots}\cr
#' \code{ignore}    \tab Select positions in \code{obj$ignore} for which \code{include == TRUE}\cr
#' \code{seed}            \tab Equals \code{obj$seed}\cr
#' \code{iteration}       \tab Equals \code{obj$iteration}\cr
#' \code{lastSeedValue}   \tab Equals \code{obj$lastSeedValue}\cr
#' \code{chainMean}       \tab Set to \code{NULL}\cr
#' \code{chainVar}        \tab Set to \code{NULL}\cr
#' \code{loggedEvents}    \tab Equals \code{obj$loggedEvents}\cr
#' \code{version}    \tab Replaced with current version\cr
#' \code{date}       \tab Replaced with current date
#' }
#' @author Patrick Rockenschaub
#' @keywords manip
#' @examples
#' imp <- mice(nhanes, m = 2, maxit = 1, print = FALSE)
#' imp_f <- filter.mids(imp, include = c(rep(TRUE, 13), rep(FALSE, 12)))
#'
#' nrow(complete(imp))
#' nrow(complete(imp_f))
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
  
  # Components that need to be recalculated/reset
  nmis <- colSums(is.na(data))
  chainMean <- NULL
  chainVar <- NULL
  
  # Create subset mids object
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
