#' @importFrom dplyr filter
#' @export
dplyr::filter

#' Subset rows of a \code{mids} object
#'
#' This function takes a \code{mids} object and returns a new
#' \code{mids} object that pertains to the subset of the data
#' identified by the expression in \dots. The expression may use
#' column values from the incomplete data in \code{.data$data}.
#'
#' @param .data A \code{mids} object.
#' @param ... Expressions that return a
#'   logical value, and are defined in terms of the variables in \code{.data$data}.
#'   If multiple expressions are specified, they are combined with the \code{&} operator.
#'   Only rows for which all conditions evaluate to \code{TRUE} are kept.
#' @inheritParams dplyr::filter
#' @seealso \code{\link[dplyr]{filter}}
#' @return An S3 object of class \code{mids}
#' @note The function calculates a logical vector \code{include} of length \code{nrow(.data$data)}.
#' The function constructs the elements of the filtered \code{mids} object as follows:
#' \tabular{ll}{
#' \code{data}     \tab Select rows in \code{.data$data} for which \code{include == TRUE}\cr
#' \code{imp}      \tab Select rows each imputation \code{data.frame} in \code{.data$imp} for which \code{include == TRUE}\cr
#' \code{m}        \tab Equals \code{.data$m}\cr
#' \code{where}    \tab Select rows in \code{.data$where} for which \code{include == TRUE}\cr
#' \code{blocks}   \tab Equals \code{.data$blocks}\cr
#' \code{call}     \tab Equals \code{.data$call}\cr
#' \code{nmis}     \tab Recalculate \code{nmis} based on the selected \code{data} rows\cr
#' \code{method}   \tab Equals \code{.data$method}\cr
#' \code{predictorMatrix} \tab Equals \code{.data$predictorMatrix}\cr
#' \code{visitSequence}   \tab Equals \code{.data$visitSequence}\cr
#' \code{formulas}  \tab Equals \code{.data$formulas}\cr
#' \code{post}      \tab Equals \code{.data$post}\cr
#' \code{blots}     \tab Equals \code{.data$blots}\cr
#' \code{ignore}    \tab Select positions in \code{.data$ignore} for which \code{include == TRUE}\cr
#' \code{seed}            \tab Equals \code{.data$seed}\cr
#' \code{iteration}       \tab Equals \code{.data$iteration}\cr
#' \code{lastSeedValue}   \tab Equals \code{.data$lastSeedValue}\cr
#' \code{chainMean}       \tab Set to \code{NULL}\cr
#' \code{chainVar}        \tab Set to \code{NULL}\cr
#' \code{loggedEvents}    \tab Equals \code{.data$loggedEvents}\cr
#' \code{version}    \tab Replaced with current version\cr
#' \code{date}       \tab Replaced with current date
#' }
#' @author Patrick Rockenschaub
#' @keywords manip
#' @examples
#' imp <- mice(nhanes, m = 2, maxit = 1, print = FALSE)
#'
#' # example with external logical vector
#' imp_f <- filter(imp, c(rep(TRUE, 13), rep(FALSE, 12)))
#'
#' nrow(complete(imp))
#' nrow(complete(imp_f))
#'
#' # example with calculated include vector
#' imp_f2 <- filter(imp, age >= 2 & hyp == 1)
#' nrow(complete(imp_f2)) # should be 5
#' @export
filter.mids <- function(.data, ..., .preserve = FALSE) {
  if (!is.mids(.data)) {
    stop("Argument `.data` should be of class mids.")
  }

  rows <- .data$data %>%
    mutate(.rownumber = row_number()) %>%
    filter(...) %>%
    pull(".rownumber")
  include <- 1L:nrow(.data$data) %in% rows

  # Components that stay the same after filtering
  m <- .data$m
  call <- .data$call
  blocks <- .data$blocks
  method <- .data$method
  predictorMatrix <- .data$predictorMatrix
  visitSequence <- .data$visitSequence
  formulas <- .data$formulas
  blots <- .data$blots
  post <- .data$post
  seed <- .data$seed
  iteration <- .data$iteration
  lastSeedValue <- .data$lastSeedValue
  loggedEvents <- .data$loggedEvents

  # Components that need to be subset
  data <- .data$data[include, ]
  ignore <- .data$ignore[include]
  where <- .data$where[include, ]

  imp <- vector("list", length(.data$imp))
  names(imp) <- names(.data$imp)
  for (i in names(.data$imp)) {
    wy <- .data$where[, i]
    iy <- .data$where[, i] & include
    impi <- .data$imp[[i]][iy[wy], , drop = FALSE]
    if (!is.null(impi)) imp[[i]] <- impi
  }

  # Components that need to be recalculated/reset
  nmis <- colSums(is.na(data))
  chainMean <- NULL
  chainVar <- NULL

  # Create subset mids object
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
