#' Multivariate Imputation by Chained Equations (Iteration Step)
#'
#' Takes a \code{mids} object, performs \code{maxit} iterations and
#' produces a new object of class \code{"mids"}.
#'
#' @details
#' This function enables the user to split up the computations of the Gibbs
#' sampler into smaller parts. This is useful for the following reasons:
#'
#' \itemize{
#'    \item To add a few extra iteration to an existing solution.
#'    \item If RAM memory is exhausted. Returning to prompt/session
#'    level may alleviate such problems.
#'    \item To customize convergence statistics at specific points, e.g.,
#'    after every \code{maxit} iterations to monitor convergence.
#' }
#'
#' The imputation model itself is specified in the \code{\link{mice}()} function
#' and cannot be changed in \code{mice.mids()}. The state of the random
#' generator is saved with the \code{mids} object. This ensures that the
#' imputations are reproducible.
#'
#' @param obj An object of class \code{mids}, typically produces by a previous
#' call to \code{mice()} or \code{mice.mids()}
#' @param newdata An optional \code{data.frame} for which multiple imputations
#' are generated according to the model in \code{obj}.
#' @param maxit The number of additional Gibbs sampling iterations. The
#' default is 1.
#' @param printFlag A Boolean flag. If \code{TRUE}, diagnostic information
#' during the Gibbs sampling iterations will be written to the command window.
#' The default is \code{TRUE}.
#' @param \dots Named arguments that are passed down to the univariate imputation
#' functions.
#' @return \code{mice.mids} returns an object of class \code{"mids"}.
#' @seealso \code{\link{complete}}, \code{\link{mice}}, \code{\link{set.seed}},
#' \code{\link{mids}}
#' @aliases mice.mids
#' @keywords iteration
#' @examples
#' imp1 <- mice(nhanes, maxit = 1, seed = 123)
#' imp2 <- mice.mids(imp1)
#'
#' # yields the same result as
#' imp <- mice(nhanes, maxit = 2, seed = 123)
#'
#' # verification
#' identical(imp$imp, imp2$imp)
#' #
#' @export
mice.mids <- function(obj, newdata = NULL, maxit = 1, printFlag = TRUE, ...) {
  if (!is.mids(obj)) {
    stop("Object should be of type mids.")
  }

  # Set seed to last seed after previous imputation
  assign(".Random.seed", obj$lastSeedValue, pos = 1)

  # obj contains training data, newdata contains test data
  # overwrite obj with combined obj + imp.newdata
  if (!is.null(newdata)) {
    ignore <- rep(FALSE, nrow(obj$data))
    if (!is.null(obj$ignore)) ignore <- obj$ignore

    newdata <- check.newdata(newdata, obj$data)
    imp.newdata <- mice(newdata,
      m = obj$m, maxit = 0,
      remove.collinear = FALSE,
      remove.constant = FALSE
    )
    obj <- withCallingHandlers(
      rbind.mids(obj, imp.newdata),
      warning = function(w) {
        if (grepl("iterations differ", w$message)) {
          # Catch warnings concerning iterations, these differ by design
          invokeRestart("muffleWarning")
        }
      }
    )

    # ignore newdata for model building, but do impute
    obj$ignore <- c(ignore, rep(TRUE, nrow(newdata)))
  }

  if (maxit < 1) {
    return(obj)
  }

  loggedEvents <- obj$loggedEvents
  state <- list(
    it = 0, im = 0, co = 0, dep = "", meth = "",
    log = !is.null(loggedEvents)
  )
  if (is.null(loggedEvents)) {
    loggedEvents <- data.frame(
      it = 0, im = 0, co = 0, dep = "",
      meth = "", out = ""
    )
  }

  # Initialize local variables
  call <- match.call()
  imp <- obj$imp
  where <- obj$where
  if (is.null(where)) where <- is.na(obj$data)
  blocks <- obj$blocks
  if (is.null(blocks)) blocks <- make.blocks(obj$data)

  ## OK. Iterate.
  sumIt <- obj$iteration + maxit
  from <- obj$iteration + 1
  to <- from + maxit - 1
  q <- sampler(
    obj$data, obj$m, obj$ignore, where, imp, blocks,
    obj$method, obj$visitSequence, obj$predictorMatrix,
    obj$formulas, obj$calltype, obj$blots, obj$post,
    c(from, to), printFlag, ...
  )

  imp <- q$imp

  ## combine with previous chainMean and chainVar
  vnames <- unique(unlist(obj$blocks))
  nvis <- length(vnames)
  if (!is.null(obj$chainMean)) {
    chainMean <- chainVar <- array(0,
      dim = c(nvis, to, obj$m),
      dimnames = list(
        vnames,
        seq_len(to), paste("Chain", seq_len(obj$m))
      )
    )
    for (j in seq_len(nvis)) {
      if (obj$iteration == 0) {
        chainMean[j, , ] <- q$chainMean[j, , ]
        chainVar[j, , ] <- q$chainVar[j, , ]
      } else {
        chainMean[j, seq_len(obj$iteration), ] <- obj$chainMean[j, , ]
        chainVar[j, seq_len(obj$iteration), ] <- obj$chainVar[j, , ]
        chainMean[j, from:to, ] <- q$chainMean[j, , ]
        chainVar[j, from:to, ] <- q$chainVar[j, , ]
      }
    }
  } else {
    chainMean <- chainVar <- NULL
  }

  if (!state$log) {
    loggedEvents <- NULL
  }
  if (state$log) {
    row.names(loggedEvents) <- seq_len(nrow(loggedEvents))
  }

  ## save, and return
  midsobj <- mids(
    data = obj$data,
    imp = imp,
    m = obj$m,
    where = where,
    blocks = obj$blocks,
    call = call,
    nmis = obj$nmis,
    method = obj$method,
    predictorMatrix = obj$predictorMatrix,
    visitSequence = obj$visitSequence,
    formulas = obj$formulas,
    calltype = obj$calltype,
    post = obj$post,
    blots = obj$blots,
    ignore = obj$ignore,
    seed = obj$seed,
    iteration = sumIt,
    lastSeedValue = get(".Random.seed",
      envir = globalenv(), mode = "integer",
      inherits = FALSE),
    chainMean = chainMean,
    chainVar = chainVar,
    loggedEvents = loggedEvents)

  if (!is.null(newdata)) {
    include <- c(
      rep(FALSE, nrow(midsobj$data) - nrow(newdata)),
      rep(TRUE, nrow(newdata))
    )
    midsobj <- filter(midsobj, include)
  }

  return(midsobj)
}
