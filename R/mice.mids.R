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
#' By default, \code{mice.mids()} continues in the same way as the object
#' was created: an object made with \code{parallel = TRUE} continues in
#' parallel, and other objects continue sequentially. In both cases, the
#' result is identical to that of a single call with all iterations. For
#' a parallel object, this works because the object stores the state of
#' the random number stream of each chain (\code{chainSeed}).
#'
#' It is possible to switch: \code{parallel = TRUE} continues a sequential
#' object in parallel, and \code{parallel = FALSE} continues a parallel
#' object sequentially, e.g. to see the iteration history or to debug an
#' imputation method. The result is reproducible, but differs from a
#' single call with all iterations, as the random numbers come from
#' different streams.
#'
#' @param obj An object of class \code{mids}, typically produces by a previous
#' call to \code{mice()} or \code{mice.mids()}
#' @param newdata An optional \code{data.frame} for which multiple imputations
#' are generated according to the model in \code{obj}.
#' @param maxit The number of additional Gibbs sampling iterations. The
#' default is 1.
#' @param print Logical. If \code{TRUE} (default), diagnostic information
#' during the Gibbs sampling iterations is written to the console. See
#' \code{\link{mice}()}.
#' @param printFlag Legacy name of \code{print}, kept for backward
#' compatibility. If given, it takes precedence over \code{print}.
#' @param parallel Logical. If \code{TRUE}, the chains are run in parallel,
#' as in \code{mice(parallel = TRUE)}. The default continues in the mode in
#' which \code{obj} was created. See Details.
#' @param n.core Optional positive integer with the number of workers used
#' when \code{parallel = TRUE}. See \code{\link{mice}()}.
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
#'
#' \dontrun{
#' # the same holds in parallel
#' imp1 <- mice(nhanes, maxit = 1, seed = 123, parallel = TRUE, n.core = 2)
#' imp2 <- mice.mids(imp1, n.core = 2)
#' imp <- mice(nhanes, maxit = 2, seed = 123, parallel = TRUE, n.core = 2)
#' identical(imp$imp, imp2$imp)
#' }
#' #
#' @export
mice.mids <- function(
  obj,
  newdata = NULL,
  maxit = 1,
  print = getOption("mice.print", getOption("mice.printFlag", TRUE)),
  parallel = isTRUE(obj$parallel),
  n.core = NULL,
  ...,
  printFlag = print
) {
  if (!is.mids(obj)) {
    stop("Object should be of type mids.")
  }
  parallel <- isTRUE(parallel)

  # Set seed to last seed after previous imputation
  assign(".Random.seed", obj$lastSeedValue, pos = 1)

  # obj contains training data, newdata contains test data
  # overwrite obj with combined obj + imp.newdata
  if (!is.null(newdata)) {
    ignore <- rep(FALSE, nrow(obj$data))
    if (!is.null(obj$ignore)) {
      ignore <- obj$ignore
    }

    newdata <- check.newdata(newdata, obj$data)
    imp.newdata <- mice(
      newdata,
      m = obj$m,
      method = obj$method,
      maxit = 0,
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
  .mice.state <- list(
    it = 0,
    im = 0,
    co = 0,
    dep = "",
    meth = "",
    log = !is.null(loggedEvents)
  )
  if (is.null(loggedEvents)) {
    loggedEvents <- data.frame(
      it = 0,
      im = 0,
      co = 0,
      dep = "",
      meth = "",
      out = ""
    )
  }

  # Initialize local variables
  call <- match.call()
  imp <- obj$imp
  where <- obj$where
  if (is.null(where)) {
    where <- is.na(obj$data)
  }
  blocks <- obj$blocks
  if (is.null(blocks)) {
    blocks <- make.blocks(obj$data)
  }

  ## OK. Iterate.
  sumIt <- obj$iteration + maxit
  from <- obj$iteration + 1
  to <- from + maxit - 1
  progress <- make.progress(obj$m * maxit)
  # continue the random number streams of parallel chains, if available
  chain.seed <- NULL
  if (parallel) {
    oldplan <- setup.parallel(obj$m, n.core)
    if (!is.null(oldplan)) {
      on.exit(future::plan(oldplan), add = TRUE)
    }
    if (isTRUE(obj$parallel) && length(obj$chainSeed) == obj$m) {
      chain.seed <- obj$chainSeed
    }
  }
  # sampler() looks up .mice.state in its caller, so it is called directly
  sampler.fun <- if (parallel) {
    function(...) sampler.parallel(..., chain.seed = chain.seed)
  } else {
    sampler
  }
  q <- sampler.fun(
    data = obj$data,
    m = obj$m,
    ignore = obj$ignore,
    where = where,
    imp = imp,
    blocks = blocks,
    method = obj$method,
    visitSequence = obj$visitSequence,
    predictorMatrix = obj$predictorMatrix,
    formulas = obj$formulas,
    calltype = obj$calltype,
    blots = obj$blots,
    post = obj$post,
    fromto = c(from, to),
    printFlag = printFlag,
    ...,
    progress = progress
  )

  imp <- q$imp

  ## combine with previous chainMean and chainVar
  vnames <- unique(unlist(obj$blocks))
  nvis <- length(vnames)
  if (!is.null(obj$chainMean)) {
    chainMean <- chainVar <- array(
      0,
      dim = c(nvis, to, obj$m),
      dimnames = list(
        vnames,
        seq_len(to),
        paste("Chain", seq_len(obj$m))
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

  # append events logged by the parallel chains
  if (parallel && !is.null(q$loggedEvents)) {
    if (.mice.state$log) {
      loggedEvents <- rbind(loggedEvents, q$loggedEvents)
    } else {
      loggedEvents <- q$loggedEvents
    }
    .mice.state$log <- TRUE
  }
  if (!.mice.state$log) {
    loggedEvents <- NULL
  }
  if (.mice.state$log) {
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
    lastSeedValue = get(
      ".Random.seed",
      envir = globalenv(),
      mode = "integer",
      inherits = FALSE
    ),
    chainMean = chainMean,
    chainVar = chainVar,
    loggedEvents = loggedEvents,
    parallel = parallel,
    chainSeed = if (parallel) q$chainSeed
  )

  if (!is.null(newdata)) {
    include <- c(
      rep(FALSE, nrow(midsobj$data) - nrow(newdata)),
      rep(TRUE, nrow(newdata))
    )
    midsobj <- filter(midsobj, include)
  }

  return(midsobj)
}
