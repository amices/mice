#'Multivariate Imputation by Chained Equations (Iteration Step)
#' 
#'Takes a \code{mids} object, and produces a new object of class \code{mids}.
#'
#'This function enables the user to split up the computations of the Gibbs
#'sampler into smaller parts. This is useful for the following reasons:
#'\itemize{ \item RAM memory may become easily exhausted if the number of
#'iterations is large. Returning to prompt/session level may alleviate these
#'problems.  \item The user can compute customized convergence statistics at
#'specific points, e.g. after each iteration, for monitoring convergence.  -
#'For computing a 'few extra iterations'.  } Note: The imputation model itself
#'is specified in the \code{mice()} function and cannot be changed with
#'\code{mice.mids}.  The state of the random generator is saved with the
#'\code{mids} object.
#'
#'@param obj An object of class \code{mids}, typically produces by a previous
#'call to \code{mice()} or \code{mice.mids()}
#'@param maxit The number of additional Gibbs sampling iterations.
#'@param printFlag A Boolean flag. If \code{TRUE}, diagnostic information
#'during the Gibbs sampling iterations will be written to the command window.
#'The default is \code{TRUE}.
#'@param ... Named arguments that are passed down to the univariate imputation
#'functions.
#'@author Stef van Buuren, Karin Groothuis-Oudshoorn, 2000
#'@seealso \code{\link{complete}}, \code{\link{mice}}, \code{\link{set.seed}}, 
#'\code{\link[=mids-class]{mids}}
#'@references Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}:
#'Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#'Statistical Software}, \bold{45}(3), 1-67.
#'\url{https://www.jstatsoft.org/v45/i03/}
#'@keywords iteration
#'@examples
#'
#'imp1 <- mice(nhanes, maxit=1, seed = 123)
#'imp2 <- mice.mids(imp1)
#'
#'# yields the same result as
#'imp <- mice(nhanes, maxit=2, seed = 123)
#'
#'# verification
#'identical(imp$imp, imp2$imp)
#'# 
#'@export
mice.mids <- function(obj, maxit = 1, printFlag = TRUE, ...) {
  if (!is.mids(obj)) 
    stop("Object should be of type mids.")
  if (maxit < 1) 
    return(obj)
  
  loggedEvents <- obj$loggedEvents
  state <- list(it = 0, im = 0, co = 0, dep = "", meth = "", 
                log = !is.null(loggedEvents))
  if (is.null(loggedEvents)) 
    loggedEvents <- data.frame(it = 0, im = 0, co = 0, dep = "", 
                               meth = "", out = "")
  
  # Initialize local variables
  call <- match.call()
  imp <- obj$imp
  where <- obj$where
  if (is.null(where)) where <- is.na(obj$data)
  blocks <- obj$blocks
  if (is.null(blocks)) blocks <- make.blocks(obj$data)
  
  assign(".Random.seed", obj$lastSeedValue, pos = 1)
  
  ## OK. Iterate.
  sumIt <- obj$iteration + maxit
  from <- obj$iteration + 1
  to <- from + maxit - 1
  q <- sampler(obj$data, obj$m, where, imp, blocks, obj$method, 
               obj$visitSequence, obj$predictorMatrix, 
               obj$formulas, obj$blots, obj$post, c(from, to), printFlag, ...)
  
  imp <- q$imp
  
  ## combine with previous chainMean and chainVar
  vnames <- unique(unlist(obj$blocks))
  nvis <- length(vnames)
  if (!is.null(obj$chainMean)) {
    chainMean <- chainVar <- array(0, dim = c(nvis, to, obj$m), 
                                   dimnames = list(vnames, 
                                                   seq_len(to), paste("Chain", seq_len(obj$m))))
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
  
  if (!state$log) 
    loggedEvents <- NULL
  if (state$log) 
    row.names(loggedEvents) <- seq_len(nrow(loggedEvents))
  
  ## save, and return
  midsobj <- list(data = obj$data, imp = imp, m = obj$m,
                  where = where, blocks = obj$blocks, 
                  call = call, nmis = obj$nmis, 
                  method = obj$method,
                  predictorMatrix = obj$predictorMatrix,
                  visitSequence = obj$visitSequence,
                  formulas = obj$formulas, post = obj$post,
                  blots = obj$blots,
                  seed = obj$seed, 
                  iteration = sumIt,
                  lastSeedValue = .Random.seed, 
                  chainMean = chainMean,
                  chainVar = chainVar, 
                  loggedEvents = loggedEvents,
                  version = packageVersion("mice"),
                  date = Sys.Date())
  oldClass(midsobj) <- "mids"
  return(midsobj)
}
