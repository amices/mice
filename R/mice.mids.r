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
#'@param diagnostics A Boolean flag. If \code{TRUE}, diagnostic information
#'will be appended to the value of the function. If \code{FALSE}, only the
#'imputed data are saved.  The default is \code{TRUE}.
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
#'\url{http://www.jstatsoft.org/v45/i03/}
#'@keywords iteration
#'@examples
#'
#'imp1 <- mice(nhanes,maxit=1)
#'imp2 <- mice.mids(imp1)
#'
#'# yields the same result as
#'imp <- mice(nhanes,maxit=2)
#'
#'# for example:
#'# 
#'# > imp$imp$bmi[1,]
#'#      1    2    3    4    5 
#'# 1 30.1 35.3 33.2 35.3 27.5
#'# > imp2$imp$bmi[1,]
#'#      1    2    3    4    5 
#'# 1 30.1 35.3 33.2 35.3 27.5
#'# 
#'@export
mice.mids <- function(obj, maxit = 1, diagnostics = TRUE, printFlag = TRUE, 
                      ...) {
  if (!is.mids(obj)) 
    stop("Object should be of type mids.")
  if (maxit < 1) 
    return(obj)
  
  call <- match.call()
  nvar <- ncol(obj$data)
  sumIt <- obj$iteration + maxit
  varnames <- dimnames(obj$data)[[2]]
  
  from <- obj$iteration + 1
  to <- from + maxit - 1
  
  loggedEvents <- obj$loggedEvents
  state <- list(it = 0, im = 0, co = 0, dep = "", meth = "", 
                log = !is.null(loggedEvents))
  if (is.null(loggedEvents)) 
    loggedEvents <- data.frame(it = 0, im = 0, co = 0, dep = "", 
                               meth = "", out = "")
  
  ## update the padModel structure
  if (is.null(obj$pad)) 
    p <- padModel(obj$data, obj$method, obj$predictorMatrix, obj$visitSequence, 
                  obj$post, obj$nmis, nvar) else p <- obj$pad
  r <- (!is.na(p$data))
  imp <- vector("list", ncol(p$data))
  for (j in obj$visitSequence) imp[[j]] <- obj$imp[[j]]
  assign(".Random.seed", obj$lastSeedValue, pos = 1)
  
  ## OK. Iterate.
  q <- sampler(p, obj$data, obj$where, obj$m, imp, r, obj$visitSequence, 
               c(from, to), printFlag, ...)
  
  ## restore the original NA's in the data
  for (j in p$visitSequence) p$data[(!r[, j]), j] <- NA
  
  ## delete data and imputations of automatic dummy variables
  data <- p$data[, seq_len(nvar)]
  imp <- q$imp[seq_len(nvar)]
  names(imp) <- varnames
  
  ## combine with previous chainMean and chainVar
  nvis <- length(obj$visitSequence)
  vnames <- varnames[obj$visitSequence]
  chainMean <- chainVar <- array(0, dim = c(nvis, to, obj$m), dimnames = list(vnames, 
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
  
  if (!state$log) 
    loggedEvents <- NULL
  if (state$log) 
    row.names(loggedEvents) <- seq_len(nrow(loggedEvents))
  
  ## save, and return
  midsobj <- list(call = call, data = as.data.frame(data), where = obj$where, 
                  m = obj$m, nmis = obj$nmis, imp = imp, method = obj$method, predictorMatrix = obj$predictorMatrix, 
                  visitSequence = obj$visitSequence, post = obj$post, seed = obj$seed, 
                  iteration = sumIt, lastSeedValue = .Random.seed, chainMean = chainMean, 
                  chainVar = chainVar, loggedEvents = loggedEvents)
  if (diagnostics) 
    midsobj <- c(midsobj, list(pad = p))
  oldClass(midsobj) <- "mids"
  return(midsobj)
}
