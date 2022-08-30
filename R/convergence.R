#' Computes convergence diagnostics for a \code{mids} object
#'
#' Takes an object of class \code{mids}, computes the autocorrelation
#' and/or potential scale reduction factor, and returns a \code{data.frame}
#' with the specified diagnostic(s) per iteration.
#'
#' @param data An object of class \code{mids} as created by the function
#' \code{mice()}.
#' @param diagnostic A keyword. One of the following keywords: \code{"ac"},
#' \code{"all"}, \code{"gr"} and \code{"psrf"}. See the Details section
#' for the interpretation.
#' The default is \code{diagnostic = "all"} which returns both the
#' autocorrelation and potential scale reduction factor per iteration.
#' @param parameter A keyword. One of the following keywords: \code{"mean"}
#' or \code{"sd"} to evaluate chain means or chain standard deviations,
#' respectively.
#' @param \dots Additional arguments. Not used.
#' @return A \code{data.frame} with the autocorrelation and/or potential
#' scale reduction factor per iteration of the MICE algorithm.
#' @details
#' The argument \code{diagnostic} can be length-1 character, which is
#' matched to one of the following keywords:
#' \describe{
#' \item{\code{"all"}}{computes both the lag-1 autocorrelation as well as 
#' the potential scale reduction factor (cf. Vehtari et al., 2021) per 
#' iteration of the MICE algorithm;}
#' \item{\code{"ac"}}{computes only the autocorrelation per iteration;}
#' \item{\code{"psrf"}}{computes only the potential scale reduction factor
#' per iteration;}
#' \item{\code{"gr"}}{same as \code{psrf}, the potential scale reduction
#' factor is colloquially called the Gelman-Rubin diagnostic.}
#' }
#' @seealso \code{\link{mice}}, \code{\link[=mids-class]{mids}}
#' @keywords none
#' @references Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Burkner, 
#' P.-C. (2021). Rank-Normalization, Folding, and Localization: An Improved 
#' R for Assessing Convergence of MCMC. Bayesian Analysis, 1(1), 1-38. 
#' https://doi.org/10.1214/20-BA1221
#' @examples
#'
#' # obtain imputed data set
#' imp <- mice(nhanes2, print = FALSE)
#' # compute convergence diagnostics
#' convergence(imp)
#' @export
convergence <- function(data, diagnostic = "all", parameter = "mean", ...) {
  # process inputs
  install.on.demand("rstan", ...)
  install.on.demand("purrr", ...)
  if (!is.mids(data))
    stop("'data' not of class 'mids'")
  if (is.null(data$chainMean)) 
    stop("no convergence diagnostics found", call. = FALSE)
  if (parameter != "mean" & parameter != "sd")
    stop("'parameter' not recognized, please use 'mean' or 'sd'")
  vrbs <- names(data$data)
  p <- length(vrbs)
  m <- as.integer(data$m)
  t <- as.integer(data$iteration)
  out <- expand.grid(.it = seq_len(t), vrb = vrbs)
    
  # extract chain means or chain standard deviations
  if (parameter == "mean") {
    param <- lapply(seq(p), function(x) aperm(data$chainMean[vrbs, , , drop = FALSE], c(2, 3, 1))[ , , x]) 
  }
  if (parameter == "sd") {
    param <- lapply(seq(p), function(x) aperm(sqrt(data$chainVar)[vrbs, , , drop = FALSE], c(2, 3, 1))[ , , x])
  }
  names(param) <- vrbs
  
  # compute autocorrelation
  if (diagnostic == "all" | diagnostic == "ac") {
    ac <-
      purrr::map_dfr(vrbs, function(.vrb) {
        base::rbind(NA, NA, purrr::map_dfr(3:t, function(.itr) {
          data.frame(ac = max(purrr::map_dbl(1:m, function(.imp) {
            suppressWarnings(stats::cor(param[[.vrb]][1:.itr - 1, .imp], param[[.vrb]][2:.itr, .imp]))
          })))
        }))
      })
    out <- base::cbind(out, ac)
  }
  
  # compute potential scale reduction factor
  if (diagnostic == "all" | diagnostic == "psrf" | diagnostic == "gr") {
    psrf <- purrr::map_dfr(param, ~ {
      purrr::map_dfr(1:t, function(.itr) {
        data.frame(psrf = rstan::Rhat(.[1:.itr,]))
      })
    })
    out <- base::cbind(out, psrf)
  }
  
  return(out)
}

