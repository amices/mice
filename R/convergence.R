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
#' In the unlikely event of perfect convergence, the autocorrelation equals
#' zero and the potential scale reduction factor equals one. To interpret
#' the convergence diagnostic(s) in the output of the function, it is
#' recommended to plot the diagnostics (ac and/or psrf) against the
#' iteration number (.it) per imputed variable (vrb). A persistently
#' decreasing trend across iterations indicates potential non-convergence.
#'
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
  if (!is.mids(data)) {
    stop("'data' not of class 'mids'")
  }
  if (data$m < 2) {
    stop("the number of imputations should be at least two (m > 1)")
  }
  if (data$iteration < 3) {
    stop("the number of iterations should be at least three (maxit > 2)")
  }
  if (is.null(data$chainMean)) {
    stop("no convergence diagnostics found", call. = FALSE)
  }
  if (parameter != "mean" & parameter != "sd") {
    stop("'parameter' not recognized, please use 'mean' or 'sd'")
  }
  vrbs <- names(data$data)
  p <- length(vrbs)
  m <- as.integer(data$m)
  t <- as.integer(data$iteration)
  out <- expand.grid(.it = seq_len(t), vrb = vrbs)

  # extract chain means or chain standard deviations
  if (parameter == "mean") {
    param <- lapply(seq(p), function(x) {
      aperm(data$chainMean[vrbs, , , drop = FALSE], c(2, 3, 1))[, , x]
    })
  }
  if (parameter == "sd") {
    param <- lapply(seq(p), function(x) {
      aperm(sqrt(data$chainVar)[vrbs, , , drop = FALSE], c(2, 3, 1))[, , x]
    })
  }
  names(param) <- vrbs

  # compute autocorrelation
  if (diagnostic == "all" | diagnostic == "ac") {
    ac <-
      purrr::map(vrbs, function(.vrb) {
        c(NA, dplyr::cummean(dplyr::coalesce(
          purrr::map_dbl(2:t, function(.itr) {
            suppressWarnings(stats::cor(
              param[[.vrb]][.itr - 1, ],
              param[[.vrb]][.itr, ],
              use = "pairwise.complete.obs"
            ))
          }), 0
        ))) + 0 * param[[.vrb]][, 1]
      })
    out <- base::cbind(out, ac = unlist(ac))
  }

  # compute potential scale reduction factor
  if (diagnostic == "all" | diagnostic == "psrf" | diagnostic == "gr") {
    psrf <- purrr::map_dfr(param, ~ {
      purrr::map_dfr(1:t, function(.itr) {
        data.frame(psrf = rstan::Rhat(.[1:.itr, ]))
      })
    })
    out <- base::cbind(out, psrf)
  }
  out[is.nan(out)] <- NA
  return(out)
}

# function to extend is.nan() to data.frame objects
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
