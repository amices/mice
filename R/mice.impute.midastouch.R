#' Imputation by predictive mean matching with distance aided donor selection
#' 
#' Imputes univariate missing data using predictive mean matching.
#' @aliases mice.impute.midastouch
#' @inheritParams mice.impute.pmm
#' @param midas.kappa Scalar. If \code{NULL} (default) then the 
#' optimal \code{kappa} gets selected automatically. Alternatively, the user 
#' may specify a scalar. Siddique and Belin 2008 find \code{midas.kappa = 3} 
#' to be sensible.
#' @param outout Logical. If \code{TRUE} (default) one model is estimated 
#' for each donor (leave-one-out principle). For speedup choose 
#' \code{outout = FALSE}, which estimates one model for all observations 
#' leading to in-sample predictions for the donors and out-of-sample 
#' predictions for the recipients. Mind the inappropriateness, though.
#' @param neff FOR EXPERTS. Null or character string. The name of an existing 
#' environment in which the effective sample size of the donors for each 
#' loop (CE iterations times multiple imputations) is supposed to be written. 
#' The effective sample size is necessary to compute the correction for the 
#' total variance as originally suggested by Parzen, Lipsitz and 
#' Fitzmaurice 2005. The objectname is \code{midastouch.neff}.
#' @param debug FOR EXPERTS. Null or character string. The name of an existing 
#' environment in which the input is supposed to be written. The objectname 
#' is \code{midastouch.inputlist}.
#' @return Vector with imputed data, same type as \code{y}, and of 
#' length \code{sum(wy)}
#' @details Imputation of \code{y} by predictive mean matching, based on 
#' Rubin (1987, p. 168, formulas a and b) and Siddique and Belin 2008. 
#' The procedure is as follows:
#' \enumerate{
#' \item Draw a bootstrap sample from the donor pool.
#' \item Estimate a beta matrix on the bootstrap sample by the leave one out principle.
#' \item Compute type II predicted values for \code{yobs} (nobs x 1) and \code{ymis} (nmis x nobs).
#' \item Calculate the distance between all \code{yobs} and the corresponding \code{ymis}. 
#' \item Convert the distances in drawing probabilities.
#' \item For each recipient draw a donor from the entire pool while considering the probabilities from the model.
#' \item Take its observed value in \code{y} as the imputation.
#' }
#' @examples
#' # do default multiple imputation on a numeric matrix
#' imp <- mice(nhanes, method = 'midastouch')
#' imp
#' 
#' # list the actual imputations for BMI
#' imp$imp$bmi
#' 
#' # first completed data matrix
#' complete(imp)
#' 
#' # imputation on mixed data with a different method per column
#' mice(nhanes2, method = c('sample', 'midastouch', 'logreg', 'norm'))
#' @author Philipp Gaffert, Florian Meinfelder, Volker Bosch 2015
#' @references
#' Gaffert, P., Meinfelder, F., Bosch V. (2015) Towards an MI-proper 
#' Predictive Mean Matching, Discussion Paper. 
#' \url{https://www.uni-bamberg.de/fileadmin/uni/fakultaeten/sowi_lehrstuehle/statistik/Personen/Dateien_Florian/properPMM.pdf}
#' 
#' Little, R.J.A. (1988), Missing data adjustments in large 
#' surveys (with discussion), Journal of Business Economics and 
#' Statistics, 6, 287--301.
#' 
#' Parzen, M., Lipsitz, S. R., Fitzmaurice, G. M. (2005), A note on reducing 
#' the bias of the approximate bayesian bootstrap imputation variance estimator. 
#' Biometrika \bold{92}, 4, 971--974.
#' 
#' Rubin, D.B. (1987), Multiple imputation for nonresponse in surveys. New York: Wiley.
#' 
#' Siddique, J., Belin, T.R. (2008), Multiple imputation using an iterative 
#' hot-deck with distance-based donor selection. Statistics in medicine, 
#' \bold{27}, 1, 83--102
#' 
#' Van Buuren, S., Brand, J.P.L., Groothuis-Oudshoorn C.G.M., Rubin, D.B. (2006), 
#' Fully conditional specification in multivariate imputation.
#' \emph{Journal of Statistical Computation and Simulation}, \bold{76}, 12, 
#' 1049--1064.
#' 
#' Van Buuren, S., Groothuis-Oudshoorn, K. (2011), \code{mice}: Multivariate 
#' Imputation by Chained Equations in \code{R}. \emph{Journal of 
#' Statistical Software}, \bold{45}, 3, 1--67. \url{http://www.jstatsoft.org/v45/i03/}
#' @family univariate imputation functions
#' @keywords datagen
#' @export
mice.impute.midastouch <- function(y, ry, x, wy = NULL, ridge = 1e-05, 
                                   midas.kappa = NULL, 
                                   outout = TRUE, neff = NULL, 
                                   debug = NULL, ...)
{
  if (is.null(wy)) 
    wy <- !ry
  
  #+ auxiliaries +#
  if (!is.null(debug))
  {
    midastouch.inputlist <- list(y = y, ry = ry, x = x, omega = NULL)
  }
  sminx <- .Machine$double.eps^(1/4)
  
  #+ ensure data format +#
  x <- data.matrix(x)
  storage.mode(x) <- "numeric"
  X <- cbind(1, x)
  y <- as.numeric(y)
  
  #+ get data dimensions +#
  nobs <- sum(ry)
  nmis <- sum(wy)
  n <- length(ry)
  obsind <- ry
  misind <- wy
  m <- ncol(X)
  yobs <- y[obsind]
  Xobs <- X[obsind, , drop = FALSE]
  Xmis <- X[misind, , drop = FALSE]
  
  #+ P-Step +#
  ##++++ bootstrap
  omega <- bootfunc.plain(nobs)
  if (!is.null(debug))
  {
    midastouch.inputlist$omega <- omega
    assign(x = "midastouch.inputlist", value = midastouch.inputlist, 
           envir = get(debug))
  }
  
  ##++++ beta estimation
  CX <- omega * Xobs
  XCX <- crossprod(Xobs, CX)
  if (ridge > 0)
  {
    diag(XCX) <- diag(XCX) * (1 + c(0, rep(ridge, m - 1)))
  }
  
  # = check if any diagonal element is exactly zero ===========#
  diag0 <- diag(XCX) == 0  #==#
  if (any(diag0)) 
  {
    diag(XCX)[diag0] <- max(sminx, ridge)
  }  #==#
  # ============================================================#
  
  Xy <- crossprod(CX, yobs)
  beta <- solve(XCX, Xy)
  yhat.obs <- c(Xobs %*% beta)
  
  ##++++ kappa estimation
  if (is.null(midas.kappa))
  {
    mean.y <- as.vector(crossprod(yobs, omega) / nobs)
    eps <- yobs - yhat.obs
    r2 <- 1 - c(crossprod(omega, eps^2)/crossprod(omega, (yobs - mean.y)^2))
    ## slight deviation from the paper to ensure real results paper: a tiny
    ## delta is added to the denominator R Code: min function is used, note
    ## that this correction gets active for r2>.999 only
    midas.kappa <- min((50 * r2/(1 - r2))^(3/8), 100)
    ## if r2 cannot be determined (eg zero variance in yhat), use 3 as
    ## suggested by Siddique / Belin
    if (is.na(midas.kappa))
    {
      midas.kappa <- 3
    }
  }
  
  #+ I-Step +#
  if (outout)
  {
    ##++++ P-step if out of sample predictions for donors
    ## estimate one model per donor by leave-one-out
    XXarray_pre <- t(t(apply(X = Xobs, MARGIN = 1, FUN = tcrossprod)) * 
                       omega)
    ridgeind <- c(1:(m - 1)) * (m + 1) + 1
    if (ridge > 0)
    {
      XXarray_pre[ridgeind, ] <- XXarray_pre[ridgeind, ] * (1 + ridge)
    }
    XXarray <- c(XCX) - XXarray_pre
    
    # = check if any diagonal element is exactly zero
    # =======================#
    diag0 <- XXarray[ridgeind, ] == 0  #==#
    if (any(diag0)) 
    {
      XXarray[ridgeind, ][diag0] <- max(sminx, ridge)
    }  #==#
    # =======================================================================#
    
    Xyarray <- c(Xy) - t(Xobs * yobs * omega)
    BETAarray <- apply(rbind(XXarray, Xyarray), 2, function(x, m)
    {
      solve(a = matrix(head(x, m^2), m), b = tail(x, m))
    }, m = m)
    YHATdon <- rowSums(Xobs * t(BETAarray))
    ## each recipient has nobs different yhats
    YHATrec <- Xmis %*% BETAarray
    ##++++ distance calculations
    dist.mat <- YHATdon - t(YHATrec)
  } else
  {
    yhat.mis <- c(Xmis %*% beta)
    dist.mat <- yhat.obs - matrix(data = yhat.mis, nrow = nobs, ncol = nmis, 
                                  byrow = TRUE)
  }
  
  ##++++ convert distances to drawing probs // ensure real results
  delta.mat <- 1/((abs(dist.mat))^midas.kappa)
  delta.mat <- minmax(delta.mat)
  probs <- delta.mat * omega
  csums <- minmax(colSums(probs, na.rm = TRUE))
  probs <- t(t(probs)/csums)
  
  #+ calculate neff +#
  if (!is.null(neff))
  {
    if (!exists("midastouch.neff", envir = get(neff)))
    {
      assign(x = "midastouch.neff", value = list(), envir = get(neff))
    }
    midastouch.neff <- get("midastouch.neff", envir = get(neff))
    midastouch.neff[[length(midastouch.neff) + 1]] <- mean(1/rowSums((t(delta.mat)/csums)^2))
    assign(x = "midastouch.neff", value = midastouch.neff, envir = get(neff))
  }
  
  #+ return result +#
  index <- apply(probs, 2, sample, x = nobs, size = 1, replace = FALSE)
  yimp <- y[obsind][index]
  return(yimp)
}
