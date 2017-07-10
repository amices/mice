#'Imputation by linear regression without parameter uncertainty
#'
#'Imputes univariate missing data using linear regression analysis without 
#'accounting for the uncertainty of the model parameters.
#'
#'@aliases mice.impute.norm.nob norm.nob
#'@inheritParams mice.impute.pmm
#'@return Vector with imputed data, same type as \code{y}, and of length 
#'\code{sum(wy)}
#'@details
#'This function creates imputations using the spread around the 
#'fitted linear regression line of \code{y} given \code{x}, as 
#'fitted on the observed data.
#'
#'This function is provided mainly to allow comparison between proper (e.g., 
#'as implemented in \code{mice.impute.norm} and improper (this function)
#'normal imputation methods. 
#'
#'For large data, having many rows, differences between proper and improper 
#'methods are small, and in those cases one may opt for speed by using 
#'\code{mice.impute.norm.nob}.
#'@section Warning: The function does not incorporate the variability of the
#'regression weights, so it is not 'proper' in the sense of Rubin. For small
#'samples, variability of the imputed data is therefore underestimated.
#'@author Stef van Buuren, Karin Groothuis-Oudshoorn, 2000
#'@seealso \code{\link{mice}}, \code{\link{mice.impute.norm}}
#'@references Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}:
#'Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#'Statistical Software}, \bold{45}(3), 1-67.
#'\url{http://www.jstatsoft.org/v45/i03/}
#'
#'Brand, J.P.L. (1999). Development, Implementation and Evaluation of Multiple
#'Imputation Strategies for the Statistical Analysis of Incomplete Data Sets.
#'Ph.D. Thesis, TNO Prevention and Health/Erasmus University Rotterdam.
#'@family univariate imputation functions
#'@keywords datagen
#'@export
mice.impute.norm.nob <- function(y, ry, x, wy = NULL, ...) {
  if (is.null(wy)) wy <- !ry
  x <- cbind(1, as.matrix(x))
  parm <- .norm.fix(y, ry, x, ...)
  return(x[wy, ] %*% parm$beta + rnorm(sum(wy)) * parm$sigma)
}

.norm.fix <- function(y, ry, x, ridge = 1e-05, ...) {
  xobs <- x[ry, ]
  yobs <- y[ry]
  xtx <- t(xobs) %*% xobs
  pen <- ridge * diag(xtx)
  if (length(pen) == 1) 
    pen <- matrix(pen)
  v <- solve(xtx + diag(pen))
  coef <- t(yobs %*% xobs %*% v)
  residuals <- yobs - xobs %*% coef
  sigma <- sqrt((sum(residuals^2))/(sum(ry) - ncol(x) - 1))
  parm <- list(coef, sigma)
  names(parm) <- c("beta", "sigma")
  return(parm)
}
