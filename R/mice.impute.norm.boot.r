#'Imputation by linear regression, bootstrap method
#'
#'Imputes univariate missing data using linear regression with bootstrap
#'
#'@aliases mice.impute.norm.boot norm.boot
#'@inheritParams mice.impute.pmm
#'@return Vector with imputed data, same type as \code{y}, and of length 
#'\code{sum(wy)}
#'@details
#'Draws a bootstrap sample from \code{x[ry,]} and \code{y[ry]}, calculates
#'regression weights and imputes with normal residuals. The \code{ridge}
#'parameter adds a penalty term \code{ridge*diag(xtx)} to the
#'variance-covariance matrix \code{xtx}.
#'@author Stef van Buuren
#'@references Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}:
#'Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#'Statistical Software}, \bold{45}(3), 1-67.
#'\url{http://www.jstatsoft.org/v45/i03/}
#'@family univariate imputation functions
#'@keywords datagen
#'@export
mice.impute.norm.boot <- function(y, ry, x, wy = NULL, ridge = 1e-05, ...) {
  if (is.null(wy)) wy <- !ry
  x <- cbind(1, as.matrix(x))
  xobs <- x[ry, ]
  yobs <- y[ry]
  n1 <- sum(ry)
  s <- sample(n1, n1, replace = TRUE)
  dotxobs <- xobs[s, ]
  dotyobs <- yobs[s]
  xtx <- t(dotxobs) %*% dotxobs
  pen <- ridge * diag(xtx)
  if (length(pen) == 1) 
    pen <- matrix(pen)
  v <- solve(xtx + diag(pen))
  coef <- t(dotyobs %*% dotxobs %*% v)
  residuals <- dotyobs - dotxobs %*% coef
  sigma <- sqrt((sum(residuals^2))/(n1 - ncol(x) - 1))
  parm <- list(coef, sigma)
  names(parm) <- c("beta", "sigma")
  return(x[wy, ] %*% parm$beta + rnorm(sum(wy)) * parm$sigma)
}
