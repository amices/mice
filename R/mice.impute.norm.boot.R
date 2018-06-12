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
#'regression weights and imputes with normal residuals. 
#'@author Gerko Vink, Stef van Buuren, 2018
#'@references Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}:
#'Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#'Statistical Software}, \bold{45}(3), 1-67.
#'\url{https://www.jstatsoft.org/v45/i03/}
#'@family univariate imputation functions
#'@keywords datagen
#'@export
mice.impute.norm.boot <- function(y, ry, x, wy = NULL, ...) {
  if (is.null(wy)) wy <- !ry
  x <- cbind(1, as.matrix(x))
  n1 <- sum(ry)
  s <- sample(n1, n1, replace = TRUE)
  ss<-s
  dotxobs <- x[ry, , drop = FALSE][s, ]
  dotyobs <- y[ry][s]
  p <- estimice(dotxobs, dotyobs, ...)
  sigma <- sqrt((sum(p$r^2))/(n1 - ncol(x) - 1))
  return(x[wy, ] %*% p$c + rnorm(sum(wy)) * sigma)
}