#' Imputation by linear regression, bootstrap method
#'
#' Imputes univariate missing data using linear regression with bootstrap
#'
#' @aliases mice.impute.norm.boot norm.boot
#' @inheritParams mice.impute.pmm
#' @return Vector with imputed data, same type as `y`, and of length
#' `sum(wy)`
#' @details
#' Draws a bootstrap sample from `x[ry,]` and `y[ry]`, calculates
#' regression weights and imputes with normal residuals.
#' @author Gerko Vink, Stef van Buuren, 2018
#' @references Van Buuren, S., Groothuis-Oudshoorn, K. (2011). `mice`:
#' Multivariate Imputation by Chained Equations in `R`. *Journal of
#' Statistical Software*, **45**(3), 1-67.
#' \doi{10.18637/jss.v045.i03}
#' @family univariate imputation functions
#' @keywords datagen
#' @export
mice.impute.norm.boot <- function(y, ry, x, wy = NULL, ...) {
  if (is.null(wy)) wy <- !ry
  x <- cbind(1, as.matrix(x))
  n1 <- sum(ry)
  s <- sample(n1, n1, replace = TRUE)
  ss <- s
  dotxobs <- x[ry, , drop = FALSE][s, ]
  dotyobs <- y[ry][s]
  p <- estimice(dotxobs, dotyobs, ...)
  sigma <- sqrt((sum(p$r^2)) / (n1 - ncol(x) - 1))
  x[wy, ] %*% p$c + rnorm(sum(wy)) * sigma
}
