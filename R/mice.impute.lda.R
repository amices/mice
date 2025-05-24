#' Imputation by linear discriminant analysis
#'
#' Imputes univariate missing data using linear discriminant analysis
#'
#' @inheritParams mice.impute.pmm
#' @param ... Other named arguments. Not used.
#' @return Vector with imputed data, of type factor, and of length
#' \code{sum(wy)}
#' @details Imputation of categorical response variables by linear discriminant analysis.
#' This function uses the Venables/Ripley functions \code{lda()} and
#' \code{predict.lda()} to compute posterior probabilities for each incomplete
#' case, and draws the imputations from this posterior.
#'
#' This function can be called from within the Gibbs sampler by specifying
#' \code{"lda"} in the \code{method} argument of \code{mice()}. This method is usually
#' faster and uses fewer resources than calling the function, but the statistical
#' properties may not be as good (Brand, 1999).
#' \code{\link{mice.impute.polyreg}}.
#' @section Warning: The function does not incorporate the variability of the
#' discriminant weight, so it is not 'proper' in the sense of Rubin. For small
#' samples and rare categories in the \code{y}, variability of the imputed data
#' could therefore be underestimated.
#'
#' Added: SvB June 2009 Tried to include bootstrap, but disabled since
#' bootstrapping may easily lead to constant variables within groups.
#' @author Stef van Buuren, Karin Groothuis-Oudshoorn, 2000
#' @seealso \code{\link{mice}}, \code{\link{mice.impute.polyreg}},
#' \code{\link[MASS]{lda}}
#' @references Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}:
#' Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#' Statistical Software}, \bold{45}(3), 1-67.
#' \doi{10.18637/jss.v045.i03}
#'
#' Brand, J.P.L. (1999). Development, Implementation and Evaluation of Multiple
#' Imputation Strategies for the Statistical Analysis of Incomplete Data Sets.
#' Ph.D. Thesis, TNO Prevention and Health/Erasmus University Rotterdam. ISBN
#' 90-74479-08-1.
#'
#' Venables, W.N. & Ripley, B.D. (1997). Modern applied statistics with S-PLUS
#' (2nd ed). Springer, Berlin.
#' @family univariate imputation functions
#' @keywords datagen
#' @export
mice.impute.lda <- function(y, ry, x, wy = NULL, ...) {
  install.on.demand("MASS", ...)
  if (is.null(wy)) wy <- !ry
  fy <- as.factor(y)
  nc <- length(levels(fy))

  #   SvB June 2009 - take bootstrap sample of training data
  #   idx <- sample((1:length(y))[ry], size=sum(ry), replace=TRUE)
  #   x[ry,] <- x[idx,]
  #   y[ry] <- y[idx]
  #   end bootstrap

  fit <- MASS::lda(x, fy, subset = ry)
  post <- predict(fit, x[wy, , drop = FALSE])$posterior
  un <- rep(runif(sum(wy)), each = nc)
  idx <- 1 + apply(un > apply(post, 1, cumsum), 2, sum)
  levels(fy)[idx]
}
