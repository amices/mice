#' Imputation by direct use of lasso linear regression
#'
#' Imputes univariate missing normal data using lasso linear regression with bootstrap.
#'
#' @aliases mice.impute.lasso.norm lasso.norm
#' @inheritParams mice.impute.norm.boot
#' @param nfolds The number of folds for the cross-validation of the lasso penalty.
#' The default is 10.
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#' @details
#' The method consists of the following steps:
#' \enumerate{
#' \item For a given y variable under imputation, draw a bootstrap version y*
#' with replacement from the observed cases \code{y[ry]}, and stores in x* the
#' corresponding values from \code{x[ry, ]}.
#' \item Fit a regularised (lasso) linear regression with y* as the outcome,
#' and x* as predictors.
#' A vector of regression coefficients bhat is obtained.
#' All of these coefficients are considered random draws from the imputation model
#' parameters posterior distribution.
#' Same of these coefficients will be shrunken to 0.
#' \item Draw the imputed values from the predictive distribution defined by
#' the original (non-bootstrap) data, bhat, and estimated error variance.
#' }
#' The method is based on the Direct Use of Regularized Regression (DURR) proposed by
#' Zhao & Long (2016) and Deng et al (2016).
#' @author Edoardo Costantini, 2021
#' @references
#'
#' Deng, Y., Chang, C., Ido, M. S., & Long, Q. (2016). Multiple imputation for
#' general missing data patterns in the presence of high-dimensional data.
#' Scientific reports, 6(1), 1-10.
#'
#' Zhao, Y., & Long, Q. (2016). Multiple imputation in the presence of
#' high-dimensional data. Statistical Methods in Medical Research, 25(5),
#' 2021-2035.
#'
#' @family univariate imputation functions
#' @keywords datagen
#' @export
mice.impute.lasso.norm <- function(y, ry, x, wy = NULL, nfolds = 10, ...) {
  install.on.demand("glmnet", ...)

  # Bootstrap sample
  if (is.null(wy)) wy <- !ry
  n1 <- sum(ry)
  s <- sample(n1, n1, replace = TRUE)
  x_glmnet <- cbind(1, x)
  dotxobs <- x_glmnet[ry, , drop = FALSE][s, , drop = FALSE]
  dotyobs <- y[ry][s]

  # Train imputation model
  cv_lasso <- glmnet::cv.glmnet(
    x = dotxobs, y = dotyobs,
    family = "gaussian",
    nfolds = nfolds,
    alpha = 1
  )

  # Obtain imputations
  s2hat <- mean((predict(cv_lasso, dotxobs, s = "lambda.min") - dotyobs)^2)
  as.vector(predict(cv_lasso, x_glmnet[wy, ], s = "lambda.min")) +
    rnorm(sum(wy), 0, sqrt(s2hat))
}
