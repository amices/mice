#' Imputation by Direct Use of (lasso) Regularised (linear) Regression (DURR)
#'
#' Imputes univariate missing data using lasso linear regression with bootstrap
#'
#' @aliases mice.impute.durr.norm durr.norm
#' @inheritParams mice.impute.norm.boot
#' @param nfolds The number of folds for the cross-validation of the lasso penalty.
#' The default is 10.
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#' @details
#' Draws a bootstrap sample from \code{x} and \code{y}, trains a
#' penalised lasso regression by \code{nfolds} cross-validation, and imputes
#' with normal residuals.
#' The method is based on the Direct Use of Regularized Regression proposed by
#' Zhao & Long (2016) and Deng et al (2016).
#' When using only mice.impute.iurr methods, the user can provide the default
#' predictor matrix. The method will then take care of selecting which variables are
#' important for imputation.
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
mice.impute.durr.norm <- function(y, ry, x, wy = NULL, nfolds = 10, ...) {
  install.on.demand("glmnet", ...)

  # Bootstrap sample
  if (is.null(wy)) wy <- !ry
  n1 <- sum(ry)
  s <- sample(n1, n1, replace = TRUE)
  dotxobs <- x[ry, , drop = FALSE][s, ]
  dotyobs <- y[ry][s]

  # Train imputation model
  cv_lasso <- glmnet::cv.glmnet(x = dotxobs, y = dotyobs,
                                family = "gaussian",
                                nfolds = nfolds,
                                alpha = 1)

  # Obtain imputations
  s2hat   <- mean((predict(cv_lasso, dotxobs, s = "lambda.min") - dotyobs)^2)
  as.vector(predict(cv_lasso, x[wy, ], s = "lambda.min")) +
    rnorm(sum(wy), 0, sqrt(s2hat))
}
