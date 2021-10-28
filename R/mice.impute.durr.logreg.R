#' Imputation by Direct Use of (lasso) Regularised (logistic) Regression (DURR)
#'
#' Imputes univariate missing data using lasso logistic regression with bootstrap
#'
#' @aliases mice.impute.durr.logreg durr.logreg
#' @inheritParams mice.impute.pmm
#' @param nfolds The number of folds for the cross-validation of the lasso penalty.
#' The default is 10.
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#' @details
#' Draws a bootstrap sample from \code{x} and \code{y}, trains a
#' penalised lasso logistic regression by \code{nfolds} cross-validation,
#' and imputes based on the logistic function.
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
mice.impute.durr.logreg <- function(y, ry, x, wy = NULL, nfolds = 10, ...) {
  install.on.demand("glmnet", ...)
  if (is.null(wy)) wy <- !ry

  # Bootstrap sample
  n1 <- sum(ry)
  s <- sample(n1, n1, replace = TRUE)
  dotxobs <- x[ry, , drop = FALSE][s, ]
  dotyobs <- y[ry][s]

  # Train imputation model
  cv_lasso <- glmnet::cv.glmnet(x = dotxobs, y = dotyobs,
                                family = "binomial",
                                nfolds = nfolds,
                                alpha = 1)

  # Obtain imputation
  p <- 1 / (1 + exp(predict(cv_lasso, x[wy, ], s = "lambda.min")))
  vec <- (runif(nrow(p)) <= p)
  vec[vec] <- 1
  if (is.factor(y)) {
    vec <- factor(vec, c(0, 1), levels(y))
  }
  vec
}
