#' Imputation by Indirect Use of (lasso) Regularised (linear) Regression (IURR)
#'
#' Imputes univariate missing data using lasso 2-step linear regression
#'
#' @aliases mice.impute.iurr.norm iurr.norm
#' @inheritParams mice.impute.pmm
#' @param nfolds The number of folds for the cross-validation of the lasso penalty.
#' The default is 10.
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#' @details
#' Uses lasso penalty to identify an active set, then samples imputation model
#' parameter values and uses them to define a posterior predictive distirbution
#' to sample imputations.
#' The method is based on the Iirect Use of Regularized Regression proposed by
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
mice.impute.iurr.norm <- function(y, ry, x, wy = NULL, nfolds = 10, ...) {
  install.on.demand("glmnet", ...)

  # Body
  if (is.null(wy)) wy <- !ry
  xobs <- x[ry, ]
  xmis <- x[wy, ]
  yobs <- y[ry]

  # Train imputation model
  # used later in the estiamtion require this.
  cv_lasso <- glmnet::cv.glmnet(x = xobs, y = yobs,
                                family = "gaussian",
                                nfolds = nfolds,
                                alpha = 1)

  # Define Active Set
  glmnet_coefs <- as.matrix(coef(cv_lasso,
                                 s = "lambda.min"))[, 1]
  AS <- which((glmnet_coefs != 0)[-1]) # Non-zero reg coefficinets

  # Perform regular norm draw from Bayesian linear model
  xas <- x[, AS, drop = FALSE]
  vec <- mice.impute.norm(y = y, ry = ry, x = xas, wy = wy,
                          ...)
  vec
}