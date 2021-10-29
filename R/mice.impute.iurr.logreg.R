#' Imputation by Indirect Use of (lasso) Regularised (logistic) Regression (IURR)
#'
#' Imputes univariate missing data using lasso 2-step logistic regression
#'
#' @aliases mice.impute.iurr.logreg iurr.logreg
#' @inheritParams mice.impute.pmm
#' @param nfolds The number of folds for the cross-validation of the lasso penalty.
#' The default is 10.
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#' @details
#' Uses lasso penalty to identify an active set, then samples imputation model
#' parameter values and uses them to define a posterior predictive distirbution
#' to sample imputations from.
#'
#' The user can specify a \code{predictMatrix} in the \code{mice} call
#' to define which predictors are provided to this univariate imputation method.
#' The lasso regularization will select, among the variables indicated by
#' the user, the ones that are important for imputation at any given iteration.
#' Therefore, users may force the exclusion of a predictor from a given
#' imputation model by speficing a \code{0} entry.
#' However, a non-zero entry does not guarantee the variable will be used,
#' as this decision is ultimately made by the lasso variable selection
#' procedure.
#'
#' This function implements the MICE-IURR for Bernoulli data univariate imputation
#' method presented by Deng Et Al (2016).
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
mice.impute.iurr.logreg <- function(y, ry, x, wy = NULL, nfolds = 10, ...) {
  install.on.demand("glmnet", ...)

  # Body
  if (is.null(wy)) wy <- !ry
  xobs <- x[ry, ]
  xmis <- x[wy, ]
  yobs <- y[ry]

  # Train imputation model
  # used later in the estiamtion require this.
  cv_lasso <- glmnet::cv.glmnet(x = xobs, y = yobs,
                                family = "binomial",
                                nfolds = nfolds,
                                alpha = 1)

  # Define Active Set
  glmnet_coefs <- as.matrix(coef(cv_lasso,
                                 s = "lambda.min"))[, 1]
  AS <- which((glmnet_coefs != 0)[-1]) # Non-zero reg coefficinets

  # Perform regular logreg draw
  xas <- x[, AS, drop = FALSE]
  vec <- mice.impute.logreg(y = y, ry = ry, x = xas, wy = wy,
                            ...)
  vec
}