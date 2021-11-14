#' Imputation by indirect use of lasso logistic regression
#'
#' Imputes univariate missing data using logistic regression following a
#' preprocessing lasso variable selection step.
#'
#' @aliases mice.impute.lasso.select.logreg lasso.select.logreg
#' @inheritParams mice.impute.pmm
#' @param nfolds The number of folds for the cross-validation of the lasso penalty.
#' The default is 10.
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#' @details
#' The method consists of the following steps:
#' \enumerate{
#' \item For a given \code{y} variable under imputation, fit a linear regression with lasso
#' penalty using \code{y[ry]} as dependent variable and \code{x[ry, ]} as predictors.
#' The coefficients that are not shrunk to 0 define the active set of predictors
#' that will be used for imputation.
#' \item Fit a logit with the active set of predictors, and find (bhat, V(bhat))
#' \item Draw BETA from N(bhat, V(bhat))
#' \item Compute predicted scores for m.d., i.e. logit-1(X BETA)
#' \item Compare the score to a random (0,1) deviate, and impute.
#' }
#' The user can specify a \code{predictorMatrix} in the \code{mice} call
#' to define which predictors are provided to this univariate imputation method.
#' The lasso regularization will select, among the variables indicated by
#' the user, the ones that are important for imputation at any given iteration.
#' Therefore, users may force the exclusion of a predictor from a given
#' imputation model by speficing a \code{0} entry.
#' However, a non-zero entry does not guarantee the variable will be used,
#' as this decision is ultimately made by the lasso variable selection
#' procedure.
#'
#' The method is based on the Indirect Use of Regularized Regression (IURR) proposed by
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
mice.impute.lasso.select.logreg <- function(y, ry, x, wy = NULL, nfolds = 10, ...) {
  install.on.demand("glmnet", ...)

  # Body
  if (is.null(wy)) wy <- !ry
  x_glmnet <- cbind(1, x)
  xobs <- x_glmnet[ry, , drop = FALSE]
  xmis <- x[wy, ]
  yobs <- y[ry]

  # Train imputation model
  # used later in the estiamtion require this.
  cv_lasso <- glmnet::cv.glmnet(
    x = xobs, y = yobs,
    family = "binomial",
    nfolds = nfolds,
    alpha = 1
  )

  # Define Active Set
  glmnet_coefs <- as.matrix(coef(cv_lasso,
    s = "lambda.min"
  ))[, 1]
  AS <- which((glmnet_coefs != 0)[-1]) # Non-zero reg coefficinets

  # Perform regular logreg draw
  xas <- x_glmnet[, AS, drop = FALSE]
  vec <- mice.impute.logreg(
    y = y, ry = ry, x = xas, wy = wy,
    ...
  )
  vec
}
