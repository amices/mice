#' Imputation by indirect use of lasso linear regression
#'
#' Imputes univariate missing data using predictive mean matching following a
#' preprocessing lasso variable selection step.
#'
#' @aliases mice.impute.lasso.select.pmm lasso.select.pmm
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
#' Coefficients that are not shrunk to 0 define an active set of predictors
#' that will be used for imputation
#' \item Define a Bayesian linear model using \code{y[ry]} as the
#' dependent variable, the active set of \code{x[ry, ]} as predictors, and standard
#' non-informative priors
#' \item Draw parameter values for the intercept and regression weights
#' \item To impute \code{y}, predictive mean matching, based on van Buuren (2018, p. 81),
#' is used with the drawn parameters as input
#' }
#' The user can specify a \code{predictorMatrix} in the \code{mice} call
#' to define which predictors are provided to this univariate imputation method.
#' The lasso regularization will select, among the variables indicated by
#' the user, the ones that are important for imputation at any given iteration.
#' Therefore, users may force the exclusion of a predictor from a given
#' imputation model by specifying a \code{0} entry.
#' However, a non-zero entry does not guarantee the variable will be used,
#' as this decision is ultimately made by the lasso variable selection
#' procedure.
#' 
#' Currently this method only works for continuous variables.
#' 
#' The method is based on the Indirect Use of Regularized Regression (IURR) proposed by
#' Zhao & Long (2016) and Deng et al (2016) but modified to use predictive mean matching to
#' generate imputations instead of using Bayesian linear regression.
#' @author Ruben Custers, 2026
#' @references
#'
#' Deng, Y., Chang, C., Ido, M. S., & Long, Q. (2016). Multiple imputation for
#' general missing data patterns in the presence of high-dimensional data.
#' Scientific reports, 6(1), 1-10.
#'
#' Van Buuren, S. (2018).
#' \href{https://stefvanbuuren.name/fimd/sec-pmm.html}{\emph{Flexible Imputation of Missing Data. Second Edition.}}
#' Chapman & Hall/CRC. Boca Raton, FL.
#'
#' Zhao, Y., & Long, Q. (2016). Multiple imputation in the presence of
#' high-dimensional data. Statistical Methods in Medical Research, 25(5),
#' 2021-2035.
#'
#' @family univariate imputation functions
#' @keywords datagen
#' @export
mice.impute.lasso.select.pmm <- function(
  y,
  ry,
  x,
  wy = NULL,
  nfolds = 10,
  ...
) {
  install.on.demand("glmnet", ...)

  # Body
  if (is.null(wy)) {
    wy <- !ry
  }
  
  xas <- .mice.impute.lasso.select(y, ry, x, wy, nfolds, ...)
  
  vec <- mice.impute.pmm(
    y = y,
    ry = ry,
    x = xas,
    wy = wy,
    ...
  )
  vec
}
