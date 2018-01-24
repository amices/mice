#'Imputation by Bayesian linear regression
#'
#'Calculates imputations for univariate missing data by Bayesian linear 
#'regression, also known as the normal model.
#'
#'@aliases mice.impute.norm norm
#'@inheritParams mice.impute.pmm
#'@return Vector with imputed data, same type as \code{y}, and of length 
#'\code{sum(wy)}
#'@author Stef van Buuren, Karin Groothuis-Oudshoorn
#'@details
#' Imputation of \code{y} by the normal model by the method defined by 
#' Rubin (1987, p. 167). The procedure is as follows:
#'
#'\enumerate{
#'\item{Calculate the cross-product matrix \eqn{S=X_{obs}'X_{obs}}.}
#'\item{Calculate \eqn{V = (S+{diag}(S)\kappa)^{-1}}, with some small ridge 
#'parameter \eqn{\kappa}.}
#'\item{Calculate regression weights \eqn{\hat\beta = VX_{obs}'y_{obs}.}}
#'\item{Draw a random variable \eqn{\dot g \sim \chi^2_\nu} with \eqn{\nu=n_1 - q}.}
#'\item{Calculate \eqn{\dot\sigma^2 = (y_{obs} - X_{obs}\hat\beta)'(y_{obs} - X_{obs}\hat\beta)/\dot g.}}
#'\item{Draw \eqn{q} independent \eqn{N(0,1)} variates in vector \eqn{\dot z_1}.}
#'\item{Calculate \eqn{V^{1/2}} by Cholesky decomposition.}
#'\item{Calculate \eqn{\dot\beta = \hat\beta + \dot\sigma\dot z_1 V^{1/2}}.}
#'\item{Draw \eqn{n_0} independent \eqn{N(0,1)} variates in vector \eqn{\dot z_2}.}
#'\item{Calculate the \eqn{n_0} values \eqn{y_{imp} = X_{mis}\dot\beta + \dot z_2\dot\sigma}.}
#'}
#'
#'Using \code{mice.impute.norm} for all columns emulates Schafer's NORM method (Schafer, 1997).
#'@references 
#'Rubin, D.B (1987). Multiple Imputation for Nonresponse in Surveys. New York: John Wiley & Sons.
#'
#'Schafer, J.L. (1997). Analysis of incomplete multivariate data. London: Chapman & Hall.
#'@family univariate imputation functions
#'@keywords datagen
#'@export
mice.impute.norm <- function(y, ry, x, wy = NULL, ...) {
  if (is.null(wy)) wy <- !ry
  x <- cbind(1, as.matrix(x))
  parm <- .norm.draw(y, ry, x, ...)
  return(x[wy, ] %*% parm$beta + rnorm(sum(wy)) * parm$sigma)
}

#' Draws values of beta and sigma by Bayesian linear regression
#' 
#' This function draws random values of beta and sigma under the Bayesian 
#' linear regression model as described in Rubin (1987, p. 167). This function
#' can be called by user-specified imputation functions.
#' 
#'@aliases norm.draw .norm.draw
#'@param y Incomplete data vector of length \code{n}
#'@param ry Vector of missing data pattern (\code{FALSE}=missing,
#'\code{TRUE}=observed)
#'@param x Matrix (\code{n} x \code{p}) of complete covariates.
#'@param ridge A small numerical value specifying the size of the ridge used. 
#' The default value \code{ridge = 1e-05} represents a compromise between stability
#' and unbiasedness. Decrease \code{ridge} if the data contain many junk variables.
#' Increase \code{ridge} for highly collinear data. 
#'@param ... Other named arguments.
#'@return A \code{list} containing components \code{coef} (least squares estimate),
#'\code{beta} (drawn regression weights) and \code{sigma} (drawn value of the 
#'residual standard deviation).
#'@references
#'Rubin, D.B. (1987). \emph{Multiple imputation for nonresponse in surveys}. New York: Wiley.
#'@author Stef van Buuren, Karin Groothuis-Oudshoorn, 2000
#'@export
norm.draw <- function(y, ry, x, ridge = 1e-05, ...) 
  return(.norm.draw(y, ry, x, ridge = 1e-05, ...))

###'@rdname norm.draw
###'@export
.norm.draw <- function(y, ry, x, ridge = 1e-05, ...) {
  xobs <- x[ry, ]
  yobs <- y[ry]
  xtx <- crossprod(xobs)
  pen <- ridge * diag(xtx)
  if (length(pen) == 1)
    pen <- matrix(pen)
  v <- solve(xtx + diag(pen))
  coef <- t(yobs %*% xobs %*% v)
  residuals <- yobs - xobs %*% coef
  df <- max(sum(ry) - ncol(x), 1)
  sigma.star <- sqrt(sum((residuals)^2)/rchisq(1, df))
  beta.star <- coef + (t(chol(sym(v))) %*% rnorm(ncol(x))) * sigma.star
  parm <- list(coef, beta.star, sigma.star)
  names(parm) <- c("coef", "beta", "sigma")
  return(parm)
}

