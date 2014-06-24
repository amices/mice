# --------------------MICE.IMPUTE.NORM----------------------------------

#'Imputation by Bayesian linear regression
#'
#'Imputes univariate missing data using Bayesian linear regression analysis
#'
#'Draws values of \code{beta} and \code{sigma} for Bayesian linear regression
#'imputation of \code{y} given \code{x} according to Rubin p. 167.
#'
#'@aliases mice.impute.norm norm
#'@param y Incomplete data vector of length \code{n}
#'@param ry Vector of missing data pattern (\code{FALSE}=missing,
#'\code{TRUE}=observed)
#'@param x Matrix (\code{n} x \code{p}) of complete covariates.
#'@param ... Other named arguments.
#'@return A vector of length \code{nmis} with imputations.
#'@note Using \code{mice.impute.norm} for all columns is similar to Schafer's
#'NORM method (Schafer, 1997).
#'@author Stef van Buuren, Karin Groothuis-Oudshoorn, 2000
#'@references Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}:
#'Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#'Statistical Software}, \bold{45}(3), 1-67.
#'\url{http://www.jstatsoft.org/v45/i03/}
#'
#'Brand, J.P.L. (1999) \emph{Development, implementation and evaluation of
#'multiple imputation strategies for the statistical analysis of incomplete
#'data sets.} Dissertation. Rotterdam: Erasmus University.
#'
#'Schafer, J.L. (1997). Analysis of incomplete multivariate data. London:
#'Chapman & Hall.
#'@keywords datagen
#'@export
mice.impute.norm <- function(y, ry, x, ...) {
    # Bayesian normal imputation of y given x according to Rubin p. 167
    # x is complete.
    #
    # TNO Quality of Life
    # authors: S. van Buuren and K. Groothuis-Oudshoorn
    x <- cbind(1, as.matrix(x))
    parm <- .norm.draw(y, ry, x, ...)
    return(x[!ry, ] %*% parm$beta + rnorm(sum(!ry)) * parm$sigma)
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
    xtx <- crossprod(xobs)  # SvB 21/01/2014
    pen <- ridge * diag(xtx)
    if (length(pen) == 1) 
        pen <- matrix(pen)
    v <- solve(xtx + diag(pen))
    coef <- t(yobs %*% xobs %*% v)
    residuals <- yobs - xobs %*% coef
    df <- max(sum(ry) - ncol(x), 1)  # SvB 31/10/2012
    sigma.star <- sqrt(sum((residuals)^2)/rchisq(1, df))  # SvB 01/02/2011
    beta.star <- coef + (t(chol(sym(v))) %*% rnorm(ncol(x))) * sigma.star
    parm <- list(coef, beta.star, sigma.star)  # SvB 10/2/2010
    names(parm) <- c("coef", "beta", "sigma")  # SvB 10/2/2010
    return(parm)
}

