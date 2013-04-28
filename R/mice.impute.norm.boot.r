### ------------------------MICE.IMPUTE.NORM.BOOT----------------------

#'Imputation by linear regression, bootstrap method
#'
#'Imputes univariate missing data using linear regression with boostrap
#'
#'Draws a bootstrap sample from \code{x[ry,]} and \code{y[ry]}, calculates
#'regression weights and imputes with normal residuals. The \code{ridge}
#'parameter adds a penalty term \code{ridge*diag(xtx)} to the
#'variance-covariance matrix \code{xtx}.
#'
#'@aliases mice.impute.norm.boot norm.boot
#'@param y Incomplete data vector of length \code{n}
#'@param ry Vector of missing data pattern (\code{FALSE}=missing,
#'\code{TRUE}=observed)
#'@param x Matrix (\code{n} x \code{p}) of complete covariates.
#'@param ridge Ridge parameter
#'@param ... Other named arguments.
#'@return A vector of length \code{nmis} with imputations.
#'@author Stef van Buuren, 2011
#'@references Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}:
#'Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#'Statistical Software}, \bold{45}(3), 1-67.
#'\url{http://www.jstatsoft.org/v45/i03/}
#'@keywords datagen
#'@export
mice.impute.norm.boot <- function(y, ry, x, ridge = 1e-05, ...) {
    # mice.impute.norm.boot
    # Regression imputations of y given x, with a fixed regression
    # line, and with random draws of the residuals around the line.
    # bootstrap version    
    x <- cbind(1, as.matrix(x))
    xobs <- x[ry, ]
    yobs <- y[ry]
    n1 <- sum(ry)
    n0 <- sum(!ry)
    s <- sample(n1, n1, replace = TRUE)
    dotxobs <- xobs[s, ]
    dotyobs <- yobs[s]
    xtx <- t(dotxobs) %*% dotxobs
    pen <- ridge * diag(xtx)
    if (length(pen) == 1) 
        pen <- matrix(pen)
    v <- solve(xtx + diag(pen))
    coef <- t(dotyobs %*% dotxobs %*% v)
    residuals <- dotyobs - dotxobs %*% coef
    sigma <- sqrt((sum(residuals^2))/(n1 - ncol(x) - 1))
    parm <- list(coef, sigma)
    names(parm) <- c("beta", "sigma")
    return(x[!ry, ] %*% parm$beta + rnorm(n0) * parm$sigma)
}
