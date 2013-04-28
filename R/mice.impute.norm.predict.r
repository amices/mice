### ------------------------MICE.IMPUTE.NORM.PREDICT--------------------

#'Imputation by linear regression, prediction method
#'
#'Imputes univariate missing data using the predicted value from a linear
#'regression
#'
#'Calculates regression weights from the observed data and and return predicted
#'values to as imputations. The \code{ridge} parameter adds a penalty term
#'\code{ridge*diag(xtx)} to the variance-covariance matrix \code{xtx}.
#'
#'@aliases mice.impute.norm.predict norm.predict
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
mice.impute.norm.predict <- function(y, ry, x, ridge = 1e-05, ...) {
    # mice.impute.norm.predict
    # Regression imputation of y given x, with a fixed regression
    # line, takes imputation from the regression line
    # prediction method    
    x <- cbind(1, as.matrix(x))
    xobs <- x[ry, ]
    yobs <- y[ry]
    xtx <- t(xobs) %*% xobs
    pen <- ridge * diag(xtx)
    if (length(pen) == 1) 
        pen <- matrix(pen)
    v <- solve(xtx + diag(pen))
    coef <- t(yobs %*% xobs %*% v)
    return(x[!ry, ] %*% coef)
}
