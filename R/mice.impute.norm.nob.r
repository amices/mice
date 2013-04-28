# ------------------------MICE.IMPUTE.NORM.NOB-----------------------

#'Imputation by linear regression (non Bayesian)
#'
#'Imputes univariate missing data using linear regression analysis (non
#'Bayesian version)
#'
#'This creates imputation using the spread around the fitted linear regression
#'line of \code{y} given \code{x}, as fitted on the observed data.
#'
#'@aliases mice.impute.norm.nob norm.nob
#'@param y Incomplete data vector of length \code{n}
#'@param ry Vector of missing data pattern (\code{FALSE}=missing,
#'\code{TRUE}=observed)
#'@param x Matrix (\code{n} x \code{p}) of complete covariates.
#'@param ... Other named arguments.
#'@return A vector of length \code{nmis} with imputations.
#'@note This function is provided mainly to allow comparison between proper and
#'improper norm methods. Also, it may be useful to impute large data containing
#'many rows.
#'@section Warning: The function does not incorporate the variability of the
#'regression weights, so it is not 'proper' in the sense of Rubin. For small
#'samples, variability of the imputed data is therefore underestimated.
#'@author Stef van Buuren, Karin Groothuis-Oudshoorn, 2000
#'@seealso \code{\link{mice}}, \code{\link{mice.impute.norm}}
#'@references Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}:
#'Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#'Statistical Software}, \bold{45}(3), 1-67.
#'\url{http://www.jstatsoft.org/v45/i03/}
#'
#'Brand, J.P.L. (1999). Development, Implementation and Evaluation of Multiple
#'Imputation Strategies for the Statistical Analysis of Incomplete Data Sets.
#'Ph.D. Thesis, TNO Prevention and Health/Erasmus University Rotterdam.
#'@keywords datagen
#'@export
mice.impute.norm.nob <- function(y, ry, x, ...) {
    # Regression imputation of y given x, with a fixed regression line, and with random draws of the
    # residuals around the line.  x is complete.
    x <- cbind(1, as.matrix(x))
    parm <- .norm.fix(y, ry, x, ...)
    return(x[!ry, ] %*% parm$beta + rnorm(sum(!ry)) * parm$sigma)
}

# --------------------------------.NORM.FIX-----------------------------
.norm.fix <- function(y, ry, x, ridge = 1e-05, ...) {
    # .norm.fix Calculates regression coefficients + error estimate
    # 
    # TNO Quality of Life authors: S. van Buuren and K. Groothuis-Oudshoorn
    xobs <- x[ry, ]
    yobs <- y[ry]
    xtx <- t(xobs) %*% xobs
    pen <- ridge * diag(xtx)
    if (length(pen) == 1) 
        pen <- matrix(pen)
    v <- solve(xtx + diag(pen))
    coef <- t(yobs %*% xobs %*% v)
    residuals <- yobs - xobs %*% coef
    sigma <- sqrt((sum(residuals^2))/(sum(ry) - ncol(x) - 1))
    parm <- list(coef, sigma)
    names(parm) <- c("beta", "sigma")
    return(parm)
}
