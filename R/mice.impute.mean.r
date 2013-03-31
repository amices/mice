# -------------------MICE.IMPUTE.MEAN------------------------------

#'Imputation by the mean
#'
#'Imputes the arithmetic mean of the observed data
#'
#'
#'@param y Incomplete data vector of length \code{n}
#'@param ry Vector of missing data pattern (\code{FALSE}=missing,
#'\code{TRUE}=observed)
#'@param x Matrix (\code{n} x \code{p}) of complete covariates.
#'@param ... Other named arguments.
#'@return A vector of length \code{nmis} with imputations.
#'@section Warning: Imputing the mean of a variable is almost never
#'appropriate.  See Little and Rubin (1987).
#'@author Stef van Buuren, Karin Groothuis-Oudshoorn, 2000
#'@seealso \code{\link{mice}}, \code{\link{mean}}
#'@references Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}:
#'Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#'Statistical Software}, \bold{45}(3), 1-67.
#'\url{http://www.jstatsoft.org/v45/i03/}
#'
#'Little, R.J.A. and Rubin, D.B. (1987). Statistical Analysis with Missing
#'Data.  New York: John Wiley and Sons.
#'@keywords datagen
#'@export
mice.impute.mean <- function(y, ry, x = NULL, ...) # mice.impute.mean
    # 
    # Unconditional mean imputation
{
    return(rep(mean(y[ry]), times = sum(!ry)))
}
