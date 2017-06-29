#'Imputation by simple random sampling
#'
#'Imputes a random sample from the observed \code{y} data
#'
#'This function takes a simple random sample from the observed values in
#'\code{y}, and returns these as imputations.
#'
#'@param y Incomplete data vector
#'@param ry Vector of missing data pattern of \code{y}
#'@param x Matrix with \code{length(y)} rows with complete covariates. Not used.
#'@param wy Logical vector of length \code{length(y)} elements indicating 
#'observation for which imputations should be created
#'@param ... Other named arguments. Not used.
#'@return A vector of length \code{sum(wy)} with imputations
#'@author Stef van Buuren, Karin Groothuis-Oudshoorn, 2000, 2017
#'@references van Buuren S and Groothuis-Oudshoorn K (2011). \code{mice}:
#'Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#'Statistical Software}, \bold{45}(3), 1-67.
#'\url{http://www.jstatsoft.org/v45/i03/}
#'@keywords datagen
#'@export
mice.impute.sample <- function(y, ry, x = NULL, wy = NULL, ...)
{
    yry <- y[ry]
    if (length(yry) < 1) return(rnorm(sum(wy)))
    if (length(yry) == 1) yry <- rep(yry, 2)
    return(sample(yry, size = sum(wy), replace = TRUE))
}
