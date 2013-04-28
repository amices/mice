# -------------------MICE.IMPUTE.SAMPLE----------------------------

#'Imputation by simple random sampling
#'
#'Imputes a random sample from the observed \code{y} data
#'
#'This function takes a simple random sample from the observed values in
#'\code{y}, and returns these as imputations.
#'
#'@param y Incomplete data vector of length \code{n}
#'@param ry Vector of missing data pattern (\code{FALSE}=missing,
#'\code{TRUE}=observed)
#'@param x Matrix (\code{n} x \code{p}) of complete covariates.
#'@param ... Other named arguments.
#'@return A vector of length \code{nmis} with imputations.
#'@author Stef van Buuren, Karin Groothuis-Oudshoorn, 2000
#'@references van Buuren S and Groothuis-Oudshoorn K (2011). \code{mice}:
#'Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#'Statistical Software}, \bold{45}(3), 1-67.
#'\url{http://www.jstatsoft.org/v45/i03/}
#'@keywords datagen
#'@export
mice.impute.sample <- function(y, ry, x = NULL, ...) # mice.impute.sample
    # 
    # Generates random sample from the observed y's
{
    yry <- y[ry]  ## bug fixed 24jun2012
    if (length(yry) <= 1) 
        return(rnorm(sum(!ry)))
    return(sample(yry, size = sum(!ry), replace = TRUE))
}
