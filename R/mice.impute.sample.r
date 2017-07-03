#'Imputation by simple random sampling
#'
#'Imputes a random sample from the observed \code{y} data
#'
#'This function takes a simple random sample from the observed values in
#'\code{y}, and returns these as imputations.
#'
#'@inheritParams mice.impute.pmm
#'@return Vector with imputed data, same type as \code{y}, and of length 
#'\code{sum(wy)}
#'@author Stef van Buuren, Karin Groothuis-Oudshoorn, 2000, 2017
#'@references van Buuren S and Groothuis-Oudshoorn K (2011). \code{mice}:
#'Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#'Statistical Software}, \bold{45}(3), 1-67.
#'\url{http://www.jstatsoft.org/v45/i03/}
#'@keywords datagen
#'@export
mice.impute.sample <- function(y, ry, x = NULL, wy = NULL, ...)
{
  if (is.null(wy)) 
    wy <- !ry
  yry <- y[ry]
  if (length(yry) < 1) 
    return(rnorm(sum(wy)))
  if (length(yry) == 1) yry <- rep(yry, 2)
  return(sample(yry, size = sum(wy), replace = TRUE))
}
