#'Number of complete cases
#'
#'Calculates the number of complete cases.
#'
#'@param x An \code{R} object. Currently supported are methods for the
#'following classes: \code{mids}, \code{data.frame} and \code{matrix}. Also, 
#'\code{x} can be a vector.
#'@return Number of elements in \code{x} with complete data.
#'@author Stef van Buuren, 2017
#'@seealso \code{\link{nic}}, \code{\link{cci}}
#'@examples
#'
#'  ncc(nhanes) # 13 complete cases 
#'
#'@export
ncc <- function(x) sum(cci(x))

#'Number of incomplete cases
#'
#'Calculates the number of incomplete cases.
#'
#'@param x An \code{R} object. Currently supported are methods for the
#'following classes: \code{mids}, \code{data.frame} and \code{matrix}. Also, 
#'\code{x} can be a vector.
#'@return Number of elements in \code{x} with incomplete data.
#'@author Stef van Buuren, 2017
#'@seealso \code{\link{ncc}}, \code{\link{cci}}
#'@examples
#'
#'  nic(nhanes) # the remaining 12 rows 
#'  nic(nhanes[,c("bmi","hyp")]) # number of cases with incomplete bmi and hyp
#'
#'@export
nic <- function(x) sum(ici(x))
