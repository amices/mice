#'Complete case indicator
#'
#'
#'The complete case indicator is useful for extracting the subset of complete cases. The function
#'\code{cci(x)} calls \code{complete.cases(x)}. 
#'The companion function \code{ici()} selects the incomplete cases.
#'
#'@name cci
#'@param x An \code{R} object. Currently supported are methods for the
#'following classes: \code{mids}. 
#'@return Logical vector indicating the complete cases.
#'@author Stef van Buuren, 2017.
#'@seealso \code{\link{complete.cases}}, \code{\link{ici}}, \code{\link{cc}}
#'@keywords univar
#'@examples
#' cci(nhanes) # indicator for 13 complete cases 
#' cci(mice(nhanes, maxit = 0))
#' f <- cci(nhanes[,c("bmi","hyp")]) # complete data for bmi and hyp
#' nhanes[f,] # obtain all data from those with complete bmi and hyp
#'@export
cci <- function (x) UseMethod("cci", x)

#' @export
cci.mids <- function(x) return(complete.cases(x$data))

#' @export
cci.default <- function(x) return(complete.cases(x))

#'Incomplete case indicator
#'
#'This array is useful for extracting the subset of incomplete cases.
#'The companion function  \code{cci()} selects the complete cases.
#'
#'@name ici
#'@aliases ici ici,data.frame-method ici,matrix-method ici,mids-method
#'@param x An \code{R} object. Currently supported are methods for the
#'following classes: \code{mids}.
#'@return Logical vector indicating the incomplete cases,
#'@author Stef van Buuren, 2017.
#'@seealso \code{\link{cci}}, \code{\link{ic}}
#'@keywords univar
#'@examples
#'
#'  ici(nhanes) # indicator for 12 rows with incomplete cases 
#'
#'@export
ici <- function(x) UseMethod("ici", x)

#' @export
ici.mids <- function(x) return(!complete.cases(x$data))

#' @export
ici.default <- function(x) return(!complete.cases(x))
