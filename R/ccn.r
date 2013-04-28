#'Complete cases n
#'
#'Calculates the number of complete cases. The companion function for 
#'calculating the number of incomplete cases is \code{icn()}.
#'
#'@aliases ccn ccn,data.frame-method ccn,matrix-method ccn,mids-method
#'@param x An \code{R} object. Currently supported are methods for the
#'following classes: \code{mids}, \code{data.frame} and \code{matrix}. In
#'addition, \code{x} can be a vector of any kind.
#'@return An integer with the number of elements in \code{x} with complete
#'data.
#'@author Stef van Buuren, 2010.
#'@seealso \code{\link{icn}}, \code{\link{cc}}, \code{\link{ic}}, 
#'\code{\link{cci}}, \code{\link{ici}}
#'@keywords univar
#'@examples
#'
#'  ccn(nhanes) # 13 complete cases 
#'
#'@docType methods
#'@rdname ccn-methods
#'@export
ccn <- function(x){
    return(sum(cci(x)))
}

#'Incomplete cases n
#'
#'Calculates the number of incomplete cases. The companion function for 
#'calculating the number of complete cases is \code{ccn()}.
#'
#'@aliases icn icn,data.frame-method icn,matrix-method icn,mids-method
#'@param x An \code{R} object. Currently supported are methods for the
#'following classes: \code{mids}, \code{data.frame} and \code{matrix}. In
#'addition, \code{x} can be a vector of any kind.
#'@return An integer with the number of elements in \code{x} with incomplete
#'data.
#'@author Stef van Buuren, 2010.
#'@seealso \code{\link{ccn}}, \code{\link{cc}}, \code{\link{ic}}, 
#'\code{\link{cci}}, \code{\link{ici}}
#'@keywords univar
#'@examples
#'
#'  icn(nhanes) # the remaining 12 rows 
#'  icn(nhanes[,c("bmi","hyp")]) # number of cases with incomplete bmi and hyp
#'
#'@docType methods
#'@rdname icn-methods
#'@export
icn <- function(x){
    return(sum(ici(x)))
}
