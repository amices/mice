#'Complete case indicator
#'
#'
#'This array is useful for extracting the subset of complete cases. The function
#'\code{cci(x)} is equivelant to \code{complete.cases(x)}.
#'Missing values in \code{x} are coded as \code{NA}. The companion function 
#'for selecting the incomplete cases is \code{ici()}.
#'
#'@name cci
#'@aliases cci cci,data.frame-method cci,matrix-method cci,mids-method
#'@param x An \code{R} object. Currently supported are methods for the
#'following classes: \code{mids}, \code{data.frame} and \code{matrix}. In
#'addition, \code{x} can be a vector of any kind.
#'@return Logical vector indicating the complete cases,
#'. If \code{x} is a \code{data.frame} or 
#'\code{matrix} the length is \code{nrow(x)}. In other cases, 
#'the length is \code{length(x)}.
#'@author Stef van Buuren, 2010.
#'@seealso \code{\link{complete.cases}}, \code{\link{ici}} 
#'\code{\link{cc}}, \code{\link{ic}}, \code{\link{ccn}}, 
#'\code{\link{icn}}
#'@keywords univar
#'@examples
#'
#'  cci(nhanes) # indicator for 13 complete cases 
#'  f <- cci(nhanes[,c("bmi","hyp")]) # complete data for bmi and hyp
#'  nhanes[f,] # obtain all data from those with complete bmi and hyp
#'
#'@docType methods
#'@rdname cci-methods
#'@export
setGeneric("cci",
           def = function(x) standardGeneric("cci"),
           package = "mice",
           useAsDefault = function(x) return(!is.na(x)))
setMethod("cci", signature(x="mids"),
          function(x) return(apply(!is.na(x$data),1,all)))
#setMethod("cci", signature(x="mi"),
#          function(x) return(apply(!is.na(x@data),1,all)))
setMethod("cci", signature(x="data.frame"),
          function(x) return(apply(!is.na(x),1,all)))
setMethod("cci", signature(x="matrix"),
          function(x) return(apply(!is.na(x),1,all)))


#'Incomplete case indicator
#'
#'This array is useful for extracting the subset of incomplete cases.
#'Missing values in \code{x} are coded as \code{NA}. The companion function 
#'for selecting the complete cases is \code{cci()}.
#'
#'@name ici
#'@aliases ici ici,data.frame-method ici,matrix-method ici,mids-method
#'@param x An \code{R} object. Currently supported are methods for the
#'following classes: \code{mids}, \code{data.frame} and \code{matrix}. In
#'addition, \code{x} can be a vector of any kind.
#'@return Logical vector indicating the incomplete cases,
#'. If \code{x} is a \code{data.frame} or 
#'\code{matrix} the length is \code{nrow(x)}. In other cases, 
#'the length is \code{length(x)}.
#'@author Stef van Buuren, 2010.
#'@seealso \code{\link{cci}} 
#'\code{\link{cc}}, \code{\link{ic}}, \code{\link{ccn}}, 
#'\code{\link{icn}}
#'@keywords univar
#'@examples
#'
#'  ici(nhanes) # indicator for 12 rows with incomplete cases 
#'
#'@docType methods
#'@rdname ici-methods
#'@export
setGeneric("ici",
           def = function(x) standardGeneric("ici"),
           package = "mice",
           useAsDefault = function(x) return(is.na(x)))
setMethod("ici", signature(x="mids"),
          function(x) return(apply(is.na(x$data),1,any)))
#setMethod("ici", signature(x="mi"),
#          function(x) return(apply(is.na(x@data),1,any)))
setMethod("ici", signature(x="data.frame"),
          function(x) return(apply(is.na(x),1,any)))
setMethod("ici", signature(x="matrix"),
          function(x) return(apply(is.na(x),1,any)))
