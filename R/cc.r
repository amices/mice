#'Complete cases
#'
#'Extracting complete cases is also known as 'listwise deletion' or 
#''complete case analyses'. \code{cc(x)} is equivalent to
#'\code{na.omit(x)}. Missing values in \code{x} are coded as \code{NA}.
#'The companion function for selecting the incomplete cases is \code{ic()}.
#'
#'@aliases cc,data.frame-method cc,matrix-method cc,mids-method
#'@param x An \code{R} object. Currently supported are methods for the
#'following classes: \code{mids}, \code{mira}, \code{mipo}, \code{data.frame}
#'and \code{matrix}. In addition, \code{x} can be a vector of any kind.
#'@param drop A logical flag for matrices and arrays. If \code{drop=TRUE} the
#'result is coerced to the lowest possible dimension.
#'@return A \code{vector}, \code{matrix} of \code{data.frame} containing the data of the complete
#'cases.
#'@author Stef van Buuren, 2010.
#'@seealso \code{\link{na.omit}}, \code{\link{ic}}, \code{\link{cci}}, \code{\link{ici}},
#'\code{\link{ccn}}, \code{\link{icn}}
#'@keywords univar
#'@examples
#'
#'cc(nhanes)   # get the 13 complete cases
#'cc(nhanes[,2,drop=FALSE], drop=FALSE) # extract complete bmi as column
#'
#'@docType methods
#'@rdname cc-methods
#'@export
setGeneric("cc",
           def = function(x, drop=TRUE) standardGeneric("cc"),
           package = "mice",
           useAsDefault = function(x, drop) return(x[cci(x)]))
#setMethod("cc", signature(x="mids"),
#          function(x, drop) return(x$data[cci(x),,drop=drop]))
#setMethod("cc", signature(x="mi"),
#          function(x, drop) return(x@data[cci(x),,drop=drop]))
setMethod("cc", signature(x="data.frame"),
          function(x, drop) return(x[cci(x),,drop=drop]))
setMethod("cc", signature(x="matrix"),
          function(x, drop) return(x[cci(x),,drop=drop]))

#'Incomplete cases
#'
#'Extracts incomplete cases from a data set. Missing values in \code{x} are coded 
#'as \code{NA}.
#'The companion function for selecting the complete cases is \code{cc()}.
#'
#'@aliases ic,data.frame-method ic,matrix-method ic,mids-method
#'@param x An \code{R} object. Currently supported are methods for the
#'following classes: \code{mids}, \code{mira}, \code{mipo}, \code{data.frame}
#'and \code{matrix}. In addition, \code{x} can be a vector of any kind.
#'@param drop A logical flag for matrices and arrays. If \code{drop=TRUE} the
#'result is coerced to the lowest possible dimension.
#'@return A \code{vector}, \code{matrix} of \code{data.frame} containing the data of the incomplete
#'cases.
#'@author Stef van Buuren, 2010.
#'@seealso \code{\link{na.omit}}, \code{\link{cc}}, \code{\link{cci}}, \code{\link{ici}},
#'\code{\link{ccn}}, \code{\link{icn}}
#'@keywords univar
#'@examples
#'
#'ic(nhanes)   # get the 12 rows with incomplete cases 
#'ic(nhanes[1:10,])  # incomplete cases within the first ten rows
#'ic(nhanes[,2:3])  # restrict extraction to variables bmi and hyp
#'
#'@docType methods
#'@rdname ic-methods
#'@export
setGeneric("ic",
           def = function(x, drop=TRUE) standardGeneric("ic"),
           package = "mice",
           useAsDefault = function(x, drop) return(x[ici(x)]))
#setMethod("ic", signature(x="mids"),
#          function(x, drop) return(x$data[ici(x),,drop=drop]))
#setMethod("ic", signature(x="mi"),
#          function(x, drop) return(x@data[ici(x),,drop=drop]))
setMethod("ic", signature(x="data.frame"),
          function(x, drop) return(x[ici(x),,drop=drop]))
setMethod("ic", signature(x="matrix"),
          function(x, drop) return(x[ici(x),,drop=drop]))

