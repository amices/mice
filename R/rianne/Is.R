#
# --------------------------------IS.MADS--------------------------------------
#
#' Check for \code{mads} object
#' 
#' @aliases is.mads
#' @param x An object
#' @return A logical indicating whether \code{x} is an object of class \code{mads}
#' @export
is.mads <- function(x) {
  inherits(x, "mads")
}

