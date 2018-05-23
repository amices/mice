
# --------------------------------IS.MIDS--------------------------------------

#' Check for \code{mids} object
#' 
#' @aliases is.mids
#' @param x An object
#' @return A logical indicating whether \code{x} is an object of class \code{mids}
#' @export
is.mids <- function(x) {
    inherits(x, "mids")
}


# --------------------------------IS.MIRA--------------------------------------

#' Check for \code{mira} object
#' 
#' @aliases is.mira
#' @param x An object
#' @return A logical indicating whether \code{x} is an object of class \code{mira}
#' @export
is.mira <- function(x) {
    inherits(x, "mira")
}


# --------------------------------IS.MIPO--------------------------------------

#' Check for \code{mipo} object
#' 
#' @aliases is.mipo
#' @param x An object
#' @return A logical indicating whether \code{x} is an object of class \code{mipo}
#' @export
is.mipo <- function(x) {
    inherits(x, "mipo")
}

#' Check for \code{mitml.result} object
#' 
#' @aliases is.mitml.result
#' @param x An object
#' @return A logical indicating whether \code{x} is an object of class \code{mitml.result}
#' @export
is.mitml.result <- function(x) {
  inherits(x, "mitml.result")
}


# ------------------------------is.passive------------------------------------

is.passive <- function(string) {
    return("~" == substring(string, 1, 1))
}

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


