#' Check for `mids` object
#'
#' @aliases is.mids
#' @param x An object
#' @return A logical indicating whether `x` is an object of class `mids`
#' @export
is.mids <- function(x) {
  inherits(x, "mids")
}

#' Check for `mira` object
#'
#' @aliases is.mira
#' @param x An object
#' @return A logical indicating whether `x` is an object of class `mira`
#' @export
is.mira <- function(x) {
  inherits(x, "mira")
}


#' Check for `mipo` object
#'
#' @aliases is.mipo
#' @param x An object
#' @return A logical indicating whether `x` is an object of class `mipo`
#' @export
is.mipo <- function(x) {
  inherits(x, "mipo")
}


#' Check for `mitml.result` object
#'
#' @aliases is.mitml.result
#' @param x An object
#' @return A logical indicating whether `x` is an object of class `mitml.result`
#' @export
is.mitml.result <- function(x) {
  inherits(x, "mitml.result")
}


is.passive <- function(string) {
  "~" == substring(string, 1, 1)
}


#' Check for `mads` object
#'
#' @aliases is.mads
#' @param x An object
#' @return A logical indicating whether `x` is an object of class `mads`
#' @export
is.mads <- function(x) {
  inherits(x, "mads")
}
