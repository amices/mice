#' Echoes the package version number
#'
#' @param pkg A character vector with the package name.
#' @return A character vector containing the package name, version number and
#' installed directory.
#' @author Stef van Buuren, Oct 2010
#' @keywords misc
#' @examples
#' version()
#' version("base")
#' @export
version <- function(pkg = "mice") {
  lib <- dirname(system.file(package = pkg))
  d <- packageDescription(pkg)
  return(paste(d$Package, d$Version, d$Date, lib))
}
