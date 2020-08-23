#' Supports semi-transparent foreground colors?
#'
#' This function is used by \code{mdc()} to find out whether the current device
#' supports semi-transparent foreground colors.
#'
#' The function calls the function \code{dev.capabilities()} from the package
#' \code{grDevices}. The function return \code{FALSE} if the status of the
#' current device is unknown.
#'
#' @aliases supports.transparent transparent
#' @return \code{TRUE} or \code{FALSE}
#' @seealso \code{\link{mdc}} \code{\link{dev.capabilities}}
#' @keywords hplot
#' @examples
#'
#' supports.transparent()
#' @export
supports.transparent <- function() {
  query <- grDevices::dev.capabilities("semiTransparency")$semiTransparency
  if (is.na(query)) {
    query <- FALSE
  }
  query
}
