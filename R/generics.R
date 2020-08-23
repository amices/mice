#' Combine R objects by rows and columns
#'
#' Functions \code{cbind()} and \code{rbind()} are defined in
#' the \code{mice} package in order to
#' enable dispatch to \code{cbind.mids()} and \code{rbind.mids()}
#' when one of the arguments is a \code{data.frame}.
#'
#' The standard \code{base::cbind()} and \code{base::rbind()}
#' always dispatch to
#' \code{base::cbind.data.frame()} or \code{base::rbind.data.frame()}
#' if one of the arguments is a
#' \code{data.frame}. The versions defined in the \code{mice}
#' package intercept the user command
#' and test whether the first argument has class \code{"mids"}. If so,
#' function calls \code{cbind.mids()}, respectively \code{rbind.mids()}. In
#' all other cases, the call is forwarded to standard functions in the
#' \code{base} package.
#'
#' @inheritParams base::cbind
#' @seealso \code{\link[base:cbind]{cbind}}, \code{\link[base:cbind]{rbind}},
#' \code{\link{cbind.mids}}, \code{\link{rbind.mids}}
#' @keywords internal
#' @export
cbind <- function(...) {
  if (is.null(attr(list(...)[[1]], "class"))) {
    return(base::cbind(...))
  }
  if ("mids" %in% attr(list(...)[[1]], "class")) {
    cbind.mids(...)
  } else {
    base::cbind(...)
  }
}

#' @rdname cbind
#' @export
rbind <- function(...) {
  if (is.null(attr(list(...)[[1]], "class"))) {
    return(base::rbind(...))
  }
  if ("mids" %in% attr(list(...)[[1]], "class")) {
    rbind.mids(...)
  } else {
    base::rbind(...)
  }
}
