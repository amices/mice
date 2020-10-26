#' Creates a \code{post} argument
#'
#' This helper function creates a valid \code{post} vector. The
#' \code{post} vector is an argument to the \code{mice} function that
#' specifies post-processing for a variable just after imputation.
#' @inheritParams mice
#' @return Character vector of \code{ncol(data)} element
#' @seealso \code{\link{mice}}
#' @examples
#' make.post(nhanes2)
#' @export
make.post <- function(data) {
  post <- vector("character", length = ncol(data))
  names(post) <- colnames(data)
  post
}

check.post <- function(post, data) {
  if (is.null(post)) {
    return(make.post(data))
  }

  # check
  if (length(post) != ncol(data)) {
    stop("length(post) does not match ncol(data)", call. = FALSE)
  }

  # change
  if (is.null(names(post))) names(post) <- colnames(data)

  post
}
