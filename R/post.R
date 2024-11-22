#' Creates a `post` argument
#'
#' This helper function creates a valid `post` vector. The
#' `post` vector is an argument to the `mice` function that
#' specifies post-processing for a variable after each iteration of imputation.
#' @inheritParams mice
#' @return Character vector of `ncol(data)` element
#' @seealso [mice()]
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
