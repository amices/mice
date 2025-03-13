#' Creates a \code{tasks} argument
#'
#' This helper function creates a valid \code{tasks} vector. The
#' \code{tasks} vector is an argument to the \code{mice} function that
#' specifies the task for each column in the data.
#' @inheritParams mice
#' @return Character vector of \code{ncol(data)} elements
#' @seealso \code{\link{mice}}
#' @examples
#' make.tasks(nhanes2)
#' @export
make.tasks <- function(data,
                       tasks = "generate",
                       blocks = make.blocks(data)) {
  bv <- unique(unlist(blocks))
  if (length(tasks) == 1L) {
    tasks <- setNames(rep(tasks, length(bv)), bv)
  } else {
    if (length(tasks) != length(bv)) {
      stop("length(tasks) does not match variables to be imputed", call. = FALSE)
    }
    names(tasks) <- bv
  }

  return(tasks)
}
