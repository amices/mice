#' Creates a `dots` argument
#'
#' This helper function creates a valid `dots` object. The
#' `dots` object is an argument to the `mice` function.
#' The name `dots` is a contraction of blocks-dots.
#' Through `dots`, the user can specify any additional
#' arguments that are specifically passed down to the lowest level
#' imputation function.
#' @param data A `data.frame` with the source data
#' @param blocks An optional specification for blocks of variables in
#' the rows. The default assigns each variable in its own block.
#' @return A matrix
#' @seealso [make.blocks()]
#' @examples
#' make.predictorMatrix(nhanes)
#' make.dots(nhanes, blocks = name.blocks(c("age", "hyp"), "xxx"))
#' @export
make.dots <- function(data, blocks = make.blocks(data)) {
  data <- check.dataform(data)
  dots <- vector("list", length(blocks))
  for (i in seq_along(dots)) dots[[i]] <- alist()
  names(dots) <- names(blocks)
  dots
}

check.dots <- function(dots, data, blocks = NULL) {
  data <- check.dataform(data)

  if (is.null(dots)) {
    return(make.dots(data, blocks))
  }

  dots <- as.list(dots)
  for (i in seq_along(dots)) dots[[i]] <- as.list(dots[[i]])

  if (length(dots) == length(blocks) && is.null(names(dots))) {
    names(dots) <- names(blocks)
  }
  dots
}
