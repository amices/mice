#' Creates a `blots` argument
#'
#' This helper function creates a valid `blots` object. The
#' `blots` object is an argument to the `mice` function.
#' The name `blots` is a contraction of blocks-dots.
#' Through `blots`, the user can specify any additional
#' arguments that are specifically passed down to the lowest level
#' imputation function.
#' @param data A `data.frame` with the source data
#' @param blocks An optional specification for blocks of variables in
#' the rows. The default assigns each variable in its own block.
#' @return A matrix
#' @seealso [make.blocks()]
#' @examples
#' make.predictorMatrix(nhanes)
#' make.blots(nhanes, blocks = name.blocks(c("age", "hyp"), "xxx"))
#' @export
make.blots <- function(data, blocks = make.blocks(data)) {
  data <- check.dataform(data)
  blots <- vector("list", length(blocks))
  for (i in seq_along(blots)) blots[[i]] <- alist()
  names(blots) <- names(blocks)
  blots
}

check.blots <- function(blots, data, blocks = NULL) {
  data <- check.dataform(data)

  if (is.null(blots)) {
    return(make.blots(data, blocks))
  }

  blots <- as.list(blots)
  for (i in seq_along(blots)) blots[[i]] <- as.list(blots[[i]])

  if (length(blots) == length(blocks) && is.null(names(blots))) {
    names(blots) <- names(blocks)
  }
  blots
}
