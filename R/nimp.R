#' Number of imputations per block
#'
#' Calculates the number of cells within a block for which imputation
#' is requested.
#' @inheritParams mice
#' @return  A numeric vector of length \code{length(blocks)} containing
#' the number of cells that need to be imputed within a block.
#' @seealso \code{\link{mice}}
#' @export
#' @examples
#' where <- is.na(nhanes)
#'
#' # standard FCS
#' nimp(where)
#'
#' # user-defined blocks
#' nimp(where, blocks = name.blocks(list(c("bmi", "hyp"), "age", "chl")))
nimp <- function(where, blocks = make.blocks(where)) {
  nwhere <- apply(where, 2, sum)
  nimp <- vector("integer", length = length(blocks))
  names(nimp) <- names(blocks)
  for (i in seq_along(blocks)) nimp[i] <- sum(nwhere[blocks[[i]]])
  nimp
}
