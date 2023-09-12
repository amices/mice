#' Number of imputations per block
#'
#' Calculates the number of cells within a block for which imputation
#' is requested.
#' @inheritParams mice
#' @return  A numeric vector of length `length(blocks)` containing
#' the number of cells that need to be imputed within a block.
#' @seealso [mice()]
#' @export
#' @examples
#' # standard FCS
#' nimp(nhanes2)
#'
#' # user-defined blocks
#' where <- is.na(nhanes)
#' blocks <- list(c("bmi", "hyp"), "age", "chl")
#' nimp(where = where, blocks = blocks)
nimp <- function(data = NULL, where = is.na(data), blocks = make.blocks(where)) {
  # legacy handling
  waswhere <- is.matrix(data) && all(is.logical(data))
  if (waswhere) {
    stop("Please call 'nimp()' as 'nimp(where = .. , blocks = ..'")
  }

  nwhere <- apply(where, 2, sum)
  nimp <- vector("integer", length = length(blocks))
  names(nimp) <- names(blocks)
  for (i in seq_along(blocks)) nimp[i] <- sum(nwhere[blocks[[i]]])
  return(nimp)
}
