#' Creates a \code{visitSequence} argument
#'
#' This helper function creates a valid \code{visitSequence}. The 
#' \code{visitSequence} is an argument to the \code{mice} function that 
#' specifies the sequence in which blocks are imputed.
#' @inheritParams mice
#' @return Vector containing block names
#' @seealso \code{\link{mice}}
#' @examples
#' make.visitSequence(nhanes)
#' @export
make.visitSequence <- function(data = NULL, blocks = NULL) {
  
  if (!is.null(blocks)) {
    blocks <- name.blocks(blocks)
    return(names(blocks))
  }
  
  if (!(is.matrix(data) || is.data.frame(data)))
    stop("Data should be a matrix or data frame", call. = FALSE)
  if (ncol(data) < 2)
    stop("Data should contain at least two columns", call. = FALSE)
  blocks <- make.blocks(data)
  names(blocks)
}

check.visitSequence <- function(visitSequence = NULL, blocks, 
                                data, where = NULL) {
  
  if (is.null(names(blocks)) || any(is.na(names(blocks))))
    stop("Missing blocks names.", call. = FALSE)
  
  if (is.null(visitSequence)) return(make.visitSequence(data, blocks))

  if (is.null(where)) where <- is.na(data)
  nimp <- nimp(where, blocks)
  if (length(nimp) == 0) visitSequence <- nimp
  
  if (length(visitSequence) == 1 && is.character(visitSequence)) {
    code <- match.arg(visitSequence, c("roman", "arabic", "monotone",
                                       "revmonotone"))
    visitSequence <- switch(
      code, 
      roman = names(blocks)[nimp > 0],
      arabic = rev(names(blocks)[nimp > 0]),
      monotone = names(blocks)[order(nimp)],
      revmonotone = rev(names(blocks)[order(nimp)])
    )
  }
  
  # legacy handling
  if (is.numeric(visitSequence)) 
    visitSequence <- colnames(data)[visitSequence]

  # check against names(blocks)
  remove <- (nimp == 0) & is.element(names(blocks), visitSequence)
  remove <- remove | !is.element(names(blocks), visitSequence)
  visitSequence <- visitSequence[!remove]
  visitSequence
}

