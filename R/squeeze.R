#' Squeeze the imputed values to be within specified boundaries.
#'
#' This function replaces any values in `x` that are lower than
#' `bounds[1]` by `bounds[1]`, and replaces any values higher
#' than `bounds[2]` by `bounds[2]`.
#'
#' @aliases squeeze
#' @param x A numerical vector with values
#' @param bounds A numerical vector of length 2 containing the lower and upper bounds.
#' By default, the bounds are to the minimum and maximum values in `x`.
#' @param r A logical vector of length `length(x)` that is used to select a
#' subset in `x` before calculating automatic bounds.
#' @return A vector of length `length(x)`.
#' @author Stef van Buuren, 2011.
#' @export
squeeze <- function(x, bounds = c(min(x[r]), max(x[r])),
                    r = rep.int(TRUE, length(x))) {
  if (length(r) != length(x)) {
    stop("Different length of vectors x and r")
  }
  x[x < bounds[1]] <- bounds[1]
  x[x > bounds[2]] <- bounds[2]
  x
}
