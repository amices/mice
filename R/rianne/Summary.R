#
# ------------------------------summary.mads-------------------------------
#
#'Summary of a \code{mads} object
#'
#'@rdname summary
#'@param object A \code{mads} object
#'@return \code{NULL}
#'@export
summary.mads <- function(x, ...) {
  if (is.mads(x)) {
    print(x, ...)
  } else {
    summary.default(x)
  }
}