#
# --------------------------------PRINT.MADS--------------------------------------
#
#'Print a \code{mads} object
#'
#'@rdname print
#'@param x Object of class \code{mads}
#'@return \code{NULL}
#'@export
print.mads <- function(x, ...) {
  if (is.mads(x)) {
    cat("Multivariately Amputed Data Set")
    cat("\nCall: ")
    print(x$call)
    cat("Proportion of Missingness: ", x$prop)
    cat("\nFrequency of Patterns: ", x$freq)
    cat("\nClass:", class(x))
    cat("\nPattern Matrix:\n")
    print(x$patterns)
    cat("Weight Matrix:\n")
    print(x$weights)
    cat("Odds Matrix:\n")
    print(x$odds)
    cat("Head of Amputed Data Set\n")
    print(head(x$amp))
  } else print(x, ...)
  invisible()
}