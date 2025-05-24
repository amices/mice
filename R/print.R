

#' Print a \code{mira} object
#'
#' @rdname print
#' @param x An object of class \code{mira}
#' @param \dots Other arguments
#' @return \code{NULL}
#' @seealso \code{\link{mira}}
#' @method print mira
#' @export
print.mira <- function(x, ...) {
  if (is.mira(x)) {
    print.listof(x, ...)
  } else {
    print(x, ...)
  }
  invisible(x)
}


#' Print a \code{mice.anova} object
#'
#' @rdname print
#' @param x An object of class \code{mice.anova}
#' @param \dots Other arguments
#' @return \code{NULL}
#' @seealso \code{\link{mipo}}
#' @method print mice.anova
#' @export
print.mice.anova <- function(x, ...) {
  z <- summary(x, ...)
  print(z$comparisons, row.names = FALSE)
  invisible(x)
}


#' Print a \code{summary.mice.anova} object
#'
#' @rdname print
#' @return \code{NULL}
#' @seealso \code{\link{mipo}}
#' @method print mice.anova.summary
#' @export
print.mice.anova.summary <- function(x, ...) {
  cat("\nModels:\n")
  print(x$models, row.names = FALSE)
  cat("\nComparisons:\n")
  print(x$comparisons, row.names = FALSE)
  cat(
    "\nNumber of imputations: ", x$m,
    "  Method", x$method
  )
  if (x$method == "D2") cat(" (", x$use, ")", sep = "")
  cat("\n")
  invisible(x)
}
