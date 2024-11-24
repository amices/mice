

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


#' Print a \code{mads} object
#'
#' @param x Object of class \code{mads}
#' @param ... Other parameters passed down to \code{print.default()}
#' @return \code{NULL}
#' @seealso \code{\link[=mads-class]{mads}}
#' @method print mads
#' @export
print.mads <- function(x, ...) {
  if (is.mads(x)) {
    cat("Multivariate Amputed Data Set")
    cat("\nCall: ")
    print(x$call)
    cat("Class:", class(x))
    cat("\nProportion of Missingness: ", x$prop)
    cat("\nFrequency of Patterns: ", x$freq)
    cat("\nPattern Matrix:\n")
    print(x$patterns)
    cat("Mechanism:")
    print(x$mech)
    cat("Weight Matrix:\n")
    print(x$weights)
    cat("Type Vector:\n")
    print(x$type)
    cat("Odds Matrix:\n")
    print(x$odds)
    cat("Head of Amputed Data Set\n")
    print(head(x$amp))
  } else {
    print(x, ...)
  }
  invisible(x)
}
