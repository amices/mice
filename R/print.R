#' Print a `mids` object
#'
#' @rdname print
#' @param x Object of class `mids`, `mira` or `mipo`
#' @param ... Other parameters passed down to `print.default()`
#' @return `NULL`
#' @seealso [`mids()`][mids-class]
#' @method print mids
#' @export
print.mids <- function(x, ...) {
  cat("Class: mids\n")
  cat("Number of multiple imputations: ", x$m, "\n")
  cat("Imputation methods:\n")
  print(x$method, ...)
  cat("predictorMatrix:\n")
  print(head(x$predictorMatrix), ...)
  if (any(x$nest != colnames(x$data))) {
    cat("Variable nest:\n")
    print(x$nest, ...)
  }
  if (!is.null(x$loggedEvents)) {
    cat("Number of logged events: ", nrow(x$loggedEvents), "\n")
    print(head(x$loggedEvents), ...)
  }
  invisible(x)
}


#' Print a `mira` object
#'
#' @rdname print
#' @return `NULL`
#' @seealso [`mira()`][mira-class]
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


#' Print a `mice.anova` object
#'
#' @rdname print
#' @return `NULL`
#' @seealso [mipo()]
#' @method print mice.anova
#' @export
print.mice.anova <- function(x, ...) {
  z <- summary(x, ...)
  print(z$comparisons, row.names = FALSE)
  invisible(x)
}


#' Print a `summary.mice.anova` object
#'
#' @rdname print
#' @return `NULL`
#' @seealso [mipo()]
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


#' Print a `mads` object
#'
#' @param x Object of class `mads`
#' @param ... Other parameters passed down to `print.default()`
#' @return `NULL`
#' @seealso [`mads()`][mads-class]
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
