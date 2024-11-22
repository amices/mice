#' Extract list of fitted models
#'
#' Function `getfit()` returns the list of objects containing the repeated analysis
#' results, or optionally, one of these fitted objects. The function looks for
#' a list element called `analyses`, and return this component as a list with
#' `mira` class. If element `analyses` is not found in `x`, then
#' it returns `x` as a `mira` object.
#'
#' No checking is done for validity of objects. The function also processes
#' objects of class `mitml.result` from the `mitml` package.
#'
#' @param x An object of class `mira`, typically produced by a call
#' to `with()`.
#' @param i An integer between 1 and `x$m` signalling the index of the
#' repeated analysis. The default `i= -1` return a list with all analyses.
#' @param simplify Should the return value be unlisted?
#' @return If `i = -1` an object of class `mira` containing
#' all analyses. If `i` selects one of the analyses, then it return
#' an object whose with class inherited from that element.
#' @author Stef van Buuren, 2012, 2020
#' @seealso [`mira()`][mira-class], [with.mids()]
#' @keywords manip
#' @examples
#' imp <- mice(nhanes, print = FALSE, seed = 21443)
#' fit <- with(imp, lm(bmi ~ chl + hyp))
#' f1 <- getfit(fit)
#' class(f1)
#' f2 <- getfit(fit, 2)
#' class(f2)
#' @export
getfit <- function(x, i = -1L, simplify = FALSE) {
  if (is.null(x$analyses)) {
    ra <- x
  } else {
    ra <- x$analyses
  }
  if (i != -1L) {
    return(ra[[i]])
  }
  if (simplify) ra <- unlist(ra)
  class(ra) <- c("mira", "list")
  ra
}

#' Extract estimate from `mipo` object
#'
#' `getqbar` returns a named vector of pooled estimates.
#'
#' @param x An object of class `mipo`
#' @export
getqbar <- function(x) {
  if (!is.mipo(x)) stop("Not a mipo object")
  qbar <- x$pooled$estimate
  # note: not supported: component/y.values
  names(qbar) <- x$pooled$term
  qbar
}
