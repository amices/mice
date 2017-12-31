#'Extract list of fitted model
#'
#'\code{getfit} returns the list of objects containing the repeated analysis
#'results, or optionally, one of these fit objects.
#'
#'@param x An object of class \code{mira} or \code{mitml.result}, 
#'typically produced by a call to \code{with()}.
#'@param i An integer between 1 and \code{x$m} signalling the number of the
#'repeated analysis. The default \code{i= -1} return a list with all analyses.
#'@param simplify Should the return value be unlisted?
#'@return If \code{i = -1} an object of class \code{mitml.result} containing 
#'all analyses, otherwise it returns the fitted object of 
#'the i'th repeated analysis.
#'@author Stef van Buuren, March 2012.
#'@seealso \code{\link[=mira-class]{mira}}, \code{\link{with.mids}}
#'@keywords manip
#'@examples
#'
#'imp <- mice(nhanes)
#'fit <- with(imp, lm(bmi~chl+hyp))
#'getfit(fit)
#'getfit(fit, 2)
#'
#'@export
getfit <- function(x, i = -1L, simplify = FALSE) {
  ra <- NULL
  if (is.mira(x)) ra <- x$analyses
  if (is.mitml.result(x)) ra <- x
  if (i != -1L) return(ra[[i]])
  if (simplify) ra <- unlist(ra)
  class(ra) <- c("mitml.result", "list")
  ra
}
