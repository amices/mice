# ----------------------------getfit-------------------------------

#'Extracts fit objects from \code{mira} object
#'
#'\code{getfit} returns the list of objects containing the repeated analysis
#'results, or optionally, one of these fit objects.
#'
#'This function is shorthand notation for \code{x$analyses} and
#'\code{x$analyses[[i]].}
#'
#'@param x An object of class \code{mira}, typically produced by a call to
#'\code{with()}.
#'@param i An integer between 1 and \code{x$m} signalling the number of the
#'repeated analysis.  The default \code{i= -1} return a list with all analyses.
#'@param simplify Should the return value be unlisted?
#'@return If \code{i = -1} an object containing all analyses, otherwise it
#'returns the fittd object of the i'th repeated analysis.
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
getfit <- function(x, i = -1, simplify = FALSE) {
    if (!is.mira(x)) 
        return(NULL)
    ra <- x$analyses
    if (i != -1) 
        return(ra[[i]])
    if (simplify) 
        ra <- unlist(ra)
    return(ra)
}
