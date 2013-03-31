#' Create a \code{mira} object from repeated analyses
#' 
#' The \code{as.mira()} function takes the results of repeated 
#' complete-data analysis stored as a list, and turns it 
#' into a \code{mira} object that can be pooled. Pooling 
#' requires that \code{coef()} and \code{vcov()} methods are 
#' available for fitted object. 
#' @param fitlist A list containing $m$ fitted analysis objects
#' @return An S3 object of class \code{mira}.
#' @seealso \code{\link[=mira-class]{mira}}
#' @author Stef van Buuren, 2011
#' @export
as.mira <- function(fitlist) {
    call <- match.call()
    if (!is.list(fitlist)) 
        stop("Argument 'fitlist' is not a list")
    m <- length(fitlist)
    object <- list(call = call, call1 = NULL, nmis = NULL, analyses = fitlist)
    oldClass(object) <- c("mira", "matrix")
    return(object)
}
