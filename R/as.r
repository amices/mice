#'Converts an multiply imputed dataset (long format) into a \code{mids} object
#'
#'This function converts imputed data stored in long format into an object of class \code{mids}.
#'The original incomplete data set needs to be available so that we know where the missing data are.
#'The function is useful to convert back operations applied to the imputed data 
#'back in a \code{mids} object. It may also be used to store multiply imputed data sets
#'from other software into the format used by \code{mice}.
#'
#'@aliases as.mids
#'@usage as.mids(data, .imp=1, .id=2)
#'@param data A multiply imputed data set in long format
#'@param .imp Mandatory column indicator for the multiple imputation stream, where \code{0} indicates the incomplete data and \code{1} through \code{m} indicate the \code{m} multiple imputation streams. Default is \code{1}.
#'@param .id Optional column indicator for the row numbers. Default is \code{2}.
#'@details If \code{.id} is specified, row names from the original data (if supplied) will be copied to the \code{mids} object.
#'@return An object of class \code{mids}
#'@author Gerko Vink, 2012
#'@examples 
#'# nhanes example without .id
#'imp <- mice(nhanes, print = FALSE)
#'X <- complete(imp, action = "long", include = TRUE)[, -2]
#'test <- as.mids(X, .id = NULL)
#'is.mids(test)
#'test.dat <- complete(test, action = "long", include = TRUE)
#'
#'# Test on boys data
#'imp <- mice(boys, print = FALSE, maxit = 1)
#'X <- complete(imp, action = "long", include = TRUE)
#'test <- as.mids(X)
#'is.mids(test)
#'test.dat <- complete(test, action = "long", include = TRUE)
#'# original rownumbers are automatically copied from .id
#'@keywords mids
#'@export
as.mids <- function(data, .imp = 1, .id = 2)
{
    ini <- mice(data[data[, .imp] == 0, -c(.imp, .id)], 
                m = max(as.numeric(data[, .imp])),
                maxit = 0)
    names  <- names(ini$imp)
    if (!is.null(.id)){
        rownames(ini$data) <- data[data[, .imp] == 0, .id]
    }
    for (i in 1:length(names)){
        for(m in 1:(max(as.numeric(data[, .imp])) - 1)){
            if(!is.null(ini$imp[[i]])){
                indic <- data[, .imp] == m & is.na(data[data[, .imp]==0, names[i]])
                ini$imp[[names[i]]][m] <- data[indic, names[i]]
            }
        }
    }
    return(ini)
}


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
