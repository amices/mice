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
#'@author Gerko Vink
#'@examples 
#'# impute the nhanes dataset
#'imp <- mice(nhanes, print = FALSE)
#'# extract the data in long format
#'X <- complete(imp, action = "long", include = TRUE)
#'# create dataset with .imp variable that is not a factor
#'X2 <- X
#'X2$.imp <- as.numeric(levels(X$.imp))[X$.imp]
#'# nhanes example without .id
#'test1 <- as.mids(X[, -2], .id = NULL)
#'is.mids(test1)
#'all(complete(test1, action = "long", include = TRUE) == X, na.rm = TRUE)
#'# nhanes example without .id where .imp is not a factor
#'test2 <- as.mids(X2[, -2], .id = NULL)
#'is.mids(test2)
#'all(complete(test1, action = "long", include = TRUE) == X, na.rm = TRUE)
#'# nhanes example with .id
#'test3 <- as.mids(X, .id = 2)
#'is.mids(test3)
#'all(complete(test1, action = "long", include = TRUE) == X, na.rm = TRUE)
#'# nhanes example with .id where .imp is not a factor
#'test4 <- as.mids(X2, .id = 2)
#'is.mids(test4)
#'all(complete(test1, action = "long", include = TRUE) == X, na.rm = TRUE)
#'@keywords mids
#'@export
as.mids <- function(data, .imp = 1, .id = 2){
  #added GV 09 Nov 2015
  imps <- data[, .imp]
  if(is.factor(imps)){
    m = max(as.numeric(levels(imps))[imps])
  } else {
    m = max(imps)
  }
  #end added
  ini <- mice(data[imps == 0, -c(.imp, .id)], m = m, maxit = 0)
  names  <- names(ini$imp)
  if (!is.null(.id)){
    rownames(ini$data) <- data[imps == 0, .id]
  }
  for (i in 1:length(names)){
    for(j in 1:m){
      if(!is.null(ini$imp[[i]])){
        indic <- imps == j & is.na(data[imps == 0, names[i]])
        ini$imp[[names[i]]][j] <- data[indic, names[i]]
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
