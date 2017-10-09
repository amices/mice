#'Converts an multiply imputed dataset (long format) into a \code{mids} object
#'
#'This function converts imputed data stored in long format into 
#'an object of class \code{mids}. The original incomplete dataset 
#'needs to be available so that we know where the missing data are.
#'The function is useful to convert back operations applied to 
#'the imputed data back in a \code{mids} object. It may also be 
#'used to store multiply imputed data sets from other software 
#'into the format used by \code{mice}.
#'
#'@aliases as.mids
#'@param data A multiply imputed data set in long format, for example 
#'produced by a call to \code{complete(..., action = 'long', include = TRUE)}. 
#'@param .imp Optional column number in \code{data} that contains the imputation 
#'number. The number \code{0} indicates the original data (with missings) 
#'and \code{1} through \code{m} correspond to the \code{m} imputation number. 
#'If not specified, the function searches for a variable named \code{".imp"}.
#'@param .id Optional column number in \code{data} indicating the subject 
#'identification. If not specified, then the function searches for a variable 
#'named \code{.id} in \code{data}. 
#'@details If .id variable is found, row names from the supplied data will 
#'be copied to the \code{data} elements of the returned \code{mids} object.
#'@return An object of class \code{mids}
#'@author Gerko Vink
#'@examples
#'# impute the nhanes dataset
#'imp <- mice(nhanes, print = FALSE)
#'# extract the data in long format
#'X <- complete(imp, action = "long", include = TRUE)
#'# create dataset with .imp variable as numeric
#'X2 <- X
#'X2$.imp <- as.numeric(levels(X$.imp))[X$.imp]
#'
#'# nhanes example without .id
#'test1 <- as.mids(X)
#'is.mids(test1)
#'all(complete(test1, action = "long", include = TRUE) == X, na.rm = TRUE)
#'
#'# nhanes example without .id where .imp is numeric
#'test2 <- as.mids(X2)
#'is.mids(test2)
#'all(complete(test2, action = "long", include = TRUE) == X, na.rm = TRUE)
#'
#'# nhanes example, where we explicitly specify .id as column 2
#'test3 <- as.mids(X, .id = 2)
#'is.mids(test3)
#'all(complete(test3, action = "long", include = TRUE) == X, na.rm = TRUE)
#'
#'# nhanes example with .id where .imp is numeric
#'test4 <- as.mids(X2, .id = 2)
#'is.mids(test4)
#'all(complete(test4, action = "long", include = TRUE) == X, na.rm = TRUE)
#'
#'# example without an .id variable
#'# variable .id not preserved
#'X3 <- X[, -2]
#'test5 <- as.mids(X3)
#'is.mids(test5)
#'all(complete(test5, action = "long", include = TRUE)[, -2] == X[, -2], na.rm = TRUE)
#'
#'# 
#'@keywords mids
#'@export
as.mids <- function(data, .imp = NA, .id = NA) {
  
  # locate first found column name .imp
  if (!is.na(.imp)) imp_pos = .imp
  else imp_pos <- which(".imp" == names(data))[1]
  if (is.na(imp_pos)) stop("No column named `.imp` found.")
  
  # locate first found column name .id
  if (!is.na(.id)) id_pos = .id
  else id_pos <- which(".id" == names(data))[1]
  
  # no missings allowed in .imp
  imps <- data[, imp_pos]
  if (anyNA(imps)) stop("Values `NA` in column `.imp` are not permitted.")
  
  # determine m
  m <- if (is.factor(imps)) max(as.numeric(levels(imps))[imps]) else max(imps)
  
  # get original data part  
  vars_to_remove <- na.omit(c(imp_pos, id_pos))
  orig_data <- data[imps == 0, -vars_to_remove, drop = FALSE]
  n <- nrow(orig_data) 
  if (n == 0) stop("Original data table not found. Use complete(..., action = 'long', include = TRUE) to include original data.")
  
  # use mice to get info on data
  ini <- mice(orig_data, m = m, maxit = 0, remove_collinear = FALSE)
  
  # store any .id as row names
  if (!is.na(id_pos)) rownames(ini$data) <- data[imps == 0, id_pos]
  
  # copy imputations from data into proper ini$imp elements
  names  <- names(ini$imp)
  for (i in seq_along(names)) {
    varname <- names[i]
    if(!is.null(ini$imp[[varname]])) {
      for(j in seq_len(m)) {
        idx <- imps == j & is.na(data[imps == 0, varname])
        ini$imp[[varname]][j] <- data[idx, varname]
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
#' @author Stef van Buuren
#' @export
as.mira <- function(fitlist) {
  call <- match.call()
  if (!is.list(fitlist)) 
    stop("Argument 'fitlist' is not a list")
  object <- list(call = call, call1 = NULL, nmis = NULL, analyses = fitlist)
  oldClass(object) <- c("mira", "matrix")
  return(object)
}
