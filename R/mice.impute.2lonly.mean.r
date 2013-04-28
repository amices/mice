# ------------------------MICE.IMPUTE.2lonly.mean---------------------------------

#'Imputation of the mean within the class
#'
#'Imputes the mean of within the class
#'
#'Observed values in \code{y} are averaged within the class, and replicated to
#'the missing \code{y} within that class. If there are no observed data in the
#'class, all entries of the class are set to \code{NaN}.  This function is
#'primarily useful for repairing incomplete data that are constant within the
#'class, but that vary over the classes.
#'
#'@aliases mice.impute.2lonly.mean 2lonly.mean
#'@param y Incomplete data vector of length \code{n}
#'@param ry Vector of missing data pattern (\code{FALSE}=missing,
#'\code{TRUE}=observed)
#'@param x Matrix (\code{n} x \code{p}) of complete covariates.
#'@param type Vector of length \code{ncol(x)} identifying random and class
#'variables.  The class variable (only one is allowed) is coded as '-2'.
#'@param ... Other named arguments.
#'@return A vector of length \code{nmis} with imputations.
#'@author Gerko Vink, Stef van Buuren, 2013
#'@keywords datagen
#'@export
mice.impute.2lonly.mean <- function(y, ry, x, type, ...) {
    ## Gerko Vink, Stef van Buuren mar2013 class mean imputation of second level variables
    if (all(ry)) 
        stop("No missing data found")
    yobs <- y[ry]
    class <- x[, type == -2]
    if (ncol(class) == 0) stop("No class variable")   ## SvB 27apr2013
    classobs <- class[ry]
    classmis <- class[!ry]
    
    # deal with empty classes (will be NaN)
    empty.classes <- class[!class %in% classobs]
    classobs <- c(classobs, empty.classes)
    yobs <- c(yobs, rep(NA, length(empty.classes)))
    
    # return the means per class
    ymean <- aggregate(yobs, list(classobs), mean, na.rm = TRUE)
    return(apply(as.matrix(classmis), 1, function(z, y) y[z == y[, 1], 2], y = ymean, ...))
}
