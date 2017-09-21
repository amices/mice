#'Imputation of the mean within the class
#'
#'Imputes the mean of within the class
#'
#'@inheritParams mice.impute.pmm
#'@param type Vector of length \code{ncol(x)} identifying random and class
#'variables.  The class variable (only one is allowed) is coded as \code{-2}.
#'@param ... Other named arguments.
#'@return Vector with imputed data, same type as \code{y}, and of length 
#'\code{sum(wy)}
#'@details
#'Observed values in \code{y} are averaged within the class, and replicated to
#'the missing \code{y} within that class. If there are no observed data in the
#'class, all entries of the class are set to \code{NaN}.  This function is
#'primarily useful for repairing incomplete data that are constant within the
#'class, but that vary over the classes.
#'@author Gerko Vink, Stef van Buuren, 2013
#'@family univariate \code{2lonly} functions
#'@keywords datagen
#'@export
mice.impute.2lonly.mean <- function(y, ry, x, type, wy = NULL, ...) {
  if (all(ry)) 
    return(numeric(0))
  if (is.null(wy)) 
    wy <- !ry
  yobs <- y[ry]
  class <- x[, type == -2]
  if (length(class) == 0) 
    stop("No class variable")
  classobs <- class[ry]
  classmis <- class[wy]
  
  # deal with empty classes (will be NaN)
  empty.classes <- class[!class %in% classobs]
  classobs <- c(classobs, empty.classes)
  yobs <- c(yobs, rep.int(NA, length(empty.classes)))
  
  # return the means per class
  ymean <- aggregate(yobs, list(classobs), mean, na.rm = TRUE)
  return(apply(as.matrix(classmis), 1, function(z, y) y[z == y[, 1], 
                                                        2], y = ymean, ...))
}
