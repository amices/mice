#' Imputation of most likely value within the class
#'
#' Method `2lonly.mean` replicates the most likely value within
#' a class of a second-level variable. It works for numeric and
#' factor data. The function is primarily useful as a quick fixup for
#' data in which the second-level variable is inconsistent.
#'
#' @aliases 2lonly.mean
#' @inheritParams mice.impute.pmm
#' @param type Vector of length `ncol(x)` identifying random and class
#' variables.  The class variable (only one is allowed) is coded as `-2`.
#' @param ... Other named arguments.
#' @return Vector with imputed data, same type as `y`, and of length
#' `sum(wy)`
#' @details
#' Observed values in `y` are averaged within the class, and
#' replicated to the missing `y` within that class.
#' This function is primarily useful for repairing incomplete data
#' that are constant within the class, but vary over classes.
#'
#' For numeric variables, `mice.impute.2lonly.mean()` imputes the
#' class mean of `y`. If `y` is a second-level variable, then
#' conventionally all observed `y` will be identical within the
#' class, and the function just provides a quick fix for any
#' missing `y` by filling in the class mean.
#'
#' For factor variables, `mice.impute.2lonly.mean()` imputes the
#' most frequently occuring category within the class.
#'
#' If there are no observed `y` in the class, all entries of the
#' class are set to `NA`. Note that this may produce problems
#' later on in `mice` if imputation routines are called that
#' expects predictor data to be complete. Methods designed for
#' imputing this type of second-level variables include
#' [mice.impute.2lonly.norm()] and
#' [mice.impute.2lonly.pmm()].
#'
#' @references
#' Van Buuren, S. (2018).
#' [*Flexible Imputation of Missing Data. Second Edition.*](https://stefvanbuuren.name/fimd/sec-level2pred.html)
#' Boca Raton, FL.: Chapman & Hall/CRC Press.
#' @author Gerko Vink, Stef van Buuren, 2019
#' @family univariate-2lonly
#' @keywords datagen
#' @export
mice.impute.2lonly.mean <- function(y, ry, x, type, wy = NULL, ...) {
  if (all(ry)) {
    return(numeric(0))
  }
  if (is.null(wy)) {
    wy <- !ry
  }
  yobs <- y[ry]
  class <- x[, type == -2]
  if (length(class) == 0) {
    stop("No class variable")
  }
  classobs <- class[ry]
  classmis <- class[wy]

  # deal with empty classes (will be NaN)
  empty.classes <- class[!class %in% classobs]
  classobs <- c(classobs, empty.classes)
  yobs <- c(yobs, rep.int(NA, length(empty.classes)))

  # factor: return class levels corresponding to class median
  if (is.factor(y)) {
    ym <- aggregate(yobs, list(classobs), median, na.rm = TRUE)
    ym$x <- as.integer(ym$x)
    return(apply(as.matrix(classmis), 1,
      function(z, y, lev) lev[y[z == y[, 1], 2]],
      y = ym, lev = levels(y), ...
    ))
  }

  # otherwise: return the class means
  ym <- aggregate(yobs, list(classobs), mean, na.rm = TRUE)
  z <- apply(as.matrix(classmis), 1,
    function(z, y) y[z == y[, 1], 2],
    y = ym, ...
  )
  z[is.nan(z)] <- NA
  z
}
