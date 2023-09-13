#' Select complete cases
#'
#' Extracts the complete cases, also known as *listwise deletion*.
#' `cc(x)` is similar to
#' `na.omit(x)`, but returns an object of the same class
#' as the input data. Dimensions are not dropped. For extracting
#' incomplete cases, use [ici()].
#'
#' @param x An `R` object. Methods are available for classes
#' `mids`, `data.frame` and `matrix`. Also, `x`
#' could be a vector.
#' @return A `vector`, `matrix` or `data.frame` containing the data of the complete cases.
#' @author Stef van Buuren, 2017.
#' @seealso [na.omit()], [cci()], [ici()]
#' @keywords univar
#' @examples
#'
#' # cc(nhanes)   # get the 13 complete cases
#' # cc(nhanes$bmi) # extract complete bmi
#' @export
cc <- function(x) UseMethod("cc", x)

#' @export
cc.mids <- function(x) {
  x$data[cci(x), , drop = FALSE]
}

#' @export
cc.matrix <- function(x) {
  x[cci(x), , drop = FALSE]
}

#' @export
cc.data.frame <- function(x) {
  x[cci(x), , drop = FALSE]
}

#' @export
cc.default <- function(x) {
  x[cci(x)]
}


#' Select incomplete cases
#'
#' Extracts incomplete cases from a data set.
#' The companion function for selecting the complete cases is [cc()].
#'
#' @param x An `R` object. Methods are available for classes
#' `mids`, `data.frame` and `matrix`. Also, `x`
#' could be a vector.
#' @return A `vector`, `matrix` or `data.frame` containing the data of the complete cases.
#' @author Stef van Buuren, 2017.
#' @seealso [cc()], [ici()]
#' @keywords univar
#' @examples
#'
#' ic(nhanes) # get the 12 rows with incomplete cases
#' ic(nhanes[1:10, ]) # incomplete cases within the first ten rows
#' ic(nhanes[, c("bmi", "hyp")]) # restrict extraction to variables bmi and hyp
#' @export
ic <- function(x) UseMethod("ic", x)

#' @export
ic.mids <- function(x) {
  x$data[ici(x), , drop = FALSE]
}

#' @export
ic.matrix <- function(x) {
  x[ici(x), , drop = FALSE]
}

#' @export
ic.data.frame <- function(x) {
  x[ici(x), , drop = FALSE]
}

#' @export
ic.default <- function(x) {
  x[ici(x)]
}
