#' Number of complete cases
#'
#' Calculates the number of complete cases.
#'
#' @param x An `R` object. Currently supported are methods for the
#' following classes: `mids`, `data.frame` and `matrix`. Also,
#' `x` can be a vector.
#' @return Number of elements in `x` with complete data.
#' @author Stef van Buuren, 2017
#' @seealso [nic()], [cci()]
#' @examples
#' ncc(nhanes) # 13 complete cases
#' @export
ncc <- function(x) sum(cci(x))

#' Number of incomplete cases
#'
#' Calculates the number of incomplete cases.
#'
#' @param x An `R` object. Currently supported are methods for the
#' following classes: `mids`, `data.frame` and `matrix`. Also,
#' `x` can be a vector.
#' @return Number of elements in `x` with incomplete data.
#' @author Stef van Buuren, 2017
#' @seealso [ncc()], [cci()]
#' @examples
#' nic(nhanes) # the remaining 12 rows
#' nic(nhanes[, c("bmi", "hyp")]) # number of cases with incomplete bmi and hyp
#' @export
nic <- function(x) sum(ici(x))
