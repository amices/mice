#' Missing data pattern by variable pairs
#'
#' Number of observations per variable pair.
#'
#' The four components in the output value is have the following interpretation:
#' \describe{ \item{list('rr')}{response-response, both variables are observed}
#' \item{list('rm')}{response-missing, row observed, column missing}
#' \item{list('mr')}{missing -response, row missing, column observed}
#' \item{list('mm')}{missing -missing, both variables are missing} }
#'
#' @param data A data frame or a matrix containing the incomplete data.  Missing
#' values are coded as \code{NA}.
#' @return A list of four components named \code{rr}, \code{rm}, \code{mr} and
#' \code{mm}.  Each component is square numerical matrix containing the number
#' observations within four missing data pattern.
#' @author Stef van Buuren, Karin Groothuis-Oudshoorn, 2009
#' @references Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}:
#' Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#' Statistical Software}, \bold{45}(3), 1-67.
#' \url{https://www.jstatsoft.org/v45/i03/}
#' @keywords univar
#' @examples
#' pat <- md.pairs(nhanes)
#' pat
#'
#' # show that these four matrices decompose the total sample size
#' # for each pair
#' pat$rr + pat$rm + pat$mr + pat$mm
#'
#' # percentage of usable cases to impute row variable from column variable
#' round(100 * pat$mr / (pat$mr + pat$mm))
#' @export
md.pairs <- function(data) {
  # calculates pairwise missing data statistics
  # rr:  response-response pairs
  # rm:  response-missing pairs
  # mr:  missing-response pairs
  # mm:  missing-missing pairs
  if (!(is.matrix(data) || is.data.frame(data))) {
    stop("Data should be a matrix or dataframe")
  }
  if (ncol(data) < 2) {
    stop("Data should have at least two columns")
  }

  r <- !is.na(data)
  rr <- t(r) %*% r
  mm <- t(!r) %*% (!r)
  mr <- t(!r) %*% r
  rm <- t(r) %*% (!r)
  list(rr = rr, rm = rm, mr = mr, mm = mm)
}
