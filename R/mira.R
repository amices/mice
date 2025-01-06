#' Create an object of class "mira"
#'
#' The mira() functions constructs an S3 object representing a set of
#' multiply imputed repeated analyses (mira). The default workflow generates
#' the \code{mira} object using the \code{with()} function.
#'
#' The \code{as.mira()} function takes the results of repeated
#' complete-data analysis stored as a list, and turns it into a \code{mira}
#' object that can be pooled.
#'
#' @param call The function call that created the object.
#' @param call1 A secondary function call, typically from the first imputation.
#' @param nmis An integer vector representing the number of missing values.
#' @param analyses A list of analyses performed on the imputed datasets.
#' @return An object of class \code{"mira"}. The \code{mira} class contains
#' the following elements:
#'  \describe{
#'    \item{\code{.Data}:}{Object of class \code{"list"} containing the
#'    following slots:}
#'    \item{\code{call}:}{The call that created the object.}
#'    \item{\code{call1}:}{The call that created the \code{mids} object that was used
#' in \code{call}.}
#'    \item{\code{nmis}:}{An array containing the number of missing observations per
#' column.}
#'    \item{\code{analyses}:}{A list of \code{m} components containing the individual
#' fit objects from each of the \code{m} complete data analyses.}
#' }
#' @details
#' In versions prior to \code{mice 3.0} pooling required only that
#' \code{coef()} and \code{vcov()} methods were available for fitted
#' objects. \emph{This feature is no longer supported}. The reason is that \code{vcov()}
#' methods are inconsistent across packages, leading to buggy behaviour
#' of the \code{pool()} function. Since \code{mice 3.0+}, the \code{broom}
#' package takes care of filtering out the relevant parts of the
#' complete-data analysis. It may happen that you'll see the messages
#' like \code{No method for tidying an S3 object of class ...} or
#' \code{Error: No glance method for objects of class ...}. The royal
#' way to solve this problem is to write your own \code{glance()} and \code{tidy()}
#' methods and add these to \code{broom} according to the specifications
#' given in \url{https://broom.tidymodels.org}.
#'
#' The \code{mira} class of objects has methods for the
#' following generic functions: \code{print}, \code{summary}.
#' @author Stef van Buuren, Karin Groothuis-Oudshoorn, 2000
#' @seealso \code{\link{with.mids}}, \code{\link[=mids-class]{mids}}, \code{\link{mipo}}
#' @references van Buuren S and Groothuis-Oudshoorn K (2011). \code{mice}:
#' Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#' Statistical Software}, \bold{45}(3), 1-67.
#' \doi{10.18637/jss.v045.i03}
#' @keywords classes
#' @name mira
#' @aliases mira mira-class
#' @export
mira <- function(
    call = match.call(),
    call1 = match.call(),
    nmis = integer(),
    analyses = list()) {

  # Create the object as a list
  obj <- list(
    call = as.call(call),
    call1 = as.call(call1),
    nmis = as.integer(nmis),
    analyses = as.list(analyses)
  )

  # Assign the class
  class(obj) <- "mira"
  return(obj)
}
