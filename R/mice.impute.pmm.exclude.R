#' Imputation by predictive mean matching with optional exclusion of values
#'
#' @aliases mice.impute.pmm.exclude pmm.exclude
#' @param y Vector to be imputed
#' @param ry Logical vector of length \code{length(y)} indicating the
#' the subset \code{y[ry]} of elements in \code{y} to which the imputation
#' model is fitted. The \code{ry} generally distinguishes the observed
#' (\code{TRUE}) and missing values (\code{FALSE}) in \code{y}.
#' @param x Numeric design matrix with \code{length(y)} rows with predictors for
#' \code{y}. Matrix \code{x} may have no missing values.
#' @param exclude Value or vector of values to exclude from the imputation donor pool in \code{y}
#' @param wy Logical vector of length \code{length(y)}. A \code{TRUE} value
#' indicates locations in \code{y} for which imputations are created.
#' @param donors The size of the donor pool among which a draw is made.
#' The default is \code{donors = 5L}. Setting \code{donors = 1L} always selects
#' the closest match, but is not recommended. Values between 3L and 10L
#' provide the best results in most cases (Morris et al, 2015).
#' @param matchtype Type of matching distance. The default choice
#' (\code{matchtype = 1L}) calculates the distance between
#' the \emph{predicted} value of \code{yobs} and
#' the \emph{drawn} values of \code{ymis} (called type-1 matching).
#' Other choices are \code{matchtype = 0L}
#' (distance between predicted values) and \code{matchtype = 2L}
#' (distance between drawn values).
#' @param ridge The ridge penalty used in \code{.norm.draw()} to prevent
#' problems with multicollinearity. The default is \code{ridge = 1e-05},
#' which means that 0.01 percent of the diagonal is added to the cross-product.
#' Larger ridges may result in more biased estimates. For highly noisy data
#' (e.g. many junk variables), set \code{ridge = 1e-06} or even lower to
#' reduce bias. For highly collinear data, set \code{ridge = 1e-04} or higher.
#' @param use.matcher Logical. Set \code{use.matcher = TRUE} to specify
#' the C function \code{matcher()}, the now deprecated matching function that
#' was default in versions
#' \code{2.22} (June 2014) to \code{3.11.7} (Oct 2020). Since version \code{3.12.0}
#' \code{mice()} uses the much faster \code{matchindex} C function. Use
#' the deprecated \code{matcher} function only for exact reproduction.
#' @param \dots Other named arguments.
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#' @author Stef van Buuren, Karin Groothuis-Oudshoorn
#' @details
#' Imputation of \code{y} by predictive mean matching, based on
#' van Buuren (2012, p. 73). More details can be found in the documentation for 
#' [mice.impute.pmm()]
#' @family univariate imputation functions
#' @keywords datagen
#' @seealso [mice.impute.pmm()]
#' @examples
#' # We normally call mice.impute.pmm.exclude() from within mice()
#' # But we may call it directly as follows (not recommended)
#'
#' set.seed(53177)
#' xname <- c("age", "hgt", "wgt")
#' r <- stats::complete.cases(boys[, xname])
#' x <- boys[r, xname]
#' y <- boys[r, "tv"]
#' ry <- !is.na(y)
#' table(ry)
#'
#' # percentage of missing data in tv
#' sum(!ry) / length(ry)
#'
#' # Impute missing tv data with original pmm and pmm.exclude where the 
#' # excluded value (-Inf) falls outside the range of the observed data
#' set.seed(123); yimp.pmm <- mice.impute.pmm(y, ry, x)
#' set.seed(123); yimp <- mice.impute.pmm.exclude(y, ry, x)
#' identical(yimp, yimp.pmm) #should be TRUE
#' 
#' # Impute missing tv data with pmm.exclude set to exclude 20 and 25
#' set.seed(123); yimp <- mice.impute.pmm.exclude(y, ry, x, exclude = c(20, 25))
#' identical(yimp, yimp.pmm) # should be FALSE
#' c(20, 25) %in% yimp # should be FALSE twice
#' @export
mice.impute.pmm.exclude <- function(y, ry, x, exclude = -Inf, wy = NULL, donors = 5L,
                            matchtype = 1L, ridge = 1e-05,
                            use.matcher = FALSE, ...) {
  id.ex <- !ry | !y %in% exclude # id vector for exclusion
  y <- y[id.ex] # leave out the exclude vector y's
  x <- x[id.ex, ] # leave out the exclude vector x's
  ry <- ry[id.ex] # leave out the exclude vector indicator
  {
    if (is.null(wy)) {
      wy <- !ry
    } else {
      wy <- wy[id.ex] # if applicable adjust wy to match exclude
    }
  }
  x <- cbind(1, as.matrix(x)) 
  ynum <-  y
  if (is.factor(y)) {
    ynum <- as.integer(y)
  }

  parm <- .norm.draw(ynum, ry, x, ridge = ridge, ...)
  if (matchtype == 0L) {
    yhatobs <- x[ry, , drop = FALSE] %*% parm$coef
    yhatmis <- x[wy, , drop = FALSE] %*% parm$coef
  }
  if (matchtype == 1L) {
    yhatobs <- x[ry, , drop = FALSE] %*% parm$coef
    yhatmis <- x[wy, , drop = FALSE] %*% parm$beta
  }
  if (matchtype == 2L) {
    yhatobs <- x[ry, , drop = FALSE] %*% parm$beta
    yhatmis <- x[wy, , drop = FALSE] %*% parm$beta
  }
  if (use.matcher) {
    idx <- matcher(yhatobs, yhatmis, k = donors)
  } else {
    idx <- matchindex(yhatobs, yhatmis, donors)
  }
  return(y[ry][idx])
}


