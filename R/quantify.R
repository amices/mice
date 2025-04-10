#' Quantify a factor for use in linear modeling
#'
#' This function replaces a factor variable by a numeric vector,
#' optionally using optimal scaling based on canonical correlation analysis.
#' It is used to transform categorical variables into a continuous scale
#' suitable for methods such as LARS or linear regression.
#'
#' If `quantify = TRUE`, the function derives an optimal numerical
#' representation of the factor levels based on their relationship with the
#' predictors `x`. If `quantify = FALSE`, the function simply maps the
#' factor levels to integers in their original order.
#'
#' If `y` is not a factor, the function returns `y` unchanged.
#'
#' @param y A response vector, possibly a factor.
#' @param ry A logical vector indicating the non-missing values of `y`.
#' @param x A numeric matrix of predictors.
#' @param quantify Logical. If `TRUE`, compute optimal scaling via canonical correlation analysis.
#' @return A list with the following components:
#' \describe{
#'   \item{ynum}{A numeric vector of the same length as `y` with the quantification applied.}
#'   \item{labels}{Character vector of factor levels (or `NULL` if `y` is not a factor).}
#'   \item{quant}{Numeric vector of quantification values for each level (or `NULL`).}
#' }
#' @author Stef van Buuren, 2025
#' @seealso [unquantify()]
#' @examples
#' # Simple quantification
#' y <- factor(c("a", "b", "a", "c", "b", "c"))
#' ry <- !is.na(y)
#' x <- matrix(rnorm(length(y) * 2), ncol = 2)
#' quantify(y, ry, x)
#'
#' # Without optimal scaling
#' quantify(y, ry, x, quantify = FALSE)
#'
#' # y is numeric: returned unchanged
#' quantify(1:6, rep(TRUE, 6), x)
#'
#' @export
quantify <- function(y, ry, x, quantify = TRUE) {
  if (!is.factor(y)) {
    return(list(ynum = y,
                labels = NULL,
                quant = NULL))
  }
  if (!quantify) {
    ynum <- as.integer(y)
    return(list(ynum = ynum,
                labels = levels(y),
                quant = 1L:length(levels(y))))
  }

  # replace (reduced set of) categories by optimal scaling
  yf <- factor(y[ry], exclude = NULL)
  yd <- model.matrix(~ 0 + yf)
  xd <- cbind(1, x[ry, , drop = FALSE])
  cca <- cancor(y = yd, x = xd, xcenter = FALSE, ycenter = FALSE)
  quant <- as.vector(cca$ycoef[, 2L])
  quant_expand <- quant[match(levels(y), levels(yf))]
  ynum <- quant_expand[match(as.character(y), levels(y))]
  return(list(ynum = ynum,
              labels = levels(y),
              quant = quant_expand))
}

#' Revert quantified variables back to factor representation
#'
#' This function reverses the transformation performed by [quantify()],
#' restoring the original factor levels from a numeric representation.
#' It works by assigning each value in `ynum` to the nearest quantification
#' value in `quant`, and then mapping that to the corresponding factor label.
#' If `ynum` contains `NA` values, these are preserved in the output.
#'
#' If `labels` is `NULL`, the function simply returns `ynum` unchanged.
#'
#' @param ynum A numeric vector created by [quantify()], typically containing scaled values.
#' @param quant A numeric vector of quantification values corresponding to the original factor levels.
#' @param labels A character vector of labels associated with the levels of the original factor.
#' @note This function is intended to be used after [quantify()] to revert the quantification.
#' The function may fail to produce the original factor levels if the quantification
#' values are not unique.
#' @return A factor vector with the levels specified by `labels`, or the numeric vector `ynum` if `labels` is `NULL`.
#'
#' @examples
#' set.seed(123)
#' y <- factor(c("low", "medium", "high", "high", "medium", "low"))
#' x <- matrix(runif(6), nrow = 6)
#' ry <- rep(TRUE, 6)
#' q <- quantify(y, ry, x)
#' unquantify(q$ynum, q$quant, q$labels)
#'
#' # Handle missing values
#' ymiss <- y
#' ymiss[c(1, 3)] <- NA
#' ry <- !is.na(ymiss)
#' q2 <- quantify(ymiss, ry, x)
#' unquantify(q2$ynum, q2$quant, q2$labels)
#' @seealso [quantify()]
#' @export
unquantify <- function(ynum = NULL, quant = NULL, labels = NULL) {
  if (is.null(labels)) return(ynum)

  closest <- vapply(seq_along(ynum), function(i) {
    y <- ynum[i]
    if (is.na(y)) return(NA_character_)
    i_match <- which.min(abs(y - quant))
    labels[i_match]
  }, character(1))

  factor(closest, levels = labels)
}
