#' Imputation by Treeffuser diffusion model
#'
#' @aliases mice.impute.treeffuser treeffuser
#' @param y Vector to be imputed
#' @param ry Logical vector of length \code{length(y)} indicating the
#' the subset \code{y[ry]} of elements in \code{y} to which the imputation
#' model is fitted. The \code{ry} generally distinguishes the observed
#' (\code{TRUE}) and missing values (\code{FALSE}) in \code{y}.
#' @param x Numeric design matrix with \code{length(y)} rows with predictors for
#' \code{y}. Matrix \code{x} may have no missing values.
#' @param wy Logical vector of length \code{length(y)}. A \code{TRUE} value
#' indicates locations in \code{y} for which imputations are created.
#' @param donors The size of the donor pool among which a draw is made.
#' The default is \code{donors = 5L}. Values between 3L and 10L provide
#' the best results in most cases.
#' @param ... Other named arguments.
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#' @export
mice.impute.treeffuser <- function(y, ry, x, wy = NULL, donors = 5L, ...) {
  if (is.null(wy)) {
    wy <- !ry
  }

  # Fit Treeffuser model to observed data
  model <- Treeffuser::train(y[ry], x[ry, , drop = FALSE])

  # Predict missing values
  y_pred <- predict(model, x[wy, , drop = FALSE])

  # Perform donor matching
  yhatobs <- predict(model, x[ry, , drop = FALSE])
  yhatmis <- y_pred

  idx <- matchindex(yhatobs, yhatmis, donors)

  return(y[ry][idx])
}
