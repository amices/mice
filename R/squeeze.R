#' Squeeze the imputed values to be within specified boundaries.
#'
#' The `squeeze()` function is useful for imputed values that are
#' constrained to a certain range. It replaces imputed values for
#' imputations in \code{x} lower than \code{bounds[1]} by \code{bounds[1]},
#' and replaces values higher than \code{bounds[2]} by \code{bounds[2]}.
#'
#' @note
#' The function is designed to be called by the `post` argument of the
#' `mice()` function. Since the update to `data.table` for the `imp`
#' element of the `mids` object, the `x` argument should now be a
#' `data.table` instead of a vector.
#'
#' @aliases squeeze
#' @param x A `data.table` with *m* columns of imputed values followed
#' by one column called `"row_id"` containing the row index in `data`.
#' @param bounds A numerical vector of length 2 containing the lower and upper bounds.
#' By default, there are no bounds.
#' @param r A logical vector of length \code{length(x)} that is used to select a
#' subset rows in \code{x} before calculating automatic bounds.
#' @return A `data.table` of the same structure as `x`
#' @author Stef van Buuren, 2011, 2025
#' @examples
#' ini <- mice(boys, maxit = 0)
#' meth <- ini$meth
#'
#' # convert to numeric to evade warnings
#' meth["tv"] <- "norm"
#' boys$tv <- as.numeric(boys$tv)
#'
#' post <- ini$post
#' post["tv"] <- "imp[[j]] <- squeeze(imp[[j]], c(1, 25))"
#'
#' # without post-processing
#' imp1 <- mice(boys, meth = meth, print = FALSE, m = 2, maxit = 2, seed = 1)
#'
#' # with post-processing
#' imp2 <- mice(boys, meth = meth, post = post, print = FALSE, m = 2,
#'              maxit = 2, seed = 1)
#'
#' summary(imp1$imp$tv)
#' summary(imp2$imp$tv)
#'
#' oldpar <- par(mfrow = c(1, 2))
#' plot(imp1$imp$tv[, 1:2], col = mdc(2), xlab = "Imputation 1",
#'      ylab = "Imputation 2", xlim = c(-20, 30), ylim = c(-20, 30),
#'      cex = 0.6, main = "No bounds")
#' plot(imp2$imp$tv[, 1:2], col = mdc(2), xlab = "Imputation 1",
#'       ylab = "Imputation 2", xlim = c(-20, 30), ylim = c(-20, 30),
#'       cex = 0.6, main = "Bounds 1 and 25")
#' par(oldpar)
#' @export
squeeze <- function(x,
                    bounds = NULL,
                    r = rep.int(TRUE, nrow(x))) {
  if (!is.data.table(x)) {
    stop("Argument x should be a data.table")
  }
  if (length(r) != nrow(x)) {
    stop("Length of r unequal to nrow(x)")
  }

  # do not apply transformation to column "row_id"
  z <- x[, !"row_id", with = FALSE]
  if (is.null(bounds)) {
    bounds <- c(min(z[r, ]), max(z[r, ]))
  }
  z[z < bounds[1]] <- bounds[1]
  z[z > bounds[2]] <- bounds[2]
  x <- as.data.table(cbind(z, row_id = x[["row_id"]]))
  return(x)
}
