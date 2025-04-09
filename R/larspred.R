#' Quick selection of predictors by Least Angle Regression (LAR) algorithm
#'
#' Single imputation of incomplete data, followed by the LARS algorithm
#' for variables selection.
#'
#' This function creates a predictor matrix using the variable selection
#' procedure described in Van Buuren et al.~(1999, p.~687--688). The function is
#' designed to aid in setting up a good imputation model for data with many
#' variables.
#'
#' Basic workings: The procedure creates a single imputation of the data
#' by a random draw from the marginal, and then applies the LAR algorithm
#' twice for each incomplete variable: once with the variable as dependent,
#' and once with the missingness indicator of the variable as dependent. The
#' union of both LAR analyses is used to select the predictors for the
#' imputation model.
#'
#' The LAR algorithm is a fast approximation of the LASSO algorithm, which
#' is a penalized regression method that shrinks coefficients to zero.
#' Predictors are selected based on the absolute correlation
#' between the target and the predictor. The procedure is fast and can
#' handle large datasets.
#'
#' Problem: Imputation by random draws from the marginal distribution
#' weakens the correlation structure in the data. This may lead to
#' suboptimal predictor selection. The procedure is best used for
#' exploratory data analysis.
#'
#' Potential extensions: The function can be extended to include other
#' imputation methods, to add support for categorical variables, and
#' to select the optimal number of variables by Mallow's Cp.
#'
#' @note \code{larspred()} uses \code{\link[base]{data.matrix}} to convert
#' factors to numbers through their internal codes. For unordered factors
#' the resulting quantification may not make sense.
#'
#' @param data Matrix or data frame with incomplete data.
#' @param type A string specifying the type of LARS algorithm. Use
#' \code{'lar'} (default) or \code{'lasso'}. Can be abbreviated.
#' @param s See \code{\link[lars:predict.lars]{predict.lars}} for details.
#' @param max.steps See \code{\link[lars:predict.lars]{predict.lars}} for
#' details.
#' @param ... Passed down to the \code{\link[lars:lars]{lars}} and
#' \code{\link[lars:coef.lars]{coef.lars}} functions.
#' @return A square binary matrix of size \code{ncol(data)}.
#' @author Stef van Buuren, June 2024
#' @examples
#' # include all variables
#' larspred(nhanes)
#'
#' # include two best variables
#' larspred(nhanes, s = 2)
#'
#' # limit to three best variables
#' larspred(boys, s = 3)
#' @export
larspred <- function(data, type = "lar",
                     s = ncol(data) - 1, max.steps = ncol(data) - 1, ...) {

  # use the lars package for variable selection
  install.on.demand("lars", ...)
  cond <- check.dataform(data)

  # initialize predictor matrix
  nvar <- ncol(data)
  ynames <- colnames(data)
  predictorMatrix <- matrix(0, nrow = nvar, ncol = nvar,
                            dimnames = list(ynames, ynames))

  # fill missings with random draws from observed data
  init <- mice(data, maxit = 0L, m = 1L, remove.collinear = FALSE, ...)
  xy <- data.matrix(complete(init, action = 1L))
  stopifnot(all(!any(is.na(xy))))

  # loop over incomplete variables, fit two lars models per target
  # fill predictorMatrix with selected variables
  for (yname in ynames) {
    y <- xy[, yname]
    ry <- !is.na(data[, yname])
    x <- xy[, yname != ynames]

    if (any(!ry)) {
      lars_y <- lars::lars(x = x, y = y, type = type, ...)
      lars_ry <- lars::lars(x = x, y = ry, type = type, ...)
      coef_y <- coef(lars_y, s = min(max.steps, s), ...)
      coef_ry <- coef(lars_ry, s = min(max.steps, s), ...)
      preds <- union(colnames(x)[coef_y != 0],
                     colnames(x)[coef_ry != 0])
      if (length(preds)) predictorMatrix[yname, preds] <- 1
    }
  }
  return(predictorMatrix)
}
