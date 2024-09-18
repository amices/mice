#' Trim the predictor set before univariate imputation
#'
#' The function \code{trim.predictors()} filters out predictors before
#' univariate imputation. The function is a wrapper for the trimming
#' functions \code{lars.filter()}, \code{remove.lindep()}, or a user-specified
#' trimming function. The default method is \code{trimmer = "lars.filter"}.
#' The function is called by \code{mice:::sampler.univ()} and not intended
#' for direct application by the user.
#'
#' @param x Numeric design matrix with predictors, for example, as produced
#' by \code{model.matrix()}. The matrix must have the same number of rows as
#' \code{length(y)} and \code{length(ry)}.
#' @param y Numeric vector of length \code{length(y)} with the target variable.
#' If not numeric, it will be converted to numeric.
#' @param ry Logical vector of length \code{length(ry)} indicating which
#' observations are observed for the target variable.
#' @param trimmer A character vector of length 1 specifying the name of the
#' trimming function. The default is \code{"lars.filter"}. Other options are
#' \code{"remove.lindep"} or \code{""}. The user can also specify the name of a
#' custom trimming function.
#' @param allow.na Logical. If \code{TRUE}, allow imputation of fully
#' missing \code{y}. This typically only occurs for passive imputation.
#' The default is \code{TRUE}.
#' @return A logical vector of length \code{ncol(x)} indicating which predictors
#' to keep.
#' @details
#' The function \code{lars.filter()} changes the behavior of the MICE algorithm.
#' It is far more agressive in removing predictors than the classic
#' \code{remove.lindep()} function. For backward compatibility,
#' add \code{trimmer = "remove.lindep"} to your call
#' \code{mice(..., trimmer = "remove.lindep")}.
#'
#' Observe that filtering works on the design matrix \code{x}. If this
#' matrix contains dummy codings of categorical variables, the filter
#' will test the contribution of separate dummy variables to the model.
#' Implicitly, this practice changes the categories of the ancestor
#' factors. A neater approach that avoids this problem would be to include
#' all dummy codings of a categorical variable as a block. This is currently
#' not implemented.
#'
#' The current implementation only supports a global trimmer that applies
#' to all variables.
#' @rdname trim.predictors
#' @examples
#' # Impute using the default (LARS filter)
#' imp <- mice(nhanes, m = 1, maxit = 1, print = FALSE)
#'
#' # LARS with no more than two predictors per variable
#' imp <- mice(nhanes, m = 1, maxit = 1, print = FALSE, max.predictors = 2)
#'
#' # Impute using the remove.lindep filter (classic MICE)
#' imp <- mice(nhanes, m = 1, maxit = 1, print = FALSE, trimmer = "remove.lindep")
#'
#' # Impute without a filter
#' imp <- mice(nhanes, m = 1, maxit = 1, print = FALSE, trimmer = "")
#' @export
trim.predictors <- function(
    x, y, ry,
    trimmer = "lars.filter",
    allow.na = TRUE,
    ...) {
  stopifnot(is.matrix(x), is.logical(ry))
  stopifnot(length(y) == length(ry), nrow(x) == length(y))

  if (allow.na && sum(ry) == 0L ||
      sum(ry) <= 1L ||
      ncol(x) <= 1L ||
      trimmer == "") {
    # Exception 1: Keep all if we allow for a fully missing y
    # Exception 2: Keep all if there are no or only one observed y
    # Exception 3: Keep all if there is only zero or one predictor
    # Exception 4: Keep all if the user specifies trimmer = ""
    keep <- rep.int(TRUE, ncol(x))
  } else if (trimmer == "lars.filter") {
    keep <- lars.filter(x, y, ry, ...)
  } else if (trimmer == "remove.lindep") {
    keep <- remove.lindep(x, y, ry, ...)
  } else {
    # handle user-specified trimmer
    args <- c(list(x = x, y = y, ry = ry), list(...))
    keep <- do.call(trimmer, args = args)
  }

  return(keep)
}

#' Filter out predictors by least angle regression (LARS)
#'
#' \code{lars.filter()} is a fast way to filter out predictors by
#' least angle regression (LARS).
#'
#' @inheritParams mice.impute.pmm
#' @param ... Further arguments passed to \code{lars::lars}, like standard
#' LARS arguments \code{type}, \code{intercept}, \code{eps} and
#' \code{max.steps}, or tuning parameters \code{lars.relax} and
#' \code{minimal.cp}.
#' @details
#' \code{lars.filter} fits the LARS model to the elements of the design matrix
#' \code{x} and the target variable \code{y}, as indicated by \code{ry}.
#' If these data contain missing data, the function will remove the relevant
#' rows before calling \code{lars()}.
#' @rdname trim.predictors
#' @export
lars.filter <- function(x, y, ry,  ...) {

  # If y is constant, predict from an intercept-only model
  yobs <- as.numeric(y[ry])
  if (var(yobs, na.rm = TRUE) < 1000 * .Machine$double.eps) {
    return(rep(FALSE, ncol(x)))
  }

  # If needed, subset data because lars() requires complete data
  xobs <- x[ry, , drop = FALSE]
  if (anyNA(xobs) || anyNA(yobs)) {
    idx <- complete.cases(xobs, yobs)
    if (sum(idx) == 0L) {
      stop("The lars filter requires complete cases, but none were found.")
    }
    xobs <- xobs[idx, , drop = FALSE]
    yobs <- yobs[idx]
  }

  # Call the lars filter
  keep <- lars.internal(x = xobs, y = yobs, ...)
  return(keep)
}

#' Filter out predictors by least angular regression
#'
#' @inheritParams lars::lars
#' @param type Character. The type of LARS model to fit. The default is "lar".
#' @param max.predictors Integer. The maximum number of variables to include
#' in the LARS model. The default is 20.
#' @param lars.relax Numeric. The percentage of the minimum Cp value that is
#' added to the minimum Cp to relax the inclusion threshold. The default is 5.
#' Use 1-5 percent for a slightly more permissive filter, 5-10 percent
#' for a moderate permissive filter, and 10-20 percent for very permissive.
#' @param minimal.cp Numeric. The minimum "Cp" value to consider. "Cp" may
#' become negative for small samples. \code{minimal_cp} is the minimum "Cp" value
#' that is is used as to define the threshold for the filter. Higher values
#' select more predictors. The default is 1.
#' @rdname trim.predictors
#' @export
lars.internal <- function(
    x, y, type = "lar", intercept = TRUE, max.predictors = 20, eps = 1e-12,
    lars.relax = 5, minimal.cp = 1, ...) {
  model <- lars(x = x, y = y, type = type, intercept = intercept,
                eps = eps, max.steps = min(ncol(x), max.predictors))
  if (any(model$R2 == 1)) {
    # work-around because Cp gives NaN for perfect fits
    coef_step <- coef(model, s = which(model$R2 == 1))
  } else {
    # find the step where Cp is minimal
    cp <- model$Cp
    min_cp <- max(min(cp), minimal.cp) # SvB small sample adjustment
    threshold <- min_cp + (lars.relax * min_cp) / 100
    step <- tail(which(cp <= threshold), n = 1L)
    step <- ifelse(length(step), step, 1L)
    coef_step <- coef(model, s = step)
  }
  return(coef_step != 0)
}

#' Filter out constant and multi-collinear predictors before imputation
#'
#' \code{remove.lindep()} prevents multicollinearity
#' in the imputation model. It removes predictors that are constant or have
#' too high correlation with the target variable. The function
#' uses the eigenvalues of the correlation matrix to detect multicollinearity.
#'
#' @inheritParams mice.impute.pmm
#' @param eps Numeric. Used by \code{remove.lindep()} as the threshold for
#' the ratio of the smallest to the largest eigenvalue of the correlation
#' matrix. The default is 1e-04. If the user sets \code{eps = 0},
#' all variables are returned (for backward compatibility). Used by
#' \code{lars.internal()} as an argument to the \code{lars::lars()} function.
#' @param maxcor Numeric. The maximum correlation between a predictor and the
#' target variable. The default is 0.99.
#' @param frame Integer. The frame number for logging. Do not alter.
#' @details
#' \code{remove.lindep()} is the classic MICE safety net to prevent
#' multicollinearity and other numerical problems. It is a far more
#' conservative filter than \code{lars.filter()}. The function is called
#' by \code{trim.predictors()}. The function is not the default anymore, but
#' will remain part of the package for backward compatibility.
#' @rdname trim.predictors
#' @export
remove.lindep <- function(x, y, ry, eps = 1e-04, maxcor = 0.99,
                          frame = 4, ...) {
  # setting eps = 0 bypasses remove.lindep()
  # for compatibility with previous versions
  if (eps == 0) {
    return(rep.int(TRUE, ncol(x)))
  }

  xobs <- x[ry, , drop = FALSE]
  yobs <- as.numeric(y[ry])
  if (var(yobs) < abs(eps)) {
    return(rep(FALSE, ncol(xobs)))
  }

  keep <- unlist(apply(xobs, 2, var) > 1000 * .Machine$double.eps)
  keep[is.na(keep)] <- FALSE
  highcor <- suppressWarnings(unlist(apply(xobs, 2, cor, yobs) < maxcor))
  keep <- keep & highcor
  if (all(!keep)) {
    updateLog(
      out = "All predictors are constant or have too high correlation.",
      frame = frame
    )
  }

  # no need to calculate correlations, so return
  k <- sum(keep)
  if (k <= 1L) {
    return(keep)
  } # at most one TRUE

  # correlation between x's
  cx <- cor(xobs[, keep, drop = FALSE], use = "all.obs")
  eig <- eigen(cx, symmetric = TRUE)
  ncx <- cx
  while (eig$values[k] / eig$values[1] < abs(eps)) {
    j <- seq_len(k)[order(abs(eig$vectors[, k]), decreasing = TRUE)[1]]
    keep[keep][j] <- FALSE
    ncx <- cx[keep[keep], keep[keep], drop = FALSE]
    k <- k - 1
    eig <- eigen(ncx)
  }
  if (!all(keep)) {
    out <- paste(dimnames(x)[[2]][!keep], collapse = ", ")
    updateLog(out = out, frame = frame)
  }
  return(keep)
}
