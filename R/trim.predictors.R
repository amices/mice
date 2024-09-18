#' Trim predictors of the imputation model
#'
#' The function `trim.predictors()` filters out predictors before imputation
#' of univariate incomplete variables. The function is called by
#' `mice:::sampler.univ()`. The function is a wrapper for the trimming
#' functions `lars.filter()`, `remove.lindep()`, and a user-specified
#' trimming function. The default method is `"lars.filter"`.
#'
#' @inheritParams mice.impute.pmm
#' @param trimmer A character vector of length 1 specifying the name of the
#' trimming function. The default is "lars.filter". The other options are
#' "remove.lindep" and "none". The user can also specify a custom trimming
#' function.
#' @param ... further arguments passed to the trimming function.
#' @return A logical vector of length `ncol(x)` indicating which predictors
#' to keep.
#' @export
trim.predictors <- function(
    x, y, ry,
    trimmer = c("lars.filter", "remove.lindep", "none"), ...) {

  if (trimmer == "lars.filter") {
    keep <- lars.filter(x, y, ry, ...)
    return(keep)
  }

  if (trimmer == "remove.lindep") {
    keep <- remove.lindep(x, y, ry, ...)
    return(keep)
  }

  if (trimmer == "none") {
    keep <- rep.int(TRUE, ncol(x))
    return(keep)
  }

  # handle user-specified trimmer
  args <- c(list(x = x, y = y, ry = ry), list(...))
  keep <- do.call(trimmer, args = args)
  return(keep)
}

#' Filter out predictors by least angular regression
#'
#' @inheritParams mice.impute.pmm
#' @param eps numeric. Criteriom for removing predictors with zero variance.
#' If `eps` is zero, the filter is bypassed. The default is 1e-04.
#' @param allow.na logical. If `TRUE`, allow imputation of fully missing `y`.
#' This typically only occurs for passive imputation. The default is `TRUE`.
#' @param ... further arguments passed to `lars`, like standard arguments
#' `type` and `intercept`, or custom tuning parameters like `lars.relax` and
#' `minimal.cp`.
#' @return A logical vector of length `ncol(x)` indicating which predictors
#' to keep.
#' @details
#' In the following conditions, the function skips the filter
#' 1. if there are no or only one predictors
#' 2. if user imputes a fully missing y accept all predictors
#' 3. if eps == 0 (for backward compatibility)
#'
#' @export
lars.filter <- function(x, y, ry, eps = 1e-04, allow.na = TRUE, ...) {
  # Skipping conditions
  if (ncol(x) <= 1L ||
      allow.na && sum(ry) == 0L ||
      eps == 0) {
    return(rep.int(TRUE, ncol(x)))
  }

  # If y is constant, predict from an intercept-only model
  yobs <- as.numeric(y[ry])
  if (var(yobs, na.rm = TRUE) < abs(eps)) {
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
#' @param lars.relax numeric. The percentage of the minimum Cp value that is
#' added to the minimum Cp to relax the inclusion threshold. The default is 5.
#' Use 1-5 percent for a more stringent filter, and 5-10 percent for a moderate
#' filter.
#' @param minimal.cp numeric. The minimum "Cp" value to consider. "Cp" may
#' become negative for small samples. `minimal_cp` is the minimum "Cp" value
#' that is is used as to define the threshold for the filter. Higher value
#' select more predictors. The default is 1.
#' @param \dots Further arguments passed to `lars`. Nothing passed down.
#' @return A logical vector of length `ncol(x)` indicating which predictors
#' to keep.
#' @export
lars.internal <- function(x, y, type = "lar", intercept = TRUE,
                          lars.relax = 5, minimal.cp = 1, ...) {
  model <- lars(x = x, y = y, type = type, intercept = intercept)
  if (any(model$R2 == 1)) {
    # we need a work-around because Cp gives NaN
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

#' Filter out constant and multicollinear predictors before imputations
#'
#' `remove.lindep()` prevents multicollinearity
#' in the imputation model. It removes predictors that are constant or have
#' too high correlation with the target variable. The function
#' uses the eigenvalues of the correlation matrix to detect multicollinearity.
#'
#' @inheritParams mice.impute.pmm
#' @param eps numeric. The threshold for the ratio of the smallest to the
#' largest eigenvalue of the correlation matrix. The default is 1e-04. If the
#' user sets `eps = 0`, the function bypasses the filter.
#' @param maxcor numeric. The maximum correlation between a predictor and the
#' target variable. The default is 0.99.
#' @param allow.na logical. If `TRUE`, allow imputation of fully missing `y`.
#' This typically only occurs for passive imputation. The default is `TRUE`.
#' @param frame integer. The frame number for logging. The default is 4.
#' @return A logical vector of length `ncol(x)` indicating which predictors
#' to keep.
#' @details
#' This function is the classic MICE safety net to prevent multicollinearity
#' and other numerical problems. It is a far more conservative filter than
#' `lars.filter()`. The function is called by `trim.predictors()` with the
#' default method `"remove.lindep"`. The function is now deprecated, but
#' will remain part of the package for backward compatibility.
#' @export
remove.lindep <- function(x, y, ry, eps = 1e-04, maxcor = 0.99,
                          allow.na = TRUE, frame = 4, ...) {
  # returns a logical vector of length ncol(x)

  if (ncol(x) == 0) {
    return(NULL)
  }

  # setting eps = 0 bypasses remove.lindep()
  if (eps == 0) {
    return(rep.int(TRUE, ncol(x)))
  }
  if (eps < 0) {
    stop("\n Argument 'eps' must be positive.")
  }

  # Keep all predictors if we allow imputation of fully missing y
  if (allow.na && sum(ry) == 0) {
    return(rep.int(TRUE, ncol(x)))
  }

  xobs <- x[ry, , drop = FALSE]
  yobs <- as.numeric(y[ry])
  if (var(yobs) < eps) {
    return(rep(FALSE, ncol(xobs)))
  }

  keep <- unlist(apply(xobs, 2, var) > eps)
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
  while (eig$values[k] / eig$values[1] < eps) {
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
