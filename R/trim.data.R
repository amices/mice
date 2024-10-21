#' Trims rows and columns of predictors before univariate imputation
#'
#' \code{trim.data()} returns two array of logicals, one for rows and one
#' for columns. The function filters out predictors that do not contribute to
#' the prediction of the target variable.
#' The user can select one of the following trimming functions:
#' \code{mice.trim.lindep()} (a wrapper of \code{remove.lindep()}),
#' \code{mice.trim.lars()}, \code{mice.trim.glmnet()},
#' \code{mice.trim.cv.glmnet()} or call a custom trimming function.
#'
#' @param y Numeric vector of length \code{length(y)} with the target variable.
#' If not numeric, it will be converted to numeric.
#' @param ry Logical vector of length \code{length(ry)} indicating which
#' observations are observed for the target variable.
#' @param x Numeric design matrix with predictors, for example, as produced
#' by \code{model.matrix()}. The matrix must have the same number of rows as
#' \code{length(y)} and \code{length(ry)}.
#' @param trimmer A string identifying the trimming function. The default is
#' \code{"lindep"}, which call \code{mice.trim.lindep()}. Other trimmers
#' include \code{"lars"}, \code{"glmnet"}, \code{"cv.glmnet"} or a custom
#' trimming function. Turn off trimming by \code{trimmer = ""}.
#' @param allow.na Logical. If \code{TRUE}, allow imputation of fully
#' missing \code{y}. Typically, this only occurs for passive imputation.
#' The default is \code{TRUE}.
#' @return A list with elements named \code{"rows"} and \code{"cols"},
#' logical vectors of lengths \code{nrow(x)} and \code{ncol(x)}, respectively.
#' @details
#' Filtering works on the design matrix \code{x}. The filter excludes columns
#' that do not contribute to the prediction of \code{y[ry]}. The filter
#' may omit columns of \code{x}. Removing a column that corresponds to a factor
#' level is equivalent to collapsing that level to the reference category.
#'
#' The function bypasses the column trimmer in the following cases:
#' \describe{
#'   \item{1}{If \code{y} is allowed to be fully missing.}
#'   \item{2}{If there are zero entries of \code{y} observed.}
#'   \item{3}{If there are zero or 1 predictors.}
#'   \item{4}{If the user specifies \code{trimmer = ""}.}
#' }
#'
#' Trimmers like \code{mice.trim.lars()} change the behaviour of the
#' MICE algorithm. They are more aggressive in removing predictors than
#' the classic \code{remove.lindep()} function. For backward compatibility,
#' set \code{trimmer = "lindep"} to your call like
#' \code{mice(..., trimmer = "lindep")}.
#' @rdname trim.data
#' @examples
#' # Impute according to old baheviour (remove.lindep())
#' imp <- mice(nhanes, m = 1, maxit = 1, print = FALSE, trimmer = "lindep")
#'
#' # Trim predictors using LARS
#' imp <- mice(nhanes, m = 1, maxit = 1, print = FALSE, trimmer = "lars")
#'
#' # Impute without a trim function
#' imp <- mice(nhanes, m = 1, maxit = 1, print = FALSE, trimmer = "")
#' @export
trim.data <- function(
    y, ry, x, trimmer = "lindep", allow.na = TRUE, ...) {
  stopifnot(is.matrix(x), is.logical(ry))
  stopifnot(length(y) == length(ry), nrow(x) == length(y))

  # handle exceptions to bypass trimming
  if (allow.na && sum(ry) == 0L ||
      sum(ry) <= 1L ||
      ncol(x) < 1L ||
      trimmer == "") {
    keep <- list(rows = ry & complete.cases(x, y),
                 cols = !logical(ncol(x)))
  } else if (trimmer == "lindep") {
    keep <- mice.trim.lindep(y, ry, x, ...)
  } else if (trimmer == "lars") {
    keep <- mice.trim.lars(y, ry, x, ...)
  } else if (trimmer == "glmnet") {
    keep <- mice.trim.glmnet(y, ry, x, ...)
  } else if (trimmer == "cv.glmnet") {
    keep <- mice.trim.cv.glmnet(y, ry, x, ...)
  } else {
    # handle user-specified trimmer
    args <- c(list(y = y, ry = ry, x = x), list(...))
    keep <- do.call(paste0("mice.trim.", trimmer), args = args)
  }

  return(keep)
}

#' Removes constant and multi-collinear predictors
#'
#' \code{mice.trim.lindep()} is a wrapper of \code{remove.lindep()}, the
#' classic MICE safety net to prevent multicollinearity and other numerical
#' problems.
#'
#' @param frame Integer. The frame number for logging. Do not alter.
#' @rdname trim.data
#' @export
mice.trim.lindep <- function(y, ry, x, frame = 5, ...) {
  xobs <- x[ry, , drop = FALSE]
  yobs <- as.numeric(y[ry])

  keep <- list(
    rows = complete.cases(x, y) & ry,
    cols = remove.lindep(x, y, ry, frame = frame,
                         expects.complete.x = FALSE, ...)
  )
  return(keep)
}

#' Include predictors by least angle regression (LARS)
#'
#' \code{trim.lars()} is a fast way to filter predictors by least angle
#' regression (LARS).
#'
#' @inheritParams lars::lars
#' @inheritParams mice.impute.pmm
#' @param lars.type Character. The type of model to fit. Could be
#' \code{"lar"}, \code{"lasso"}, \code{"forward.stagewise"} or
#' \code{"stepwise"}.
#' @param max.predictors Integer. The maximum number of variables to include.
#' The default \code{NULL} does not use a maximum.
#' @param lars.relax Numeric. Percent minimum Cp value that is
#' added to the minimum Cp to relax the inclusion threshold. The default is 5.
#' Use 1-5 percent for a slightly more permissive filter, 5-10 percent
#' for a moderate permissive filter, and 10-20 percent for very permissive.
#' @param minimal.cp Numeric. The minimum \code{"Cp"} value to consider.
#' \code{"Cp"} may become negative for small samples.
#' \code{minimal_cp} is the minimum \code{"Cp"} value used as to define the
#' threshold for the filter. Higher values select more predictors.
#' The default is 1.
#' @returns A logical vector of length \code{ncol(x)} indicating the predictors
#' to keep.
#' @details
#' \code{mice.trim.lars()} fits a LARS model to the elements of the design
#' matrix \code{x} and the target variable \code{y[ry]}. The procedure
#' removes rows with missing data before calling \code{lars()}.
#' @return A logical vector of length \code{ncol(x)} indicating which predictors
#' to keep.
#' @rdname trim.data
#' @export
mice.trim.lars <- function(
    y, ry, x,
    lars.type = c("lar", "lasso", "forward.stagewise", "stepwise"),
    max.predictors = NULL, lars.relax = 5, minimal.cp = 1, ...) {

  keep <- trim.preprocess(y, ry, x)
  if (!any(keep$cols) || !any(keep$rows)) {
    return(keep)
  }
  yobs <- as.numeric(y[keep$rows])
  xobs <- x[keep$rows, keep$cols, drop = FALSE]

  max.steps <- ifelse(is.null(max.predictors), ncol(xobs), max.predictors)
  model <- lars(x = xobs, y = yobs, type = lars.type, eps = eps,
                max.steps = max.steps)
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

  keep$cols <- coef_step != 0
  return(keep)
}

#' Filter out predictors by glmnet
#'
#' @inheritParams glmnet::glmnet
#' @rdname trim.data
#' @export
mice.trim.glmnet <- function(y, ry, x, dfmax = NULL, ...) {

  keep <- trim.preprocess(y, ry, x)
  if (!any(keep$cols) || !any(keep$rows)) {
    return(keep)
  }
  yobs <- as.numeric(y[keep$rows])
  xobs <- x[keep$rows, keep$cols, drop = FALSE]

  # select max.predictors predictors with LASSO
  dfmax <- ifelse(is.null(dfmax), ncol(x), dfmax)
  fit <- glmnet::glmnet(x = xobs, y = yobs, dfmax = dfmax, ...)
  valid_indices <- which(fit$df <= dfmax)
  closest_index <- valid_indices[which.max(fit$df[valid_indices])]
  lambda <- fit$lambda[closest_index]

  # select predictors
  coefs <- coef(fit, s = lambda)
  keep$cols <- as.logical(coefs[-1, ] != 0)
  return(keep)
}

#' Filter out predictors by cv.glmnet
#'
#' @inheritParams glmnet::glmnet
#' @rdname trim.data
#' @export
mice.trim.cv.glmnet <- function(y, ry, x, dfmax = NULL, ...) {

  keep <- trim.preprocess(y, ry, x)
  if (!any(keep$cols) || !any(keep$rows)) {
    return(keep)
  }
  yobs <- as.numeric(y[keep$rows])
  xobs <- x[keep$rows, keep$cols, drop = FALSE]

  # select predictors with cross-validation
  dfmax <- ifelse(is.null(dfmax), ncol(x), dfmax)
  fit <- glmnet::cv.glmnet(x = xobs, y = yobs, dfmax = dfmax, ...)
  lambda <- fit$lambda.min

  # select predictors
  coefs <- coef(fit, s = lambda)
  keep$cols <- as.logical(coefs[-1, ] != 0)
  return(keep)
}

trim.preprocess <- function(y, ry, x) {
  # Common actions for trimmers
  # 1) if y is constant, remove all predictors
  # 2) subset rows to obtain complete cases
  # Returns: rows, cols
  cols <- !logical(ncol(x))

  # Safety: If yobs is constant, predict from an intercept-only model
  yobs <- as.numeric(y[ry])
  if (var(yobs, na.rm = TRUE) < 1000 * .Machine$double.eps) {
    cols <- logical(ncol(x))
  }

  return(list(rows = ry & complete.cases(x, y),
              cols = cols))
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
#' matrix. The default is 1e-04. Setting \code{eps = 0} bypasses
#' \code{remove.lindep()} and returns all variables.
#' Note: In \code{lars.trimmer()} the \code{eps} argument has a different
#' meaning.
#' @param maxcor Numeric. The maximum correlation between a predictor and the
#' target variable. The default is 0.99.
#' @param frame Integer. The frame number for logging. Do not alter.
#' @param expects.complete.x Logical. If \code{TRUE}, the function expects
#' the data in \code{x} to be complete (used in mice <= 3.17.0).
#' If \code{FALSE}, the function uses only the observed values to calculate
#' (co)variances from incomplete data in \code{x}.
#' @details
#' \code{remove.lindep()} is the classic MICE safety net to prevent
#' multicollinearity and other numerical problems. It is a more
#' conservative filter than \code{trim.lars()}.
#' @rdname trim.data
#' @export
remove.lindep <- function(x, y, ry, eps = 1e-04, maxcor = 0.99,
                          frame = 4, expects.complete.x = TRUE, ...) {

  # handle.incomplete is a flag to indicate what to do with incomplete x
  if (expects.complete.x) {
    # classic remove.lindep
    na.rm <- FALSE
    use1 <- "everything"
    use2 <- "all.obs"
  } else {
    # more permissive remove.lindep
    na.rm <- TRUE
    use1 <- "pairwise.complete.obs"
    use2 <- "pairwise.complete.obs"
  }

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

  cols <- apply(xobs, 2, var, na.rm = na.rm) > 1000 * .Machine$double.eps
  cols[is.na(cols)] <- FALSE
  highcor <- suppressWarnings(apply(xobs, 2, cor, yobs, use = use1) < maxcor)
  cols <- cols & highcor
  if (all(!cols)) {
    updateLog(
      out = "All predictors are constant or have too high correlation.",
      frame = frame
    )
  }

  # no need to calculate correlations, so return
  k <- sum(cols)
  if (k <= 1L) {
    return(cols)
  } # at most one TRUE

  # correlation between x's
  cx <- cor(xobs[, cols, drop = FALSE], use = use2)
  eig <- eigen(cx, symmetric = TRUE)
  ncx <- cx
  while (eig$values[k] / eig$values[1] < abs(eps)) {
    j <- seq_len(k)[order(abs(eig$vectors[, k]), decreasing = TRUE)[1]]
    cols[cols][j] <- FALSE
    ncx <- cx[cols[cols], cols[cols], drop = FALSE]
    k <- k - 1
    eig <- eigen(ncx)
  }
  if (!all(cols)) {
    out <- paste(dimnames(x)[[2]][!cols], collapse = ", ")
    updateLog(out = out, frame = frame)
  }
  return(cols)
}
