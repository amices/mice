keep.in.model <- function(y, ry, x, wy) {
  (complete.cases(y, x) & ry) | (complete.cases(x) & wy)
}


impute.with.na <- function(x, wy) !complete.cases(x) & wy


check.df <- function(x, y, ry) {
  # If needed, writes the df warning message to the log
  df <- sum(ry) - ncol(x) - 1
  mess <- paste("df set to 1. # observed cases:", sum(ry), " # predictors:", ncol(x) + 1)
  if (df < 1 && sum(ry) > 0) {
    logEvent(out = mess, frame = 4)
  }
}

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
    logEvent(
      out = "All predictors are constant or have too high correlation.",
      frame = frame
    )
  }

  k <- sum(keep)
  if (k <= 1L) {
    return(keep)
  }

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
    logEvent(out = out, frame = frame)
  }

  return(keep)
}


## make list of collinear variables to remove
find.collinear <- function(x, threshold = 0.999, logenv = NULL, ...) {
  nvar <- ncol(x)
  x <- data.matrix(x)
  r <- !is.na(x)
  nr <- apply(r, 2, sum, na.rm = TRUE)
  ord <- order(nr, decreasing = TRUE)
  xo <- x[, ord, drop = FALSE]
  varnames <- dimnames(xo)[[2]]

  z <- suppressWarnings(cor(xo, use = "pairwise.complete.obs"))
  hit <- outer(seq_len(nvar), seq_len(nvar), "<") & (abs(z) >= threshold)
  out <- apply(hit, 2, any, na.rm = TRUE)

  if (any(out)) {
    logEvent(
      out = paste(paste(varnames[out], collapse = ", ")),
      meth = "collinear",
      logenv = logenv
    )
  }

  return(varnames[out])
}

updateLog <- function(out = NULL, meth = NULL, frame = 1) {
  # find structures defined a mice() level
  pos_state <- ma_exists("state", frame)$pos
  pos_loggedEvents <- ma_exists("loggedEvents", frame)$pos

  s <- get("state", pos_state)
  r <- get("loggedEvents", pos_loggedEvents)

  rec <- data.frame(
    it = s$it,
    im = s$im,
    dep = s$dep,
    meth = if (is.null(meth)) s$meth else meth,
    out = if (is.null(out)) "" else out
  )

  if (s$log) {
    rec <- rbind(r, rec)
  }
  s$log <- TRUE
  assign("state", s, pos = pos_state, inherits = TRUE)
  assign("loggedEvents", rec, pos = pos_loggedEvents, inherits = TRUE)
  return()
}

#' Record an event in the imputation log
#'
#' Records a logged event during the imputation process. This function is the
#' modern replacement for `updateLog()`, supporting both sequential and
#' parallel execution through an explicit logging environment.
#'
#' @param out A character string describing the event or the affected variable.
#' @param meth Optional method name associated with the event. If `NULL`, the method
#'   from the current logging state will be used.
#' @param logenv Optional logging environment to store log entries and the current state.
#'   If not supplied, the function attempts to log using `updateLog()` as fallback.
#' @param frame Stack frame level used in fallback mode when `logenv` is not available.
#'   Default is `2`.
#'
#' @return This function is called for its side effects. It returns `invisible(NULL)`.
#'
#' @details
#' If `logenv` is provided and contains a valid logging state (`logenv$state`),
#' the function appends a log entry to `logenv$log` and sets `logenv$state$log <- TRUE`.
#' This is compatible with parallel execution using the `future` framework.
#'
#' When `logenv` is not provided, `logEvent()` calls `mice:::updateLog()` using the legacy
#' frame-based mechanism.
#'
#' @section Advanced:
#' This function is intended primarily for package authors and developers of
#' custom imputation methods that integrate with the `mice` framework. It replaces
#' the internal `updateLog()` function to support structured, parallel-safe logging.
#'
#' @seealso [mice()]
#'
#' @export
logEvent <- function(out = NULL, meth = NULL, frame = 1, logenv = NULL) {
  # If no logenv passed explicitly, try .logenv from globalenv
  if (is.null(logenv)) {
    logenv <- tryCatch(get(".logenv", envir = .GlobalEnv), error = function(e) NULL)
  }

  if (is.environment(logenv) && exists("state", envir = logenv, inherits = FALSE)) {
    s <- get("state", envir = logenv, inherits = FALSE)

    if (!exists("log", envir = logenv, inherits = FALSE)) {
      logenv$log <- data.frame(
        it = integer(), im = integer(), dep = character(),
        meth = character(), out = character(),
        stringsAsFactors = FALSE
      )
    }

    new_entry <- data.frame(
      it = s$it,
      im = s$im,
      dep = s$dep,
      meth = if (is.null(meth)) s$meth else meth,
      out = if (is.null(out)) "" else out,
      stringsAsFactors = FALSE
    )

    logenv$log <- rbind(logenv$log, new_entry)
    s$log <- TRUE
    assign("state", s, envir = logenv)
  } else {
    # Fallback: original behavior
    updateLog(out = out, meth = meth, frame = frame + 1)
  }

  invisible(NULL)
}

sym <- function(x) {
  (x + t(x)) / 2
}


# This helper function was copied from
# https://github.com/alexanderrobitzsch/miceadds/blob/master/R/ma_exists.R
ma_exists <- function(x, pos, n_index = 1:8) {
  n_index <- n_index + 1
  is_there <- exists(x, where = pos)
  obj <- NULL
  if (is_there) {
    obj <- get(x, pos)
  }
  if (!is_there) {
    for (nn in n_index) {
      pos <- parent.frame(n = nn)
      is_there <- exists(x, where = pos)
      if (is_there) {
        obj <- get(x, pos)
        break
      }
    }
  }
  #--- output
  res <- list(is_there = is_there, obj = obj, pos = pos)
  return(res)
}

backticks <- function(varname) {
  sprintf("`%s`", varname)
}

is.named.list <- function(x) {
  is.list(x) && !is.null(names(x)) && all(names(x) != "")
}
