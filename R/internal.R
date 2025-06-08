keep.in.model <- function(y, ry, x, wy) {
  (complete.cases(y, x) & ry) | (complete.cases(x) & wy)
}


impute.with.na <- function(x, wy) !complete.cases(x) & wy

check.df <- function(x, y, ry) {
  # If needed, writes the df warning message to the log
  df <- sum(ry) - ncol(x) - 1
  mess <- paste("df set to 1. # observed cases:", sum(ry), " # predictors:", ncol(x) + 1)
  if (df < 1 && sum(ry) > 0) {
    updateLog(
      out = mess,
      frame = 4)
  }
}

find.collinear <- function(x, threshold = 0.999, ...) {
  nvar <- ncol(x)
  x <- data.matrix(x)
  r <- !is.na(x)
  nr <- apply(r, 2, sum, na.rm = TRUE)
  ord <- order(nr, decreasing = TRUE)
  xo <- x[, ord, drop = FALSE]  # reorder columns by data completeness
  varnames <- colnames(xo)
  z <- suppressWarnings(cor(xo, use = "pairwise.complete.obs"))

  # Find highly correlated pairs (upper triangle only)
  hit <- outer(seq_len(nvar), seq_len(nvar), "<") & (abs(z) >= threshold)
  collinear_pairs <- which(hit, arr.ind = TRUE)

  # Create data.frame of variable name pairs
  df <- data.frame(
    dep = varnames[collinear_pairs[, 1]],
    out = varnames[collinear_pairs[, 2]],
    stringsAsFactors = FALSE
  )

  return(df)
}

updateLog <- function(dep = NULL, out = NULL, meth = NULL, fn = NULL, frame = 1) {
  pos_state <- ma_exists("state", frame)$pos
  pos_loggedEvents <- ma_exists("loggedEvents", frame)$pos

  s <- get("state", pos_state)
  r <- get("loggedEvents", pos_loggedEvents)

  rec <- data.frame(
    it   = s$it,
    im   = s$im,
    dep  = if (is.null(dep)) s$dep else dep,
    meth = if (is.null(meth)) s$meth else meth,
    fn   = if (is.null(fn)) as.character(sys.call(-1)[1]) else NA_character_,
    out  = if (is.null(out)) "" else out,
    stringsAsFactors = FALSE
  )

  # r$im == s$im is critical to evade repeated entries
  r <- rbind(r[r$im == s$im, ], rec)
  assign("loggedEvents", r, pos = pos_loggedEvents, inherits = TRUE)

  invisible(NULL)
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
#' When `logenv` is not provided, `record.event()` calls `mice:::updateLog()` using the legacy
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
record.event <- function(out = NULL, meth = NULL, frame = 1, logenv = NULL) {
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

    state.entry <- data.frame(
      it = s$it,
      im = s$im,
      dep = s$dep,
      meth = if (is.null(meth)) s$meth else meth,
      out = if (is.null(out)) "" else out,
      stringsAsFactors = FALSE
    )

    logenv$log <- rbind(logenv$log, state.entry)
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
ma_exists <- function( x, pos, n_index = 1:8)
{
  n_index <- n_index + 1
  is_there <- exists(x, where = pos)
  obj <- NULL
  nn <- 0
  if (is_there){
    obj <- get(x, pos)
  }
  if (!is_there){
    for (nn in n_index){
      pos <- parent.frame(n = nn)
      is_there <- exists(x, where = pos)
      if (is_there){
        obj <- get(x, pos)
        break
      }
    }
  }
  #--- output
  res <- list(is_there = is_there, obj = obj, pos = pos, n = nn)
  return(res)
}


backticks <- function(varname) {
  sprintf("`%s`", varname)
}

sweep_operator <- function(S, k) {
  A <- S[k, k]
  B <- matrix(S[-k, k], ncol = 1L)
  C <- matrix(S[k, -k], nrow = 1L)
  D <- S[-k, -k] - B %*% C / A

  # Update the matrix S in place
  S[-k, -k] <- D
  S[-k, k] <- -B / A
  S[k, -k] <- -t(C) / A
  S[k, k] <- 1 / A

  return(S)
}

is.named.list <- function(x) {
  is.list(x) && !is.null(names(x)) && all(names(x) != "")
}


sanitize.vec <- function(vec, y) {
  # Insert at the end of any draw() or imputation function
  # # Example for logreg.draw()
  # vec <- logreg.draw(lp)
  # vec <- sanitize.vec(vec, y)

  cls <- class(y)[1L]

  if (cls == "logical") {
    return(as.logical(vec))
  }

  if (cls == "factor") {
    return(factor(vec, levels = levels(y), ordered = is.ordered(y)))
  }

  if (cls == "ordered") {
    return(factor(vec, levels = levels(y), ordered = TRUE))
  }

  # default (numeric, character, etc.)
  vec
}

