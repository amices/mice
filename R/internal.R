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

## make list of collinear variables to remove
find.collinear <- function(x, threshold = 0.999, ...) {
  nvar <- ncol(x)
  x <- data.matrix(x)
  r <- !is.na(x)
  nr <- apply(r, 2, sum, na.rm = TRUE)
  ord <- order(nr, decreasing = TRUE)
  xo <- x[, ord, drop = FALSE] ## SvB 24mar2011
  varnames <- dimnames(xo)[[2]]
  z <- suppressWarnings(cor(xo, use = "pairwise.complete.obs"))
  hit <- outer(seq_len(nvar), seq_len(nvar), "<") & (abs(z) >= threshold)
  out <- apply(hit, 2, any, na.rm = TRUE)

  # Inform user
  # if (any(out)) {
  #   updateLog(
  #     out = paste(paste(varnames[out], collapse = ", ")),
  #     meth = "collinear",
  #     frame = 2
  #   )
  # }

  return(varnames[out])
}

updateLog <- function(out = NULL, meth = NULL, msg = NULL, fn = NULL, frame = 1) {
  pos_state <- ma_exists("state", frame)$pos
  pos_loggedEvents <- ma_exists("loggedEvents", frame)$pos

  s <- get("state", pos_state)
  r <- get("loggedEvents", pos_loggedEvents)

  rec <- data.frame(
    it   = s$it,
    im   = s$im,
    dep  = s$dep,
    meth = if (is.null(meth)) s$meth else meth,
    out  = if (is.null(out)) "" else out,
    msg  = if (is.null(msg)) NA_character_ else msg,
    fn   = if (is.null(fn)) as.character(sys.call(-1)[1]) else NA_character_,
    stringsAsFactors = FALSE
  )

  if (s$log) {
    r <- rbind(r, rec)
  }

  s$log <- TRUE
  assign("state", s, pos = pos_state, inherits = TRUE)
  assign("loggedEvents", r, pos = pos_loggedEvents, inherits = TRUE)

  invisible(NULL)
}

# updateLog <- function(out = NULL, meth = NULL, frame = 1) {
#   # find structures defined a mice() level
#   pos_state <- ma_exists("state", frame)$pos
#   pos_loggedEvents <- ma_exists("loggedEvents", frame)$pos
#
#   s <- get("state", pos_state)
#   r <- get("loggedEvents", pos_loggedEvents)
#
#   rec <- data.frame(
#     it = s$it,
#     im = s$im,
#     dep = s$dep,
#     meth = if (is.null(meth)) s$meth else meth,
#     out = if (is.null(out)) "" else out
#   )
#
#   if (s$log) {
#     rec <- rbind(r, rec)
#   }
#   s$log <- TRUE
#   assign("state", s, pos = pos_state, inherits = TRUE)
#   assign("loggedEvents", rec, pos = pos_loggedEvents, inherits = TRUE)
#   return()
# }

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

