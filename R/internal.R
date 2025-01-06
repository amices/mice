keep.in.model <- function(y, ry, x, wy) {
  (complete.cases(y, x) & ry) | (complete.cases(x) & wy)
}


impute.with.na <- function(x, wy) !complete.cases(x) & wy

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
