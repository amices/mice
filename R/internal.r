keep.in.model <- function(y, ry, x, wy) 
  (complete.cases(y, x) & ry) | (complete.cases(x) & wy)

impute.with.na <- function(x, wy) !complete.cases(x) & wy

check.df <- function(x, y, ry) {
  # if needed, writes the df warning message to the log
  df <- sum(ry) - ncol(x) - 1
  mess <- paste("df set to 1. # observed cases:", sum(ry), " # predictors:", ncol(x) + 1)
  if (df < 1)
    updateLog(out = mess, frame = 3)
}

remove.lindep <- function(x, y, ry, eps = 1e-04, maxcor = 0.99, allow.na = FALSE, ...) {
  if (ncol(x) == 0)
    return(NULL)
  if (eps <= 0)
    stop("\n Argument 'eps' must be positive.")
  
  # Keep all predictors if we allow imputation of fully missing variable
  if (allow.na && sum(ry) == 0) {
    updateLog(out = "No observed outcomes, keep all predictors", frame = 3)
    return(rep.int(TRUE, ncol(x)))
  }
  
  xobs <- x[ry, , drop = FALSE]
  yobs <- as.numeric(y[ry])
  keep <- unlist(apply(xobs, 2, var) > eps)
  keep[is.na(keep)] <- FALSE
  highcor <- suppressWarnings(unlist(apply(xobs, 2, cor, yobs) < maxcor))
  keep <- keep & highcor
  if (all(!keep))
    updateLog(out = "All predictors are constant or have too high correlation.", frame = 3)
  if (length(keep) == 1) keep[1] <- TRUE
  k <- sum(keep)
  cx <- cor(xobs[, keep, drop = FALSE], use = "all.obs")
  eig <- eigen(cx, symmetric = TRUE)
  ncx <- cx
  while (eig$values[k]/eig$values[1] < eps) {
    j <- seq_len(k)[order(abs(eig$vectors[, k]), decreasing = TRUE)[1]]
    keep[keep][j] <- FALSE
    ncx <- cx[keep[keep], keep[keep], drop = FALSE]
    k <- k - 1
    eig <- eigen(ncx)
  }
  if (!all(keep)) {
    out <- paste(dimnames(x)[[2]][!keep], collapse = ", ")
    updateLog(out = out, frame = 3)
  }
  return(keep)
}


## make list of collinear variables to remove
find.collinear <- function(x, threshold = 0.999, ...) {
  nvar <- ncol(x)
  x <- data.matrix(x)
  r <- !is.na(x)
  nr <- apply(r, 2, sum, na.rm = TRUE)
  ord <- order(nr, decreasing = TRUE)
  xo <- x[, ord, drop = FALSE]  ## SvB 24mar2011
  varnames <- dimnames(xo)[[2]]
  z <- suppressWarnings(cor(xo, use = "pairwise.complete.obs"))
  hit <- outer(seq_len(nvar), seq_len(nvar), "<") & (abs(z) >= threshold)
  out <- apply(hit, 2, any, na.rm = TRUE)
  return(varnames[out])
}


updateLog <- function(out = NULL, meth = NULL, frame = 2) {
  s <- get("state", parent.frame(frame))
  r <- get("loggedEvents", parent.frame(frame))
  
  rec <- data.frame(it = s$it, im = s$im, co = s$co, dep = s$dep, meth = if(is.null(meth)) s$meth else meth, out = if (is.null(out)) "" else out)
  
  if (s$log)
    rec <- rbind(r, rec)
  s$log <- TRUE
  assign("state", s, pos = parent.frame(frame), inherits = TRUE)
  assign("loggedEvents", rec, pos = parent.frame(frame), inherits = TRUE)
  return()
}

sym <- function(x) {(x + t(x)) / 2}
