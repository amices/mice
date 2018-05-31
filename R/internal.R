
keep.in.model <- function(y, ry, x, wy) 
  (complete.cases(y, x) & ry) | (complete.cases(x) & wy)

impute.with.na <- function(x, wy) !complete.cases(x) & wy

check.df <- function(x, y, ry) {
  # if needed, writes the df warning message to the log
  df <- sum(ry) - ncol(x) - 1
  mess <- paste("df set to 1. # observed cases:", sum(ry), " # predictors:", ncol(x) + 1)
  if (df < 1 && sum(ry) > 0)
    updateLog(out = mess, frame = 4)
}

remove.lindep <- function(x, y, ry, eps = 1e-04, maxcor = 0.99, 
                          allow.na = TRUE, frame = 4, ...) {
  # returns a logical vector of length ncol(x)
  
  if (ncol(x) == 0)
    return(NULL)
  if (eps <= 0)
    stop("\n Argument 'eps' must be positive.")
  
  # Keep all predictors if we allow imputation of fully missing y
  if (allow.na && sum(ry) == 0) return(rep.int(TRUE, ncol(x)))
  
  xobs <- x[ry, , drop = FALSE]
  yobs <- as.numeric(y[ry])
  if (var(yobs) < eps) return(rep(FALSE, ncol(xobs)))
  
  keep <- unlist(apply(xobs, 2, var) > eps)
  keep[is.na(keep)] <- FALSE
  highcor <- suppressWarnings(unlist(apply(xobs, 2, cor, yobs) < maxcor))
  keep <- keep & highcor
  if (all(!keep))
    updateLog(out = "All predictors are constant or have too high correlation.", 
              frame = frame)
  
  # no need to calculate correlations, so return
  k <- sum(keep)
  if (k <= 1L) return(keep)  # at most one TRUE
  
  # correlation between x's
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
    updateLog(out = out, frame = frame)
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


updateLog <- function(out = NULL, meth = NULL, frame = 1) {
  
  # find structures defined a mice() level
  pos_state <- ma_exists("state", frame)$pos
  pos_loggedEvents <- ma_exists("loggedEvents", frame)$pos
  
  s <- get("state", pos_state)
  r <- get("loggedEvents", pos_loggedEvents)

  rec <- data.frame(it = s$it, 
                    im = s$im, 
                    dep = s$dep, 
                    meth = if(is.null(meth)) s$meth else meth, 
                    out = if (is.null(out)) "" else out)
  
  if (s$log)
    rec <- rbind(r, rec)
  s$log <- TRUE
  assign("state", s, pos = pos_state, inherits = TRUE)
  assign("loggedEvents", rec, pos = pos_loggedEvents, inherits = TRUE)
  return()
}

sym <- function(x) {(x + t(x)) / 2}


# This helper function was copied from
# https://github.com/alexanderrobitzsch/miceadds/blob/master/R/ma_exists.R
ma_exists <- function( x, pos, n_index=1:8)
{
  n_index <- n_index + 1
  is_there <- exists(x, where=pos)
  obj <- NULL
  if (is_there){
    obj <- get(x, pos)
  }
  if (! is_there){
    for (nn in n_index){
      pos <- parent.frame(n=nn)
      is_there <- exists(x, where=pos)
      if (is_there){
        obj <- get(x, pos)
        break
      }        
    }
  }
  #--- output
  res <- list( is_there=is_there, obj=obj, pos=pos)
  return(res)
}
