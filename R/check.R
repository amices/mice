# internal function for checking input to main mice() function

check.visitSequence <- function(setup, where) {
  
  nwhere <- setup$nwhere
  nimp <- setup$nimp
  nvar <- setup$nvar
  visitSequence <- setup$visitSequence
  blocks <- setup$blocks
  
  # set default visit sequence, left to right
  if (is.null(visitSequence))
    visitSequence <- seq_along(blocks)
  
  if (length(nimp) == 0) visitSequence <- nimp
  
  if (!is.numeric(visitSequence)) {
    code <- match.arg(visitSequence, c("roman", "arabic", "monotone",
                                       "revmonotone"))
    visitSequence <- switch(
      code, 
      roman = seq_along(blocks)[nimp > 0],
      arabic = rev(seq_along(blocks)[nimp > 0]),
      monotone = order(nimp)[(sum(nimp == 0) + 1):length(nimp)],
      revmonotone = rev(order(nimp)[(sum(nimp == 0) + 1):length(nimp)]),
      seq_len(nimp)[nimp > 0]
    )
  }
  
  flags <- (nimp == 0) & is.element(seq_along(blocks), visitSequence)
  if (any(flags)) visitSequence <- visitSequence[!flags]
  visitSequence <- visitSequence[visitSequence <= length(blocks)]
  visitSequence <- visitSequence[visitSequence >= 1]
  setup$visitSequence <- visitSequence
  return(setup)
}



check.method <- function(setup, data) {
  # check method, set defaults if appropriate
  blocks <- setup$blocks
  nwhere <- setup$nwhere
  nimp <- setup$nimp
  method <- setup$method
  defaultMethod <- setup$defaultMethod
  visitSequence <- setup$visitSequence
  nblo <- length(blocks)
  
  assign.method <- function(y) {
    if (is.numeric(y)) return(1)
    if (nlevels(y) == 2) return(2)
    if (is.ordered(y) && nlevels(y) > 2) return(4)
    if (nlevels(y) > 2) return(3)
    if (is.logical(y)) return(2)
    return(1)
  }
  
  # handle default, use method 1 if there is no single method 
  # within the block
  if (all(method == "")) {
    for (j in visitSequence) {
      yvar <- blocks[[j]]
      y <- data[, yvar]
      def <- sapply(y, assign.method)
      k <- ifelse(all(diff(def) == 0), k <- def[1], 1)
      method[j] <- defaultMethod[k]
    }
    method[nimp == 0] <- ""
  }
  
  # expand user's imputation method to all visited columns
  # single string supplied by user (implicit assumption of two columns)
  if (length(method) == 1) {
    if (is.passive(method))
      stop("Cannot have a passive imputation method for every column.")
    method <- rep(method, nblo)
    method[nimp == 0] <- ""
  }
  
  # if user specifies multiple methods, check the length of the argument
  if (length(method) != nblo) {
    stop(paste0("The length of method (", length(method),
                ") does not match the number of blocks (", nblo,
                ")."))
  }
  
  # check whether the requested imputation methods are on the search path
  active.check <- !is.passive(method) & nimp > 0 & method != ""
  passive.check <- is.passive(method) & nimp > 0 & method != ""
  check <- all(active.check) & any(passive.check)
  if (check) {
    fullNames <- rep.int("mice.impute.passive", length(method[passive.check]))
  } else {
    fullNames <- paste("mice.impute", method[active.check], sep = ".")
    if (length(method[active.check]) == 0) fullNames <- character(0)
  }
  notFound <- !vapply(fullNames, exists, logical(1), 
                      mode = "function", inherits = TRUE)
  if (any(notFound)) {
    stop(paste("The following functions were not found:",
               paste(fullNames[notFound], collapse = ", ")))
  }
  
  # type checks on built-in imputation methods
  for (j in visitSequence) {
    vname <- blocks[[j]]
    y <- data[, vname, drop = FALSE]
    mj <- method[j]
    mlist <- list(m1 = c("logreg", "logreg.boot", "polyreg", "lda", "polr"), 
                  m2 = c("norm", "norm.nob", "norm.predict", "norm.boot",
                         "mean", "2l.norm", "2l.pan",
                         "2lonly.pan", "quadratic", "ri"), 
                  m3 = c("norm", "norm.nob", "norm.predict", "norm.boot",
                         "mean", "2l.norm", "2l.pan", 
                         "2lonly.pan", "quadratic", "logreg", "logreg.boot"))
    cond1 <- sapply(y, is.numeric)
    cond2 <- sapply(y, is.factor) & sapply(y, nlevels) == 2 
    cond3 <- sapply(y, is.factor) & sapply(y, nlevels) > 2
    if (any(cond1) && mj %in% mlist$m1)
      warning("Type mismatch for variable(s): ", 
              paste(vname[cond1], collapse = ", "),
              "\nImputation method ", mj, " is for categorical data.",
              call. = FALSE)
    if (any(cond2) && mj %in% mlist$m2)
      warning("Type mismatch for variable(s): ", 
              paste(vname[cond2], collapse = ", "),
              "\nImputation method ", mj, " is not for factors.", 
              call. = FALSE)
    if (any(cond3) && mj %in% mlist$m3)
      warning("Type mismatch for variable(s): ", 
              paste(vname[cond3], collapse = ", "),
              "\nImputation method ", mj, " is not for factors with >2 levels.",
              call. = FALSE)
  }
  setup$method <- method
  setup
}


check.predictorMatrix <- function(setup) {
  ## checks and makes consistency edits of the predictormatrix
  blocks <- setup$blocks
  nimp <- setup$nimp
  pred <- setup$predictorMatrix
  varnames <- setup$varnames
  nwhere <- setup$nwhere
  nvar <- setup$nvar
  vis <- setup$visitSequence
  post <- setup$post
  nblo <- length(blocks)
  
  if (!is.matrix(pred))
    stop("Argument 'predictorMatrix' not a matrix.")
  if (nblo != nrow(pred))
    stop(paste0("The predictorMatrix has ", nrow(pred), 
                " rows. This should be ", nblo, "."))
  if (nvar != ncol(pred))
    stop(paste0("The predictorMatrix has ", ncol(pred), 
               " columns. This should be ", nvar, "."))
  dimnames(pred) <- list(names(blocks), varnames)
  
  # inactivate predictors of complete (or not imputed) block
  for (j in seq_along(blocks)) {
    if (nimp[j] == 0) pred[j, ] <- 0
  }
  
  setup$predictorMatrix <- pred
  setup$post <- post
  return(setup)
}


check.data <- function(setup, data, allow.na = FALSE, 
                       remove_collinear = TRUE, ...) {
  
  pred <- setup$predictorMatrix
  nvar <- setup$nvar
  varnames <- setup$varnames
  meth <- setup$method
  vis <- setup$visitSequence
  post <- setup$post
  
  # stop if the class variable is a factor
  isclassvar <- apply(pred == -2, 2, any)
  for (j in seq_len(nvar)) {
    if (isclassvar[j] && is.factor(data[,j])) 
      stop("Class variable (column ", j,
           ") cannot be factor. Convert to numeric by as.integer()")
  }
  
  # remove constant variables but leave passive variables untouched
  for (j in seq_len(nvar)) {
    if (!is.passive(meth[j])) {
      d.j <- data[, j]
      v <- if (is.character(d.j)) NA else var(as.numeric(d.j), na.rm = TRUE)
      constant <- if (allow.na) {
        if (is.na(v)) FALSE else v < 1000 * .Machine$double.eps
      } else {
        is.na(v) || v < 1000 * .Machine$double.eps
      }
      didlog <- FALSE
      if (constant && any(pred[, j] != 0)) {
        out <- varnames[j]
        pred[, j] <- 0
        updateLog(out = out, meth = "constant")
        didlog <- TRUE
      }
      if (constant && meth[j] != "") {
        out <- varnames[j]
        pred[j, ] <- 0
        if (!didlog)
          updateLog(out = out, meth = "constant")
        meth[j] <- ""
        vis <- vis[vis != j]
        post[j] <- ""
      }
    }
  }
  
  ## remove collinear variables
  ispredictor <- apply(pred != 0, 2, any)
  if (any(ispredictor)) {
    droplist <- find.collinear(data[, ispredictor, drop = FALSE], ...)
  } else {
    droplist <- NULL
  }
  if (length(droplist) > 0 && remove_collinear) {
    for (k in seq_along(droplist)) {
      j <- which(varnames %in% droplist[k])
      didlog <- FALSE
      if (any(pred[, j] != 0)) {
        # remove as predictor
        out <- varnames[j]
        pred[, j] <- 0
        updateLog(out = out, meth = "collinear")
        didlog <- TRUE
      }
      if (meth[j] != "") {
        out <- varnames[j]
        pred[j, ] <- 0
        if (!didlog)
          updateLog(out = out, meth = "collinear")
        meth[j] <- ""
        vis <- vis[vis != j]
        post[j] <- ""
      }
    }
  }
  
  setup$predictorMatrix <- pred
  setup$visitSequence <- vis
  setup$post <- post
  setup$meth <- meth
  return(setup)
}
