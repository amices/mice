edit.setup <- function(data, setup,
                       allow.na = FALSE,
                       remove.constant = TRUE,
                       remove.collinear = TRUE,
                       remove_collinear = TRUE,
                       ...) {
  # legacy handling
  if (!remove_collinear) remove.collinear <- FALSE

  # edits the imputation model setup
  # When it detec constant or collinear variables, write in loggedEvents
  # and continues imputation with reduced model

  pred <- setup$predictorMatrix
  meth <- setup$method
  vis <- setup$visitSequence
  post <- setup$post

  # FIXME: this function is not yet adapted to blocks
  if (ncol(pred) != nrow(pred) || length(meth) != nrow(pred)
  || ncol(data) != nrow(pred)) {
    return(setup)
  }

  varnames <- colnames(data)

  # remove constant variables but leave passive variables untouched
  for (j in seq_len(ncol(data))) {
    if (!is.passive(meth[j])) {
      d.j <- data[, j]
      v <- if (is.character(d.j)) NA else var(as.numeric(d.j), na.rm = TRUE)
      constant <- if (allow.na) {
        if (is.na(v)) FALSE else v < 1000 * .Machine$double.eps
      } else {
        is.na(v) || v < 1000 * .Machine$double.eps
      }
      didlog <- FALSE
      if (constant && any(pred[, j] != 0) && remove.constant) {
        out <- varnames[j]
        pred[, j] <- 0
        updateLog(out = out, meth = "constant")
        didlog <- TRUE
      }
      if (constant && meth[j] != "" && remove.constant) {
        out <- varnames[j]
        pred[j, ] <- 0
        if (!didlog) {
          updateLog(out = out, meth = "constant")
        }
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
  if (length(droplist) > 0) {
    for (k in seq_along(droplist)) {
      j <- which(varnames %in% droplist[k])
      didlog <- FALSE
      if (any(pred[, j] != 0) && remove.collinear) {
        # remove as predictor
        out <- varnames[j]
        pred[, j] <- 0
        updateLog(out = out, meth = "collinear")
        didlog <- TRUE
      }
      if (meth[j] != "" && remove.collinear) {
        out <- varnames[j]
        pred[j, ] <- 0
        if (!didlog) {
          updateLog(out = out, meth = "collinear")
        }
        meth[j] <- ""
        vis <- vis[vis != j]
        post[j] <- ""
      }
    }
  }

  if (all(pred == 0L)) {
    stop("`mice` detected constant and/or collinear variables. No predictors were left after their removal.")
  }

  setup$predictorMatrix <- pred
  setup$visitSequence <- vis
  setup$post <- post
  setup$method <- meth
  setup
}
