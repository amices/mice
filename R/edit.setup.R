mice.edit.setup <- function(data, setup, tasks,
                            user.visitSequence = NULL,
                            allow.na = FALSE,
                            remove.constant = TRUE,
                            remove.collinear = TRUE,
                            remove_collinear = TRUE,
                            ...) {
  # legacy handling
  if (!remove_collinear) remove.collinear <- FALSE

  pred <- setup$predictorMatrix
  meth <- setup$method
  vis <- setup$visitSequence
  post <- setup$post

  if (ncol(pred) != nrow(pred) || length(meth) != nrow(pred) ||
      ncol(data) != nrow(pred)) {
    return(setup)
  }

  varnames <- colnames(data)

  for (j in seq_len(ncol(data))) {
    if (!is.passive(meth[j])) {
      d.j <- data[, j]
      task <- unname(tasks[varnames[j]])
      if (task != "fill") {
        v <- if (is.character(d.j)) NA else var(as.numeric(d.j), na.rm = TRUE)
        constant <- if (allow.na) {
          if (is.na(v)) FALSE else v < 1000 * .Machine$double.eps
        } else {
          is.na(v) || v < 1000 * .Machine$double.eps
        }
        if (constant && any(pred[, j] != 0) && remove.constant) {
          pred[, j] <- 0
          updateLog(out = varnames[j], meth = "constant", frame = 1)
          didlog <- TRUE
        }
        if (constant && meth[j] != "" && remove.constant) {
          pred[j, ] <- 0
          meth[j] <- ""
          vis <- vis[vis != j]
          post[j] <- ""
          if (!didlog) {
            updateLog(out = varnames[j], meth = "constant", frame = 1)
          }
        }
      }
    }
  }

  ## remove collinear variables
  ispredictor <- apply(pred != 0, 2, any)
  droplist <- if (any(ispredictor)) {
    find.collinear(data[, ispredictor, drop = FALSE], ...)
  } else {
    NULL
  }

  # do not drop variables with task "fill"
  droplist <- setdiff(droplist, names(tasks[tasks == "fill"]))

  if (length(droplist) > 0) {
    for (k in seq_along(droplist)) {
      j <- which(varnames %in% droplist[k])

      if (any(pred[, j] != 0) && remove.collinear) {
        pred[, j] <- 0
        updateLog(out = varnames[j], meth = "collinear", frame = 1)
        didlog <- TRUE
      }

      if (meth[j] != "" && remove.collinear) {
        pred[j, ] <- 0
        meth[j] <- ""
        vis <- vis[vis != j]
        post[j] <- ""
        if (!didlog) {
          updateLog(out = varnames[j], meth = "collinear", frame = 1)
        }
      }
    }
  }

  if (all(pred == 0L)) {
    stop("`mice` detected constant and/or collinear variables. No predictors were left after their removal.")
  }

  # Detect passive methods
  is_passive <- grepl("^~", meth)

  # By default, place passive at the end of the visitSequence
  if (is.null(user.visitSequence)) {
    active_vars <- vis[!is_passive]
    passive_vars <- vis[is_passive]
    vis <- c(active_vars, passive_vars)
  }

  setup$predictorMatrix <- pred
  setup$visitSequence <- vis
  setup$post <- post
  setup$method <- meth
  setup
}
