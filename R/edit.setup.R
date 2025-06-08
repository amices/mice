mice.edit.setup <- function(data, setup, tasks,
                            user.visitSequence = NULL,
                            allow.na = FALSE,
                            remove.constant = TRUE,
                            remove.collinear = TRUE,
                            remove_collinear = TRUE,
                            ...) {
  # If `remove.constant` is TRUE, removes constant variables
  # If `remove.collinear` is TRUE, removes collinear variables in predictors with tasks `impute` or `train`
  # If the user did not specify `visitSequence`, places any passive variable at the end of the sequence
  # Edits `predictorMatrix`, `visitSequence`, `post` and `method`
  # Updates `loggedEvents` per variable removed (for `collinear` and `constant`)
  #
  # Removed variables are not used or imputed by `mice`.

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
  ispredictor <- apply(pred != 0, 2, any)

  # remove constant variables, but keep passive or filled variables
  for (j in seq_len(ncol(data))) {
    if (!is.passive(meth[j])) {
      d.j <- data[, j]
      task <- unname(tasks[varnames[j]])
      if (task != "fill") {
        v <- if (is.character(d.j)) NA else var(as.numeric(d.j), na.rm = TRUE)
        isconstant <- if (allow.na) {
          if (is.na(v)) FALSE else v < 1000 * .Machine$double.eps
        } else {
          is.na(v) || v < 1000 * .Machine$double.eps
        }

        # remove j as predictor
        if (isconstant && ispredictor[j] && remove.constant) {
          pred[, j] <- 0
        }

        # FIXME length meth should be length(blocks)
        # remove j as dependent
        if (isconstant && meth[j] != "" && remove.constant) {
          pred[j, ] <- 0
          meth[j] <- ""
          vis <- vis[vis != j]
          post[j] <- ""
        }

        if (isconstant && remove.constant) {
          updateLog(dep = varnames[j], out = varnames[j], meth = "constant", frame = 1)
        }
      }
    }
  }

  ## remove collinear variables
  droplist <- NULL
  if (any(ispredictor) && remove.collinear) {
    droplist <- find.collinear(data[, ispredictor, drop = FALSE], ...)
  }

  # but keep variables with task "fill"
  keep_vars <- setdiff(droplist$out, names(tasks[tasks == "fill"]))
  droplist <- droplist[droplist$out %in% keep_vars, ]

  if (length(droplist$out)) {
    for (k in seq_along(droplist$out)) {
      i <- which(varnames %in% droplist$dep[k])
      j <- which(varnames %in% droplist$out[k])

      # remove j as predictor
      if (any(pred[, j] != 0) && remove.collinear) {
        pred[, j] <- 0
      }
      # remove j as dependent
      if (meth[j] != "" && remove.collinear) {
        pred[j, ] <- 0
        meth[j] <- ""
        vis <- vis[vis != j]
        post[j] <- ""
      }
      updateLog(dep = varnames[i], out = varnames[j], meth = "collinear", frame = 1)
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
