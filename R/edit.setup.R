mice.edit.setup <- function(data, setup,
                       allow.na = FALSE,
                       remove.constant = TRUE,
                       remove.collinear = TRUE,
                       remove_collinear = TRUE,
                       ...) {
  # legacy handling
  if (!remove_collinear) remove.collinear <- FALSE

  # Procedure to detect constant or collinear variables
  #
  # If found:
  # - writes to loggedEvents
  # - edits predictorMatrix, method, formulas, visitSequence and post
  # - continues with reduced imputation model
  #
  # Specify remove.constant = FALSE and remove.collinear = FALSE to bypass
  # these checks and edits

  pred <- setup$predictorMatrix
  meth <- setup$method
  form <- setup$formulas
  dots <- setup$dots  # not used
  vis <- setup$visitSequence
  post <- setup$post

  # FIXME: need to generalise indexing and updating of meth, vis and post to blocks

  if (!validate.predictorMatrix(pred)) {
    warning("Problem with predictorMatrix detected in edit.setup()")
    return(setup)
  }

  varnames <- colnames(data)

  # remove constant variables but leave passive variables untouched
  for (j in seq_len(ncol(data))) {
    d.j <- data[, j]
    v <- if (is.character(d.j)) NA else var(as.numeric(d.j), na.rm = TRUE)
    constant <- if (allow.na) {
      if (is.na(v)) FALSE else v < 1000 * .Machine$double.eps
    } else {
      is.na(v) || v < 1000 * .Machine$double.eps
    }
    didlog <- FALSE
    if (constant && any(pred[, j] != 0) && remove.constant) {
      # inactivate j as predictor
      out <- varnames[j]
      pred[, j] <- 0
      updateLog(out = out, meth = "constant")
      didlog <- TRUE
    }
    if (constant && meth[j] != "" && remove.constant) {
      # inactivate j as dependent
      out <- varnames[j]
      pred[j, ] <- 0
      if (!didlog) {
        updateLog(out = out, meth = "constant")
      }
      form <- p2f(pred, blocks = construct.blocks(form, pred))
      # this following three statements do not work for blocks
      meth[j] <- ""
      vis <- vis[vis != j]
      post[j] <- ""
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
        form <- p2f(pred, blocks = construct.blocks(form, pred))
        meth[j] <- ""
        vis <- vis[vis != j]
        post[j] <- ""
      }
    }
  }

  if (!validate.predictorMatrix(pred)) {
    stop("Problem with predictorMatrix detected after edit.setup()")
  }

  setup$predictorMatrix <- pred
  setup$formulas <- form
  setup$dots <- dots
  setup$visitSequence <- vis
  setup$post <- post
  setup$method <- meth
  setup
}
