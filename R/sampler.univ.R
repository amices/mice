sampler.univ <- function(data, r, where, pred, formula, method, task, model,
                         yname, k, calltype = "pred", user, ignore,
                         trimmer = "lindep", ...) {
  j <- yname[1L]

  # nothing to impute
  if (all(!where[, j]) && task != "train") {
    return(numeric(0))
  }

  # prepare formula and model matrix
  formula <- prepare.formula(formula, data, model, j, calltype, pred, task)
  x <- obtain.design(data, formula)

  # expand pred vector to model matrix, remove intercept
  if (calltype == "pred") {
    type <- pred[labels(terms(formula))][attr(x, "assign")]
    x <- x[, -1L, drop = FALSE]
    names(type) <- colnames(x)
  }
  if (calltype == "formula") {
    x <- x[, -1L, drop = FALSE]
    type <- rep(1L, length = ncol(x))
    names(type) <- colnames(x)
  }

  # remove linear dependencies
  if (task != "fill") {
    keep <- trim.data(
      y = data[, j],
      ry = r[, j] & !ignore,
      x = x,
      trimmer = trimmer, ...
    )
  }

  # store the names of the features
  # xj <- unique(xnames[keep$cols])
  # print(xj)

  # set up univariate imputation method
  # wy: entries we wish to impute (length(y) elements)
  # iy: entries we will impute (sum(wy) elements)
  wy <- complete.cases(x) & where[, j]
  iy <- wy[where[, j]]

  # wipe out previous values
  imputes <- data[wy, j]
  imputes[!iy] <- NA

  # here we go
  f <- paste("mice.impute", method, sep = ".")
  args <- c(
    list(
      y = data[, j],
      ry = keep$rows,
      x = x[, keep$cols, drop = FALSE],
      wy = wy,
      type = type[keep$cols],
      task = task,
      model = model),
    user, list(...))
  imputes[iy] <- do.call(f, args = args)
  return(imputes)
}


prepare.formula <- function(formula, data, model, j, ct, pred, task) {
  # prepares the formula for univariate imputation
  # saves (for "train") or retrieves (for "fill") the formula

  # for "fill", use the stored formula instead of recalculating
  if (task == "fill") {
    if (!exists("formula", envir = model)) {
      stop("Error: No stored formula found in model for 'fill' task.")
    }
    formula <- get("formula", envir = model)
    return(as.formula(formula))
  }

  if (ct == "pred") {
    vars <- colnames(data)[pred != 0]
    xnames <- setdiff(vars, j)
    if (length(xnames) > 0L) {
      formula <- reformulate(backticks(xnames), response = backticks(j))
      formula <- update(formula, ". ~ . ")
    } else {
      formula <- as.formula(paste0(j, " ~ 1"))
    }
  }

  if (ct == "formula") {
    # move terms other than j from lhs to rhs
    ymove <- setdiff(lhs(formula), j)
    formula <- update(formula, paste(j, " ~ . "))
    if (length(ymove) > 0L) {
      formula <- update(formula, paste("~ . + ", paste(backticks(ymove), collapse = "+")))
    }
  }

  # store formula in `model` only when task is "train"
  if (task == "train") {
    assign("formula", paste(deparse(formula), collapse = ""), envir = model)
  }

  return(formula)
}
