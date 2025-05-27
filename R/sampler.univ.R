sampler.univ <- function(data, r, where, pred, formula, method, yname, k,
                         ct = "pred", user, ignore, ...) {
  j <- yname[1L]

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

  # get the model matrix
  x <- obtain.design(data, formula)

  # expand pred vector to model matrix, remove intercept
  if (ct == "pred") {
    type <- pred[labels(terms(formula))][attr(x, "assign")]
    x <- x[, -1L, drop = FALSE]
    names(type) <- colnames(x)
  }
  if (ct == "formula") {
    x <- x[, -1L, drop = FALSE]
    type <- rep(1L, length = ncol(x))
    names(type) <- colnames(x)
  }

  # define y, ry and wy
  y <- data[, j]
  ry <- complete.cases(x, y) & r[, j] & !ignore
  wy <- complete.cases(x) & where[, j]

  # nothing to impute
  if (all(!wy)) {
    return(numeric(0))
  }

  cc <- wy[where[, j]]
  if (k == 1L) check.df(x, y, ry)

  # remove linear dependencies
  keep <- remove.lindep(x, y, ry, ...)
  x <- x[, keep, drop = FALSE]
  type <- type[keep]
  if (ncol(x) != length(type)) {
    stop("Internal error: length(type) != number of predictors")
  }

  # here we go
  f <- paste("mice.impute", method, sep = ".")
  imputes <- data[wy, j]
  imputes[!cc] <- NA

  args <- c(list(y = y, ry = ry, x = x, wy = wy, type = type), user, list(...))
  imputes[cc] <- do.call(f, args = args)
  imputes
}
