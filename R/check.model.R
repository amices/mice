check.model.exists <- function(model, task) {
  if (task == "impute") {
    return()
  }
  if (is.null(model) || !is.environment(model)) {
    stop("`model` must be an environment to store results persistently.")
  }
  return()
}

check.model.match <- function(model, x, method) {
  formula <- model$formula
  if (!length(formula)) {
    stop("No model stored in environment")
  }

  mmeth <- model$setup$method
  if (length(mmeth) && mmeth != method) {
    stop(paste("Model-Method mismatch: ", deparse(formula), "\n",
               " Model:  ", mmeth, "\n",
               " Method: ", method, "\n"))
  }

  xnames <- model$xnames
  dnames <- colnames(x)
  notfound <- !xnames %in% dnames
  if (any(notfound)) {
    stop(paste("Model-Data mismatch: ", deparse(formula), "\n",
               "Not found in data:   ", paste(xnames[notfound], collapse = " "), "\n"))
  }

  notfound <- !dnames %in% xnames
  return(!notfound)
}
