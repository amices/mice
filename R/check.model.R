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
  mnames <- names(model$beta.mis)
  if (is.matrix(model$beta.mis)) mnames <- rownames(model$beta.mis)
  dnames <- colnames(x)
  if (ncol(x) != length(mnames) || any(mnames != dnames)) {
    stop(paste("Model-Data mismatch: ", deparse(formula), "\n",
               " Model:", paste(mnames, collapse = " "), "\n",
               " Data: ", paste(dnames, collapse = " "), "\n"))
  }

  mmeth <- model$setup$method
  if (length(mmeth) && mmeth != method) {
    stop(paste("Model-Method mismatch: ", deparse(formula), "\n",
               " Model:  ", mmeth, "\n",
               " Method: ", method, "\n"))
  }
  return(TRUE)
}
