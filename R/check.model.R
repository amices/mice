check.model.exists <- function(model, task) {
  if (task == "impute") {
    return()
  }
  if (is.null(model) || !is.environment(model)) {
    stop("`model` must be an environment to store results persistently.")
  }
  if (task == "fill" && !length(ls(model))) {
    stop("No stored model found for 'fill' task.")
  }
  return()
}

check.model.data.match <- function(model, x) {
  formula <- model$formula
  if (!length(formula)) {
    stop("No model stored in environment")
  }
  mnames <- rownames(model$beta.mis)
  dnames <- colnames(x)
  if (ncol(x) != nrow(model$beta.mis) || any(mnames != dnames)) {
    stop(paste("Model-Data mismatch: ", deparse(formula), "\n",
               " Model:", paste(mnames, collapse = " "), "\n",
               " Data: ", paste(dnames, collapse = " "), "\n"))
  }
  return(TRUE)
}
