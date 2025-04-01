check.tasks <- function(tasks, data, models = NULL, blocks = NULL,
                        skip.check.tasks = FALSE) {
  if (skip.check.tasks) {
    return(tasks)
  }

  # This function is called during initialization
  if (is.null(tasks)) {
    tasks <- "impute"
  }

  valid_tasks <- c("impute", "train", "fill")

  # 1. Default blocks to individual variables if not provided
  if (is.null(blocks)) {
    blocks <- setNames(as.list(names(data)), names(data))
  }

  # 2. Expand tasks if it's a single value
  bv <- unique(unlist(blocks))
  if (length(tasks) == 1L) {
    tasks <- setNames(rep(tasks, length(bv)), bv)
  }

  # 3. Check length
  if (length(tasks) != length(bv)) {
    stop("The length of `tasks` (", length(tasks),
         ") must match the number of variables in `blocks` (", length(bv),").")
  }

  # 4. Check that tasks is a named vector
  if (is.null(names(tasks))) {
    # Attach names assuming same order as unique variables in blocks
    names(tasks) <- bv
  }

  # 5. Check if all names in tasks exist in blocks
  notFound <- !names(tasks) %in% bv
  if (any(notFound)) {
    stop(paste0(
      "The following variables specified in `tasks` are not present in `blocks`: ",
      paste(names(tasks)[notFound], collapse = ", "), ".\n",
      "Ensure all specified variables match those in `blocks`."
    ))
  }

  # 6. Check if all tasks are valid
  invalid_ops <- setdiff(unique(tasks), valid_tasks)
  if (length(invalid_ops) > 0L) {
    stop(paste0(
      "Invalid task(s) detected: ", paste(invalid_ops, collapse = ", "), ".\n",
      "Valid tasks are: ", paste(valid_tasks, collapse = ", "), ".\n",
      "Please correct the `tasks` argument."
    ))
  }

  # 7. Prevent "fill" if models is NULL
  if ("fill" %in% tasks && is.null(models)) {
    stop("The task 'fill' requires a stored model, but `models` is NULL.\n",
         "Please provide a valid `models` object with a trained imputation model.")
  }

  # 8. Ensure that all "fill" variables have a trained model in models
  if ("fill" %in% tasks && !is.null(models)) {
    fill_vars <- names(tasks[tasks == "fill"])
    missing_models <- setdiff(fill_vars, ls(models))
    if (length(missing_models) > 0L) {
      stop(paste0(
        "The following variables specified as 'fill' do not have stored models: ",
        paste(missing_models, collapse = ", "), ".\n",
        "Ensure these variables were previously fitted before using 'fill'."
      ))
    }
  }

  # 9. Ensure all variables in models exist in blocks
  if (!is.null(models)) {
    trained_vars <- ls(models)
    missing_from_data <- setdiff(trained_vars, bv)  # Use block variable names
    if (length(missing_from_data) > 0L) {
      stop(paste0(
        "The following variables are present in `models` but missing from `data`: ",
        paste(missing_from_data, collapse = ", "), ".\n",
        "Ensure that all stored models correspond to variables in the dataset."
      ))
    }
  }

  # 10. Check whether the data match the models for filling
  if ("fill" %in% tasks) {
    fill_vars <- names(tasks[tasks == "fill"])
    scanned <- scan.data(data, models)
    idx <- scanned$variable %in% fill_vars & !scanned$can_fill
    if (any(idx)) {
      stop(paste0(
        "The following variables in `data` do not match the stored models for filling: ",
        paste(scanned$variable[idx], collapse = ", "), ".\n",
        "Use scan.data() to diagnose mismatch between data and models."
      ))
    }
  }

  return(tasks)
}
