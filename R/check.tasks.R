check.tasks <- function(tasks, data, models = NULL, blocks = NULL) {
  # This function is called during initialization
  if (is.null(tasks)) {
    tasks <- "generate"
  }

  valid_tasks <- c("generate", "retain", "train", "apply")

  # 1. Default blocks to individual variables if not provided
  if (is.null(blocks)) {
    blocks <- setNames(as.list(names(data)), names(data))
  }

  # 2. Expand tasks if it's a single value
  bv <- unique(unlist(blocks))
  if (length(tasks) == 1) {
    tasks <- setNames(rep(tasks, length(bv)), bv)
  }

  # 3. Check length
  if (length(tasks) != length(bv)) {
    stop("The length of `tasks` (", length(tasks),
         ") must match the number of variables in `blocks` (", length(bv),").")
  }

  # 4. Check if all names in tasks exist in blocks
  notFound <- !names(tasks) %in% bv
  if (any(notFound)) {
    stop(paste0(
      "The following variables specified in `tasks` are not present in `blocks`: ",
      paste(names(tasks)[notFound], collapse = ", "), ".\n",
      "Ensure all specified variables match those in `blocks`."
    ))
  }

  # 5. Check if all tasks are valid
  invalid_ops <- setdiff(unique(tasks), valid_tasks)
  if (length(invalid_ops) > 0) {
    stop(paste0(
      "Invalid task(s) detected: ", paste(invalid_ops, collapse = ", "), ".\n",
      "Valid tasks are: ", paste(valid_tasks, collapse = ", "), ".\n",
      "Please correct the `tasks` argument."
    ))
  }

  # 6. Prevent "apply" if models is NULL
  if ("apply" %in% tasks && is.null(models)) {
    stop("The task 'apply' requires a stored model, but `models` is NULL.\n",
         "Please provide a valid `models` object containing trained imputation models.")
  }

  # 7. Ensure that all "apply" variables have a trained model in models
  if ("apply" %in% tasks && !is.null(models)) {
    fill_vars <- names(tasks[tasks == "apply"])
    missing_models <- setdiff(fill_vars, ls(models))
    if (length(missing_models) > 0) {
      stop(paste0(
        "The following variables specified as 'apply' do not have stored models: ",
        paste(missing_models, collapse = ", "), ".\n",
        "Ensure these variables were previously fitted before using 'apply'."
      ))
    }
  }

  # 8. Ensure all variables in models exist in blocks
  if (!is.null(models)) {
    trained_vars <- ls(models)
    missing_from_data <- setdiff(trained_vars, bv)  # Use block variable names
    if (length(missing_from_data) > 0) {
      stop(paste0(
        "The following variables are present in `models` but missing from `data`: ",
        paste(missing_from_data, collapse = ", "), ".\n",
        "Ensure that all stored models correspond to variables in the dataset."
      ))
    }
  }

  return(tasks)
}

check.model <- function(model,
                        task = c("generate", "retain", "train", "apply")) {
  # This function is called during iteration
  task <- match.arg(task)
  if (task %in% c("retain", "train", "apply")) {
    if (is.null(model)) {
      stop(paste("`model` cannot be NULL for task:", task))
    }
    if (!is.environment(model)) {
      stop("`model` must be an environment to store results persistently.")
    }
  }
  if (task == "apply" && !length(ls(model))) {
    stop("No stored model found for 'apply' task.")
  }
  return()
}
