check.actions <- function(actions, data, models = NULL, blocks = NULL) {
  if (is.null(actions)) {
    actions <- "walk"
  }

  valid_actions <- c("walk", "train", "run")

  # 1. Default blocks to individual variables if not provided
  if (is.null(blocks)) {
    blocks <- setNames(as.list(names(data)), names(data))
  }

  # 2. Expand actions if it's a single value
  bv <- unique(unlist(blocks))
  if (length(actions) == 1) {
    actions <- setNames(rep(actions, length(bv)), bv)
  }

  # 3. Check length
  if (length(actions) != length(bv)) {
    stop("The length of `actions` (", length(actions),
         ") must match the number of variables in `blocks` (", length(bv),").")
  }

  # 4. Check if all names in actions exist in blocks
  notFound <- !names(actions) %in% bv
  if (any(notFound)) {
    stop(paste0(
      "The following variables specified in `actions` are not present in `blocks`: ",
      paste(names(actions)[notFound], collapse = ", "), ".\n",
      "Ensure all specified variables match those in `blocks`."
    ))
  }

  # 5. Check if all actions are valid
  invalid_ops <- setdiff(unique(actions), valid_actions)
  if (length(invalid_ops) > 0) {
    stop(paste0(
      "Invalid action(s) detected: ", paste(invalid_ops, collapse = ", "), ".\n",
      "Valid actions are: ", paste(valid_actions, collapse = ", "), ".\n",
      "Please correct the `actions` argument."
    ))
  }

  # 6. Prevent "run" if models is NULL
  if ("run" %in% actions && is.null(models)) {
    stop("The action 'fill' requires a stored model, but `models` is NULL.\n",
         "Please provide a valid `models` object containing trained imputation models.")
  }

  # 7. Ensure that all "run" variables have a trained model in models
  if ("run" %in% actions && !is.null(models)) {
    fill_vars <- names(actions[actions == "run"])
    missing_models <- setdiff(fill_vars, ls(models))
    if (length(missing_models) > 0) {
      stop(paste0(
        "The following variables specified as 'fill' do not have stored models: ",
        paste(missing_models, collapse = ", "), ".\n",
        "Ensure these variables were previously fitted before using 'fill'."
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

  return(actions)
}
