check.operations <- function(operations, data, models = NULL, blocks = NULL) {
  if (is.null(operations)) {
    operations <- "estimate"
  }

  valid_operations <- c("estimate", "fit", "fill")

  # 1. Default blocks to individual variables if not provided
  if (is.null(blocks)) {
    blocks <- setNames(as.list(names(data)), names(data))
  }

  # Convert blocks into actual variable names
  bv <- unique(unlist(blocks))

  # 2. Expand operations if it's a single value
  if (length(operations) == 1) {
    operations <- setNames(rep(operations, length(bv)), bv)
  }

  # 3. Check if all names in operations exist in blocks
  notFound <- !names(operations) %in% bv
  if (any(notFound)) {
    stop(paste0(
      "The following variables specified in `operations` are not present in `blocks`: ",
      paste(names(operations)[notFound], collapse = ", "), ".\n",
      "Ensure all specified variables match those in `blocks`."
    ))
  }

  # 4. Check if all operations are valid
  invalid_ops <- setdiff(unique(operations), valid_operations)
  if (length(invalid_ops) > 0) {
    stop(paste0(
      "Invalid operation(s) detected: ", paste(invalid_ops, collapse = ", "), ".\n",
      "Valid operations are: ", paste(valid_operations, collapse = ", "), ".\n",
      "Please correct the `operations` argument."
    ))
  }

  # 5. Prevent "fill" if models is NULL
  if ("fill" %in% operations && is.null(models)) {
    stop("The operation 'fill' requires a stored model, but `models` is NULL.\n",
         "Please provide a valid `models` object containing trained imputation models.")
  }

  # 6. Ensure that all "fill" variables have a trained model in models
  if ("fill" %in% operations && !is.null(models)) {
    fill_vars <- names(operations[operations == "fill"])
    missing_models <- setdiff(fill_vars, ls(models))
    if (length(missing_models) > 0) {
      stop(paste0(
        "The following variables specified as 'fill' do not have stored models: ",
        paste(missing_models, collapse = ", "), ".\n",
        "Ensure these variables were previously fitted before using 'fill'."
      ))
    }
  }

  # 7. Ensure all variables in models exist in blocks
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

  return(operations)
}
