#' Scan data types and compare to model expectations
#'
#' This function compares the structure and type of `data` to what is expected
#' from the trained `models`. It reports differences in class, levels, and predictor
#' availability, useful to prepare for imputation or prediction.
#'
#' @param data A `data.frame` to be checked.
#' @param models A list of trained models from `mice()`.
#' @param print Logical, whether to print the resulting table.
#'
#' @return A `data.frame` with one row per variable (from either `data` or `models`)
#' and columns summarizing compatibility diagnostics. The following columns are included:
#'
#' \tabular{ll}{
#' `variable`            \tab Variable name \cr
#' `in_data`             \tab Logical: whether variable is present in `data` \cr
#' `in_model`            \tab Logical: whether a trained model is available \cr
#' `data_class`          \tab Class of the variable in `data` (e.g., `"factor"`, `"ordered"`) \cr
#' `model_class`        \tab model class according to the trained model \cr
#' `class_match`         \tab `"Y"` if `data_class` matches `model_class`, `"N"` otherwise \cr
#' `levels_match`        \tab `"Y"` if factor levels exactly match, `"N"` if they differ, "" if not applicable \cr
#' `pred_match`          \tab `"Y"` if all predictors used by the model are present in `data`, `"N"` if any are missing, "" if unknown \cr
#' `can_fill`            \tab `"Y"` if there is a model, if classes match, if levels match and if predictor match, other "" \cr
#' }
#'
#' @examples
#' # Train model on boys data
#' imp <- mice(boys, tasks = "train", m = 1, maxit = 1, print = FALSE)
#'
#' # Create a new dataset with missing values and mismatched types
#' data <- boys[1:3, ]
#' data$phb <- factor(data$phb, levels = levels(data$phb), ordered = FALSE)  # remove ordering
#'
#' # Run scan
#' scan.data(data, imp$models)
#'
#' @export
scan.data <- function(data, models, print = FALSE) {
  orig.data <- data
  vars.data <- names(orig.data)
  vars.model <- names(models)
  vars.all <- union(vars.data, vars.model)

  report <- data.frame(
    variable = vars.all,
    in_data = vars.all %in% vars.data,
    in_model = vars.all %in% vars.model,
    data_class = "",
    model_class = "",
    class_match = "",
    levels_match = "",
    pred_match = "",
    can_fill = "",
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(report))) {
    j <- report$variable[i]
    in_data <- isTRUE(report$in_data[i])
    in_model <- isTRUE(report$in_model[i])

    x <- if (isTRUE(in_data) && j %in% names(orig.data)) orig.data[[j]] else NULL
    mod <- if (in_model) models[[j]][[1]] else NULL

    report$data_class[i] <- if (!is.null(x)) {
      if (inherits(x, "ordered")) "ordered" else class(x)[1]
    } else NA

    if (!is.null(mod$class)) {
      report$model_class[i] <- mod$class
    }

    if (!is.null(x) && !is.null(mod$class)) {
      match <- if (inherits(x, "ordered")) "ordered" else class(x)[1]
      report$class_match[i] <- if (identical(match, mod$class)) "Y" else "N"
    }

    if (!is.null(x) && is.factor(x) && !is.null(mod$factor$labels)) {
      lvls.data <- levels(x)
      lvls.model <- mod$factor$labels
      report$levels_match[i] <- if (identical(lvls.data, lvls.model)) "Y" else "N"
      # report$levels_new_missing[i] <- if (any(!lvls.model %in% lvls.data)) "Y" else "N"
      # report$levels_extra[i] <- if (any(!lvls.data %in% lvls.model)) "Y" else "N"
    }

    # predictor variable coverage
    if (!is.null(mod$formula)) {
      f <- as.formula(mod$formula)
      needed <- all.vars(f[[3L]])
      present <- names(orig.data)
      report$pred_match[i] <- if (all(needed %in% present)) "Y" else "N"
    }
  }

  # Add can_fill column
  report$can_fill <- vapply(seq_len(nrow(report)), function(i) {
    if (isTRUE(report$in_model[i]) &&
        identical(report$class_match[i], "Y") &&
        !identical(report$levels_match[i], "N") &&
        identical(report$pred_match[i], "Y")) {
      "Y"
    } else {
      "N"
    }
  }, character(1))

  if (print) print(report)
  report
}

#' Construct a new data.frame from a trained model
#'
#' This function generates a data.frame with the correct structure (names, classes,
#' levels) based on a set of trained models, as produced by `mice()` with `tasks = "train"`.
#'
#' @param models A list of trained models from `mice()`.
#' @param n Number of rows to generate. Default is 1.
#' @param fill Value to fill the data with. Default is `NA`.
#' @param vars Optional character vector specifying the variable order. Default is `NULL`.
#'
#' @return A `data.frame` of `n` rows, where each column matches the type and levels
#'         expected from the corresponding model.
#' @examples
#' trained <- mice(boys, tasks = "train", print = FALSE)
#' newdata <- make.data(trained$models, n = 5, vars = names(boys))
#' str(newdata)
#' @export
make.data <- function(models, n = 1L, fill = NA, vars = NULL) {
  stopifnot(is.list(models), is.numeric(n), length(n) == 1L)

  all_vars <- names(models)
  vars <- if (is.null(vars)) all_vars else intersect(vars, all_vars)

  out <- vector("list", length(vars))
  names(out) <- vars

  for (j in vars) {
    mod <- models[[j]][[1]]
    cls <- mod$class
    lvls <- mod$factor$labels

    if (cls == "factor") {
      out[[j]] <- factor(rep(fill, n), levels = lvls)
    } else if (cls == "ordered") {
      out[[j]] <- factor(rep(fill, n), levels = lvls, ordered = TRUE)
    } else if (cls == "logical") {
      out[[j]] <- as.logical(rep(fill, n))
    } else if (cls == "numeric") {
      out[[j]] <- as.numeric(rep(fill, n))
    } else {
      warning(sprintf("Unknown class '%s' for variable '%s'", cls, j))
      out[[j]] <- rep(fill, n)
    }
  }

  as.data.frame(out)
}

#' Coerce a data.frame to match model expectations
#'
#' This function coerces columns in `data` to match the type and levels
#' expected by the corresponding trained `models`.
#'
#' @param data A `data.frame` with input data.
#' @param models A list of trained models from `mice()`.
#'
#' @return A coerced `data.frame` that matches the class and levels from `models`.
#' @export
coerce.data <- function(data, models) {
  data <- as.data.frame(data)
  for (j in intersect(names(data), names(models))) {
    mod <- models[[j]][[1]]
    cls <- mod$class
    lvls <- mod$factor$labels

    if (cls == "factor") {
      data[[j]] <- factor(data[[j]], levels = lvls)
    } else if (cls == "ordered") {
      data[[j]] <- factor(data[[j]], levels = lvls, ordered = TRUE)
    } else if (cls == "logical") {
      data[[j]] <- as.logical(data[[j]])
    } else if (cls == "numeric") {
      data[[j]] <- as.numeric(data[[j]])
    }
  }
  data
}
