#' Scan variable types in new data and compare to trained models
#'
#' This function scans the variables in `data` and compares their types and structure
#' against the trained models. If `coerce = TRUE`, variables in `data` will be
#' coerced to match the type information in `models`, and the scan is rerun on the
#' coerced data. The modified data is attached as an attribute `"data"` to the result.
#'
#' @param data A data frame with new data to be filled
#' @param models A list of trained model objects as produced by `mice()`
#' @param coerce Logical, if TRUE attempt to coerce variables in `data` to match `models`
#' @param print Logical, if TRUE prints the resulting table.
#' @return A data frame with one row per variable, and diagnostic columns. If `coerce = TRUE`,
#' the result has an attribute `"data"` with the coerced data.
#' @export
scan.types <- function(data, models, coerce = FALSE, print = FALSE) {
  orig.data <- data
  vars.data <- names(orig.data)
  vars.model <- names(models)
  vars.all <- union(vars.data, vars.model)

  report <- data.frame(
    variable = vars.all,
    in_data = vars.all %in% vars.data,
    in_model = vars.all %in% vars.model,
    data_class = NA_character_,
    model_class = NA_character_,
    class_match = NA_character_,
    levels_match = NA_character_,
    levels_new_missing = NA_character_,
    levels_extra = NA_character_,
    distribution_match = NA_character_,
    pred_match = NA_character_,
    task = NA_character_,
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
      report$levels_new_missing[i] <- if (any(!lvls.model %in% lvls.data)) "Y" else "N"
      report$levels_extra[i] <- if (any(!lvls.data %in% lvls.model)) "Y" else "N"
    }

    if (!is.null(x) && is.numeric(x) && !all(is.na(x))) {
      q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
      report$distribution_match[i] <- sprintf("Q1=%.2f, Q3=%.2f", q[1], q[2])
    }

    # predictor variable coverage
    if (!is.null(mod$formula)) {
      f <- as.formula(mod$formula)
      needed <- all.vars(f[[3L]])
      present <- names(orig.data)
      report$pred_match[i] <- if (all(needed %in% present)) "Y" else "N"
    }

    report$task[i] <- if (!is.null(mod$setup$task)) {
      mod$setup$task
    } else if (in_data) {
      "train"
    } else {
      "none"
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

  # If coerce = TRUE, try to coerce and rerun scan on coerced data
  if (isTRUE(coerce)) {
    coerced.data <- orig.data
    for (i in seq_len(nrow(report))) {
      j <- report$variable[i]
      if (report$in_data[i] && report$in_model[i] && report$class_match[i] == "N") {
        mod <- models[[j]][[1]]
        x <- coerced.data[[j]]
        target_class <- mod$class
        target_levels <- mod$factor$labels

        if (!is.null(target_class)) {
          if (target_class %in% c("factor", "ordered")) {
            coerced.data[[j]] <- factor(x, levels = target_levels, ordered = (target_class == "ordered"))
          } else if (target_class == "logical") {
            coerced.data[[j]] <- as.logical(x)
          } else if (target_class == "numeric") {
            coerced.data[[j]] <- as.numeric(x)
          }
        }
      }
    }
    result <- Recall(coerced.data, models, coerce = FALSE)
    attr(result, "data") <- coerced.data
    if (print) print(result)
    return(result)
  }

  if (print) print(report)
  report
}
