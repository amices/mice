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
