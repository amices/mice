# contributed by Simon Grund, #137
check.deprecated <- function(...) {
  # print warnings for deprecated argument names
  nms <- names(list(...))
  replace.args <- list(
    imputationMethod = "method",
    defaultImputationMethod = "defaultMethod",
    form = "formulas"
  )

  wrn <- names(replace.args) %in% nms
  if (any(wrn)) {
    for (i in which(wrn)) {
      msg <- paste0(
        "The '",
        names(replace.args)[i],
        "' argument is no longer supported. Please use '",
        replace.args[i],
        "' instead."
      )
      warning(msg)
    }
  }

  invisible(NULL)
}

# warns for arguments of the development versions 3.19.13 and 3.19.14.
# These now pass down to the imputation methods, as they did before
# 3.19.13, so wrappers such as parlmice() that pass their own n.core keep
# working. Only a clear use of the old interface gives a warning.
check.removed <- function(future, ...) {
  dots <- list(...)
  if (isTRUE(dots[["parallel"]])) {
    warning(
      "The 'parallel' argument is no longer supported and is ignored. ",
      "Use 'future = TRUE' instead, and set the backend by future::plan().",
      call. = FALSE
    )
  }
  if (isTRUE(future) && !is.null(dots[["n.core"]])) {
    warning(
      "The 'n.core' argument is no longer supported and is ignored. ",
      "Set the number of workers by future::plan(), ",
      "e.g. future::plan(future::multisession, workers = 4).",
      call. = FALSE
    )
  }
  invisible(NULL)
}
