handles.arg <- function(x, a = "data") {
  # determine whether function x handles argument a
  if (!is.function(x)) return(FALSE)
  !is.null(formals(x)[[a]])
}

handles.format <- function(x) {
  # determine whether function x handles the `format` argument
  handles.arg(x, "format")
}
