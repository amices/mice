handles.arg <- function(f, a = "data") {
  # determine whether function f handles argument a
  if (!is.function(f)) return(FALSE)
  a %in% names(formals(f))
}

handles.format <- function(fn) {
  # determine whether function fn handles the `format` argument
  f <- get(fn)
  handles.arg(f, "format")
}
