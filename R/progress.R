# Returns a function(k, i) that signals one progressr step for iteration
# k of imputation i, or NULL if progressr is not installed. The progressor
# belongs to the frame of the caller, so it finishes when that function
# returns. Progress is only shown if the user enabled a progressr handler.
make.progress <- function(steps, envir = parent.frame()) {
  if (steps < 1L || !requireNamespace("progressr", quietly = TRUE)) {
    return(NULL)
  }
  p <- progressr::progressor(steps = steps, envir = envir)
  function(k, i) {
    p(message = paste0("iter ", k, ", imp ", i))
  }
}
