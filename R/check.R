# internal function for checking input to main mice() function

check.data <- function(data) {
  if (!(is.matrix(data) || is.data.frame(data)))
    stop("Data should be a matrix or data frame", call. = FALSE)
  if (ncol(data) < 2)
    stop("Data should contain at least two columns", call. = FALSE)
  as.data.frame(data)
}

check.post <- function(setup) {
  blocks <- setup$blocks
  post <- setup$post
  
  # check
  #if (length(post) != length(blocks))
  #  stop("`length(post)` does not match `length(blocks)`.")
  
  # change
  #if (is.null(names(post))) names(post) <- names(blocks)
  
  setup$post <- post
  setup
}

