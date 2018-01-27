check.data <- function(data) {
  if (!(is.matrix(data) || is.data.frame(data)))
    stop("Data should be a matrix or data frame", call. = FALSE)
  if (ncol(data) < 2)
    stop("Data should contain at least two columns", call. = FALSE)
  as.data.frame(data)
}

check.cluster <- function(data, predictorMatrix) {
  # stop if the cluster variable is a factor
  isclassvar <- apply(predictorMatrix == -2, 2, any)
  for (j in colnames(predictorMatrix)) {
    if (isclassvar[j] && lapply(data, is.factor)[[j]]) 
      stop("Convert cluster variable ", j, " to integer by as.integer()")
  }
  TRUE
}
