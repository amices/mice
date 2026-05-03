check.data <- function(data, method) {
  check.dataform(data)
}


check.dataform <- function(data) {
  if (!(is.matrix(data) || is.data.frame(data))) {
    stop("Data should be a matrix or data frame", call. = FALSE)
  }
  if (ncol(data) < 2) {
    stop("Data should contain at least two columns", call. = FALSE)
  }
  data <- as.data.frame(data)
  mat <- sapply(data, is.matrix)
  df <- sapply(data, is.data.frame)
  if (any(mat)) {
    stop(
      "Cannot handle columns with class matrix: ",
      colnames(data)[mat]
    )
  }
  if (any(df)) {
    stop(
      "Cannot handle columns with class data.frame: ",
      colnames(data)[df]
    )
  }

  dup <- duplicated(colnames(data))
  if (any(dup)) {
    stop(
      "Duplicate names found: ",
      paste(colnames(data)[dup], collapse = ", ")
    )
  }

  posix <- sapply(data, function(x) inherits(x, c("POSIXct", "POSIXlt")))
  if (any(posix)) {
    warning(
      "Data contain POSIX date-time columns: ",
      paste(colnames(data)[posix], collapse = ", "),
      ".\nPOSIX variables are stored as seconds since 1970 (large numbers ~1e9-1e10) ",
      "and can cause near-singular matrices in norm-based imputation methods. ",
      "Consider converting to numeric, Date, or a standardised numeric variable before imputing.",
      call. = FALSE
    )
  }

  data
}


check.m <- function(m) {
  m <- m[1L]
  if (!is.numeric(m)) {
    stop("Argument m not numeric", call. = FALSE)
  }
  m <- floor(m)
  if (m < 1L) {
    stop("Number of imputations (m) lower than 1.", call. = FALSE)
  }
  m
}


check.cluster <- function(data, predictorMatrix) {
  # stop if the cluster variable is a factor
  isclassvar <- apply(predictorMatrix == -2, 2, any)
  for (j in colnames(predictorMatrix)) {
    if (isclassvar[j] && lapply(data, is.factor)[[j]]) {
      stop("Convert cluster variable ", j, " to integer by as.integer()")
    }
  }
  TRUE
}

check.ignore <- function(ignore, data) {
  if (is.null(ignore)) {
    return(rep(FALSE, nrow(data)))
  }
  if (!is.logical(ignore)) {
    stop("Argument ignore not a logical.")
  }
  if (length(ignore) != nrow(data)) {
    stop(
      "length(ignore) (",
      length(ignore),
      ") does not match nrow(data) (",
      nrow(data),
      ")."
    )
  }
  if (sum(!ignore) < 10L) {
    warning(
      "Fewer than 10 rows for fitting the imputation model. Are you sure?",
      call. = FALSE
    )
  }
  ignore
}

check.newdata <- function(newdata, data) {
  if (is.null(newdata)) {
    stop("No newdata found.")
  }
  if (!is.data.frame(newdata)) {
    stop("newdata not a data.frame.")
  }
  newdata
}
