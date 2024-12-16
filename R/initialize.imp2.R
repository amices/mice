initialize.imp2 <- function(data, m, ignore, where, blocks, visitSequence, method, nmis, data.init) {
  # Validate inputs
  if (any(!visitSequence %in% seq_along(blocks))) stop("Invalid `visitSequence`.")
  if (ncol(where) != ncol(data)) stop("`where` must match the dimensions of `data`.")

  # Initialize list of imputations
  imp <- vector("list", ncol(data))
  names(imp) <- names(data)
  r <- !is.na(data)  # Identify observed values

  # Iterate over blocks in the visit sequence
  for (h in visitSequence) {
    for (j in blocks[[h]]) {
      y <- data[, j]
      ry <- r[, j] & !ignore
      wy <- where[, j]
      n <- sum(wy)

      # Create an empty imputation matrix
      imp[[j]] <- data.table(matrix(NA, nrow = n, ncol = m))
      setnames(imp[[j]], paste0("imp_", seq_len(m)))
      rownames(imp[[j]]) <- row.names(data)[wy]

      if (method[h] != "") {
        imp[[j]][] <- impute_column(y, ry, wy, m, data.init, is.factor(y))
      }
    }
  }
  imp
}

impute_column <- function(y, ry, wy, m, data.init = NULL, is_factor = is.factor(y)) {
  n <- sum(wy)
  imputed <- matrix(NA, nrow = n, ncol = m)

  if (!is.null(data.init)) {
    imputed[,] <- data.init[wy]
  } else if (is_factor) {
    for (i in seq_len(m)) {
      imputed[, i] <- sample(y[ry], n, replace = TRUE)
    }
  } else {
    for (i in seq_len(m)) {
      imputed[, i] <- rnorm(n, mean(y[ry], na.rm = TRUE), sd(y[ry], na.rm = TRUE))
    }
  }
  imputed
}