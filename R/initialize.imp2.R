initialize.imp2 <- function(data, m, ignore, where, blocks, visitSequence, method, nmis, data.init) {
  # Ensure `data` is a data.table
  if (!is.data.table(data)) stop("Input `data` must be a data.table.")

  # Validate `where`
  if (ncol(where) != ncol(data)) stop("`where` must match the dimensions of `data`.")

  # Initialize list of imputations
  imp <- vector("list", ncol(data))
  names(imp) <- names(data)

  r <- !is.na(data)  # Identify observed values

  # Iterate over blocks in the visit sequence
  for (h in visitSequence) {
    for (j in blocks[[h]]) {
      y <- data[[j]]  # Extract column `j`
      ry <- r[, j] & !ignore  # Observed values (not ignored)
      wy <- where[, j]  # Where imputation is needed
      row_ids <- which(wy)  # Row indices needing imputation
      n <- length(row_ids)  # Number of rows needing imputation

      # prepare imputed_table
      imputed_table <- data.table(matrix(NA, nrow = n, ncol = m + 1))
      setnames(imputed_table, c(as.character(seq_len(m)), "row_id"))
      imputed_table[, "row_id" := row_ids]

      # for backward compatibility, store row_ids as rownames
      # FIXME remove this in the future
      rownames(imputed_table) <- as.character(row_ids)

      for (i in seq_len(m)) {
        if (nmis[j] < nrow(data) && is.null(data.init)) {
          imp_values <- mice.impute.sample(y, ry, wy = wy)
        } else if (!is.null(data.init)) {
          imp_values <- data.init[wy, j]
        } else {
          if (is.factor(y)) {
            imp_values <- sample(levels(y), n, replace = TRUE)
          } else if (is.numeric(y)) {
            imp_values <- rnorm(n, mean(y[ry], na.rm = TRUE), sd(y[ry], na.rm = TRUE))
          } else if (is.character(y)) {
            imp_values <- sample(y[ry], n, replace = TRUE)
          } else if (inherits(y, "Date")) {
            imp_values <- sample(y[ry], n, replace = TRUE)
          } else if (is.logical(y)) {
            imp_values <- sample(c(TRUE, FALSE), n, replace = TRUE)
          } else {
            stop("Unsupported column type.")
          }
        }

        imputed_table[[as.character(i)]] <- imp_values
      }

      imp[[j]] <- imputed_table
    }
  }

  return(imp)
}
