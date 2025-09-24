initialize.imp <- function(data, m, ignore, where, blocks, visitSequence,
                           method, nmis, data.init) {
  imp <- vector("list", ncol(data))
  names(imp) <- names(data)
  r <- !is.na(data)

  for (h in visitSequence) {
    for (j in blocks[[h]]) {
      y <- data[, j]
      ry <- r[, j] & !ignore
      wy <- where[, j]
      imp[[j]] <- as.data.frame(matrix(NA, nrow = sum(wy), ncol = m))
      dimnames(imp[[j]]) <- list(row.names(data)[wy], 1:m)

      if (method[h] != "") {
        for (i in seq_len(m)) {
          if (nmis[j] < nrow(data) && is.null(data.init)) {
            imp[[j]][, i] <- mice.impute.sample(y, ry, wy = wy)
          } else if (!is.null(data.init)) {
            imp[[j]][, i] <- data.init[wy, j]
          } else {
            if (is.factor(y)) {
              imp[[j]][, i] <- sample(levels(y), sum(wy), replace = TRUE)
            } else {
              imp[[j]][, i] <- rnorm(sum(wy))
            }
          }
        }
      }
    }
  }

  # Ensure imp[[j]] exists for any j used in where or blocks
  vars_needed <- union(colnames(where)[colSums(where) > 0], unique(unlist(blocks)))
  for (j in vars_needed) {
    if (is.null(imp[[j]])) {
      if (j %in% colnames(where)) {
        wy <- where[, j]
      } else {
        wy <- rep(FALSE, nrow(data))
      }
      imp[[j]] <- as.data.frame(matrix(NA, nrow = sum(wy), ncol = m))
      dimnames(imp[[j]]) <- list(row.names(data)[wy], as.character(seq_len(m)))
    }
  }

  imp
}
