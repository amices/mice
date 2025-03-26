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

      # Determine correct NA type
      na_type <- switch(class(y)[1],
                        "logical" = as.logical(NA),
                        "factor"  = as.character(NA),
                        "ordered" = as.character(NA),
                        NA_real_
      )

      # Initialize imp[[j]] with correct type
      imp[[j]] <- as.data.frame(
        matrix(na_type, nrow = sum(wy), ncol = m)
      )
      dimnames(imp[[j]]) <- list(row.names(data)[wy], as.character(seq_len(m)))

      if (method[h] != "") {
        for (i in seq_len(m)) {
          if (nmis[j] < nrow(data) && is.null(data.init)) {
            vec <- mice.impute.sample(y, ry, wy = wy)
          } else if (!is.null(data.init)) {
            vec <- data.init[wy, j]
          } else {
            # Type-safe fallback
            n <- sum(wy)
            if (is.factor(y)) {
              vec <- sample(levels(y), n, replace = TRUE)
              vec <- factor(vec, levels = levels(y), ordered = is.ordered(y))
            } else if (is.logical(y)) {
              vec <- sample(c(TRUE, FALSE), n, replace = TRUE)
            } else {
              vec <- rnorm(n)
            }
          }

          # Final safety check: enforce type match with y
          if (is.logical(y)) vec <- as.logical(vec)
          if (is.factor(y)) vec <- factor(vec, levels = levels(y), ordered = is.ordered(y))

          imp[[j]][, i] <- vec
        }
      }
    }
  }

  return(imp)
}
