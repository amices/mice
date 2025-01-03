initialize.imp <- function(data, m, ignore, where, blocks,
                           visitSequence, method, nmis, data.init = NULL,
                           leave.empty = FALSE) {
  col.type.NA <- function(x) {
    if (is.factor(x)) {
      NA_character_
    } else if (is.numeric(x)) {
      NA_real_
    } else if (is.character(x)) {
      NA_character_
    } else if (is.logical(x)) {
      NA
    } else if (is.integer(x)) {
      NA_integer_
    } else if (inherits(x, "Date")) {
      as.POSIXct(NA)
    } else {
      NA
    }
  }

  # Create imp with initialization based on column types
  imp <- lapply(seq_len(ncol(data)), function(i) {
    col_type <- class(data[[i]])
    init_val <- col.type.NA(data[[i]])
    tbl <- data.table(matrix(init_val, nrow = 0L, ncol = m + 1L))
    setnames(tbl, c(as.character(seq_len(m)), "row_id"))
    tbl
  })
  names(imp) <- names(data)
  r <- !is.na(data)

  for (h in visitSequence) {
    for (j in blocks[[h]]) {
      y <- extract.column(data, j)
      ry <- extract.column(r, j) & !ignore
      wy <- extract.column(where, j)

      # create the structure of imp element j
      init_val <- col.type.NA(y)
      row_ids <- which(wy)
      n <- length(row_ids)
      tbl <- data.table(matrix(init_val, nrow = n, ncol = m + 1L))
      setnames(tbl, c(as.character(seq_len(m)), "row_id"))
      tbl[, "row_id" := row_ids]
      setkeyv(tbl, cols = "row_id")

      # fill element with initial imputations
      # this advances the random number generator seed
      if (!leave.empty && method[h] != "") {
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
          tbl[[as.character(i)]] <- imp_values
        }
      }
      imp[[j]] <- tbl
    }
  }
  return(imp)
}
