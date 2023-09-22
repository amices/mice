#' Creates a `predictorMatrix` argument
#'
#' This helper function creates a valid `predictMatrix`. The
#' `predictorMatrix` is an argument to the `mice` function.
#' It specifies the target variable or block in the rows, and the
#' predictor variables on the columns. An entry of `0` means that
#' the column variable is NOT used to impute the row variable or block.
#' A nonzero value indicates that it is used.
#' @param data A `data.frame` with the source data
#' @param blocks An optional specification for blocks of variables in
#' the rows. The default assigns each variable in its own block.
#' @param predictorMatrix A predictor matrix from which rows with the same
#' names are copied into the output predictor matrix.
#' @return A matrix
#' @seealso [make.blocks()]
#' @examples
#' make.predictorMatrix(nhanes)
#' make.predictorMatrix(nhanes, blocks = make.blocks(nhanes, "collect"))
#' @export
make.predictorMatrix <- function(data, blocks = make.blocks(data),
                                 predictorMatrix = NULL) {
  input.predictorMatrix <- predictorMatrix
  data <- check.dataform(data)
  predictorMatrix <- matrix(1, nrow = ncol(data), ncol = ncol(data))
  dimnames(predictorMatrix) <- list(colnames(data), colnames(data))
  for (i in row.names(predictorMatrix)) {
    predictorMatrix[i, colnames(predictorMatrix) %in% i] <- 0
  }
  # preserve any user setting in predictorMatrix specification
  if (!is.null(input.predictorMatrix)) {
    for (i in row.names(predictorMatrix)) {
      if (i %in% row.names(input.predictorMatrix)) {
        predictorMatrix[i, ] <- input.predictorMatrix[i, ]
      }
    }
  }
  # but insist on zero diagonal
  diag(predictorMatrix) <- 0
  valid <- validate.predictorMatrix(predictorMatrix)
  if (!valid) {
    warning("Malformed predictorMatrix. See ?make.predictorMatrix")
  }
  predictorMatrix
}

check.predictorMatrix <- function(predictorMatrix,
                                  data,
                                  blocks = NULL,
                                  autoremove = TRUE) {
  data <- check.dataform(data)

  if (!is.matrix(predictorMatrix)) {
    stop("predictorMatrix not a matrix", call. = FALSE)
  }
  if (any(dim(predictorMatrix) == 0L)) {
    stop("predictorMatrix has no rows or columns", call. = FALSE)
  }

  # restrict to square predictorMatrix
  if (nrow(predictorMatrix) != ncol(predictorMatrix)) {
    stop("predictorMatrix must have same number of rows and columns",
         call. = FALSE
    )
  }

  if (is.null(dimnames(predictorMatrix))) {
    if (ncol(predictorMatrix) == ncol(data)) {
      dimnames(predictorMatrix) <- list(colnames(data), colnames(data))
    } else {
      stop("Missing row/column names in predictorMatrix", call. = FALSE)
    }
  }

  # set diagonal to zero
  diag(predictorMatrix) <- 0

  # check existence of variable names in data
  found <- colnames(predictorMatrix) %in% colnames(data)
  if (!all(found)) {
    stop("Names not found in data: ",
         paste(colnames(predictorMatrix)[!found], collapse = ", "),
         call. = FALSE
    )
  }

  # NA-propagation prevention
  # find all dependent (imputed) variables
  hit <- apply(predictorMatrix, 1, function(x) any(x != 0))
  ynames <- row.names(predictorMatrix)[hit]
  # find all variables in data that are not imputed
  notimputed <- setdiff(colnames(data), ynames)
  # select uip: unimputed incomplete predictors
  completevars <- colnames(data)[!apply(is.na(data), 2, sum)]
  uip <- setdiff(notimputed, completevars)
  # if any of these are predictors, remove them
  removeme <- intersect(uip, colnames(predictorMatrix))
  if (length(removeme) && autoremove) {
    predictorMatrix[, removeme] <- 0
    for (j in removeme) {
    updateLog(out = paste("removed incomplete predictor", j),
              meth = "check", frame = 1)
    }
  }

  # grow predictorMatrix to all variables in data
  if (ncol(predictorMatrix) < ncol(data)) {
    p <- matrix(0, nrow = ncol(data), ncol = ncol(data),
                dimnames = list(colnames(data), colnames(data)))
    p[row.names(predictorMatrix), colnames(predictorMatrix)] <- predictorMatrix
    predictorMatrix <- p
  }

  # save calculated ynames
  attr(predictorMatrix, "ynames") <- ynames

  # needed for cases E and H
  if (!is.null(blocks)) {
    if (nrow(predictorMatrix) < length(blocks)) {
      stop(
        paste0(
          "predictorMatrix has fewer rows (", nrow(predictorMatrix),
          ") than blocks (", length(blocks), ")"
        ),
        call. = FALSE
      )
    }
  }

  valid <- validate.predictorMatrix(predictorMatrix)

  if (!valid) {
    warning("Malformed predictorMatrix. See ?make.predictorMatrix")
  }
  return(predictorMatrix)
}

edit.predictorMatrix <- function(predictorMatrix,
                                 method,
                                 blocks,
                                 where,
                                 visitSequence,
                                 user.visitSequence,
                                 maxit) {
  # for empty method, set predictorMatrix row to zero
  for (b in names(method)) {
    ynames <- blocks[[b]]
    for (j in ynames) {
      if (method[b] == "") {
        predictorMatrix[j, ] <- 0
      }
    }
  }

  # for variables that will not be imputed, set predictorMatrix row to zero
  nimp <- nimp(where = where, blocks = blocks)
  for (j in seq_along(blocks)) {
    if (!nimp[j]) {
      predictorMatrix[blocks[[j]], ] <- 0
    }
  }

  # edit predictorMatrix to a monotone pattern
  if (maxit == 1L && !is.null(user.visitSequence) && user.visitSequence == "monotone") {
    for (i in 1L:length(visitSequence)) {
      predictorMatrix[visitSequence[i], visitSequence[i:length(visitSequence)]] <- 0
    }
  }

  valid <- validate.predictorMatrix(predictorMatrix)
  if (!valid) {
    warning("Malformed predictorMatrix. See ?make.predictorMatrix")
  }
  predictorMatrix
}

validate.predictorMatrix <- function(predictorMatrix, silent = FALSE) {

  if (!is.matrix(predictorMatrix)) {
    if (!silent) warning("predictorMatrix not a matrix", call. = FALSE)
    return(FALSE)
  }
  if (any(dim(predictorMatrix) == 0L)) {
    if (!silent) warning("predictorMatrix has no rows or columns", call. = FALSE)
    return(FALSE)
  }
  if (nrow(predictorMatrix) != ncol(predictorMatrix)) {
    if (!silent) warning("predictorMatrix is not square")
    return(FALSE)
  }
  if (is.null(dimnames(predictorMatrix))) {
    if (!silent) warning("predictorMatrix has no row/column names")
    return(FALSE)
  }
  if (any(diag(predictorMatrix) != 0)) {
    if (!silent) warning("predictorMatrix has no zero diagonal")
  }

  return(TRUE)
}
