#' Creates a \code{predictorMatrix} argument
#'
#' This helper function creates a valid \code{predictMatrix}. The
#' \code{predictorMatrix} is an argument to the \code{mice} function.
#' It specifies the target variable or block in the rows, and the
#' predictor variables on the columns. An entry of \code{0} means that
#' the column variable is NOT used to impute the row variable or block.
#' A nonzero value indicates that it is used.
#' @param data A \code{data.frame} with the source data
#' @param blocks An optional specification for blocks of variables in
#' the rows. The default assigns each variable in its own block.
#' @param predictorMatrix A predictor matrix from which rows with the same
#' names are copied into the output predictor matrix.
#' @return A matrix
#' @seealso \code{\link{make.blocks}}
#' @examples
#' make.predictorMatrix(nhanes)
#' make.predictorMatrix(nhanes, blocks = make.blocks(nhanes, "collect"))
#' @export
make.predictorMatrix <- function(data, blocks = make.blocks(data),
                                 predictorMatrix = NULL) {
  input.predictorMatrix <- predictorMatrix
  data <- check.dataform(data)
  predictorMatrix <- matrix(1, nrow = length(blocks), ncol = ncol(data))
  dimnames(predictorMatrix) <- list(names(blocks), colnames(data))
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
  predictorMatrix
}

check.predictorMatrix <- function(predictorMatrix,
                                  data,
                                  blocks = NULL) {
  data <- check.dataform(data)

  if (!is.matrix(predictorMatrix)) {
    stop("predictorMatrix not a matrix", call. = FALSE)
  }
  if (any(dim(predictorMatrix) == 0L)) {
    stop("predictorMatrix has no rows or columns", call. = FALSE)
  }

  # if we have no blocks, restrict to square predictorMatrix
  if (is.null(blocks)) {
    if (nrow(predictorMatrix) != ncol(predictorMatrix)) {
      stop(
        paste(
          "If no blocks are specified, predictorMatrix must",
          "have same number of rows and columns"
        ),
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
    for (i in row.names(predictorMatrix)) {
      predictorMatrix[i, grep(paste0("^", i, "$"), colnames(predictorMatrix))] <- 0
    }
    return(predictorMatrix)
  }

  # check conforming arguments
  if (nrow(predictorMatrix) > length(blocks)) {
    stop(
      paste0(
        "predictorMatrix has more rows (", nrow(predictorMatrix),
        ") than blocks (", length(blocks), ")"
      ),
      call. = FALSE
    )
  }

  # borrow rownames from blocks if needed
  if (is.null(rownames(predictorMatrix)) &&
      nrow(predictorMatrix) == length(blocks)) {
    rownames(predictorMatrix) <- names(blocks)
  }
  if (is.null(rownames(predictorMatrix))) {
    stop("Unable to set row names of predictorMatrix", call. = FALSE)
  }

  # borrow blocknames from predictorMatrix if needed
  if (is.null(names(blocks)) &&
      nrow(predictorMatrix) == length(blocks)) {
    names(blocks) <- rownames(predictorMatrix)
  }
  if (is.null(names(blocks))) {
    stop("Unable to set names of blocks", call. = FALSE)
  }

  # check existence of row names in blocks
  found <- rownames(predictorMatrix) %in% names(blocks)
  if (!all(found)) {
    stop("Names not found in blocks: ",
         paste(rownames(predictorMatrix)[!found], collapse = ", "),
         call. = FALSE
    )
  }

  # borrow colnames from data if needed
  if (is.null(colnames(predictorMatrix)) &&
      ncol(predictorMatrix) == ncol(data)) {
    colnames(predictorMatrix) <- names(data)
  }
  if (is.null(colnames(predictorMatrix))) {
    stop("Unable to set column names of predictorMatrix", call. = FALSE)
  }

  # check existence of variable names on data
  found <- colnames(predictorMatrix) %in% names(data)
  if (!all(found)) {
    stop("Names not found in data: ",
         paste(colnames(predictorMatrix)[!found], collapse = ", "),
         call. = FALSE
    )
  }

  list(
    predictorMatrix = predictorMatrix,
    blocks = blocks
  )
}

edit.predictorMatrix <- function(predictorMatrix,
                                 visitSequence,
                                 user.visitSequence,
                                 maxit) {
  # edit predictorMatrix to a monotone pattern
  if (maxit == 1L &&
      !is.null(user.visitSequence) &&
      length(user.visitSequence) == 1 &&
      user.visitSequence == "monotone") {
    for (i in 1L:length(visitSequence)) {
      predictorMatrix[visitSequence[i], visitSequence[i:length(visitSequence)]] <- 0
    }
  }
  predictorMatrix
}
