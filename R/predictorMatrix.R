#' Creates a \code{predictorMatrix}
#'
#' This helper function creates a valid \code{predictMatrix}. The 
#' \code{predictorMatrix} is an argument to the \code{mice} function 
#' that specifies the target variable or block in the rows, and the 
#' predictor variables on the columns. An entry of \code{0} means that 
#' the column variable is NOT used to impute the row variable or block.
#' A nonzero value indicates that it is used.
#' @param data A \code{data.frame} with the source data
#' @param blocks An optional specification for blocks of variables in 
#' the rows. The default assigns each variable in its own block.
#' @return A matrix
#' @seealso \code{\link{make.blocks}}
#' @examples
#' make.predictorMatrix(nhanes)
#' make.predictorMatrix(nhanes, blocks = make.blocks(nhanes, "collect"))
#' @export
make.predictorMatrix <- function(data, blocks = make.blocks(data)) {
  predictorMatrix <- matrix(1, nrow = length(blocks), ncol = ncol(data))
  dimnames(predictorMatrix) <- list(names(blocks), colnames(data))
  for (i in row.names(predictorMatrix)) 
    predictorMatrix[i, grep(i, colnames(predictorMatrix))] <- 0
  predictorMatrix
}

check.predictorMatrix <- function(setup) {
  ## checks and makes consistency edits of the predictormatrix
  blocks <- setup$blocks
  nimp <- setup$nimp
  pred <- setup$predictorMatrix
  varnames <- setup$varnames
  nwhere <- setup$nwhere
  nvar <- setup$nvar
  vis <- setup$visitSequence
  
  nblo <- length(blocks)
  blocknames <- names(blocks)
  
  if (!is.matrix(pred))
    stop("Argument 'predictorMatrix' not a matrix.")
  if (nblo != nrow(pred))
    stop(paste0("The predictorMatrix has ", nrow(pred), 
                " rows. This should be ", nblo, "."))
  
  # inactivate predictors of complete (or not imputed) block
  for (j in seq_along(blocks)) {
    if (nimp[j] == 0) pred[j, ] <- 0
  }
  
  # variable cannot be its own predictor
  for (i in names(blocks))
    if (!is.null(i)) pred[i, grep(i, colnames(pred))] <- 0
  
  setup$predictorMatrix <- pred
  setup
}
