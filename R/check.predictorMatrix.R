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
