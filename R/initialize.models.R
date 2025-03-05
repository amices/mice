#
## Example usage
# visitSequence <- c("block1", "block2", "block3")  # Example block names
# m <- 5  # Number of imputations
# models <- initialize_models(visitSequence, m)
initialize.models <- function(visitSequence, m) {
  models <- new.env(parent = emptyenv())

  # Pre-allocate NULL entries for every (blockname, iteration) pair
  for (block in visitSequence) {
    for (iter in 1:m) {
      assign(paste0(block, "_", iter), NULL, envir = models)
    }
  }
  return(models)
}
