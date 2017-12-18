check.blocks <- function(blocks, data) {

  # for proper workings, name all blocks  
  blocks <- name.blocks(blocks)
  
  # check that all names exists
  bv <- unique(unlist(blocks))
  notFound <- !bv %in% colnames(data)
  if (any(notFound)) 
    stop(paste("The following names were not found in `data`:",
               paste(bv[notFound], collapse = ", ")))
  blocks
}
