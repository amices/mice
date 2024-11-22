collect.ynames <- function(predictorMatrix, blocks, formulas) {
  # reads and combines the ynames attributes
  ynames1 <- attr(predictorMatrix, "ynames")
  ynames2 <- attr(blocks, "ynames")
  ynames3 <- attr(formulas, "ynames")
  ynames <- unique(c(ynames1, ynames2, ynames3))
  return(ynames)
}
