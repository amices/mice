check.formula <- function(setup, data, ...) {
  blocks <- setup$blocks
  predictorMatrix <- setup$predictorMatrix
  formula <- setup$formula

  # initialize if user specified no formula
  if (is.null(formula)) {
    formula <- as.list(rep("~ 0", length(blocks)))
  }
  
  # if no names were specified AND if the length matchs, assume that 
  # formula names are same as block names (convenience function)
  if (is.null(names(formula)) && length(formula) == length(blocks))
    names(formula) <- names(blocks)
  
  # lecacy handling: character vector to list
  if (length(formula) == length(blocks) && is.vector(formula) 
      && is.character(formula))
  {
    formula[formula != ""] <- "~ 0"
    fl <- as.list(formula)
    names(fl) <- names(formula)
    formula <- fl
  }
  
  # add formula for any missing blocks
  noFormula <- !names(blocks) %in% names(formula)
  fl <- c(formula, as.list(rep("~ 0", sum(noFormula))))
  names(fl) <- c(names(formula), names(blocks)[noFormula])
  formula <- fl
  
  # check whether formula names are also block names
  found <- names(formula) %in% names(blocks)
  if (any(!found)) stop("Unknown block names: ", names(formula)[!found])
  
  # convert to formula
  formula <- lapply(formula, as.formula)
  
  # determine blocks with no specified formula
  attr(formula, "has.formula") <- !sapply(formula, is.empty.model)
  
  # extend formula with predictorMatrix
  for (h in seq_along(blocks)) {
    type <- predictorMatrix[h, ]
    predictors <- names(type)[type != 0]
    ff <- extend.formula(formula = formula[[h]], predictors = predictors, ...)
    formula[[h]] <- ff
  }
  
  # store
  setup$formula.arg <- setup$formula
  setup$formula <- formula
  setup
}
