check.formula <- function(setup, data) {
  blocks <- setup$blocks
  formula <- setup$formula
  varnames <- setup$varnames
  
  # do nothing if we have no formula
  if (is.null(formula)) return(setup)
  
  # if no names were specified AND if the length matchs, assume that 
  # formula names are same as block names (convenience function)
  if (is.null(names(formula)) && length(formula) == length(blocks))
    names(formula) <- names(blocks)
  
  # check whether all formula names are also block names
  found <- names(formula) %in% names(blocks)
  if (any(!found)) stop("Unknown block names: ", names(formula)[!found])
  
  # convert to formula
  formula <- lapply(formula, as.formula)
  
  # check whether all term are variable names
  
  
  setup$formula <- formula
  setup
}
