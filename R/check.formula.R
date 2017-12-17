check.formulas <- function(setup, data, ...) {
  blocks <- setup$blocks
  predictorMatrix <- setup$predictorMatrix
  formulas <- setup$formulas

  # initialize if user specified no formulas
  if (is.null(formulas)) {
    formulas <- as.list(rep("~ 0", length(blocks)))
  }
  
  # if no names were specified AND if the length matchs, assume that 
  # formula names are same as block names (convenience function)
  if (is.null(names(formulas)) && length(formulas) == length(blocks))
    names(formulas) <- names(blocks)
  
  # lecacy handling: character vector to list
  if (length(formulas) == length(blocks) && is.vector(formulas) 
      && is.character(formulas))
  {
    formulas[formulas != ""] <- "~ 0"
    fl <- as.list(formulas)
    names(fl) <- names(formulas)
    formulas <- fl
  }
  
  # add formula for any missing blocks
  # noFormula <- !names(blocks) %in% names(formulas)
  # fl <- c(formulas, as.list(rep("~ 0", sum(noFormula))))
  # names(fl) <- c(names(formulas), names(blocks)[noFormula])
  # formulas <- fl
  
  # check whether formula names are also block names
  # found <- names(formulas) %in% names(blocks)
  # if (any(!found)) stop("Missing block names: ", 
  #                       paste(names(formulas)[!found], collapse = ", "),
  #                       call. = FALSE)
  
  # convert to formula
  formulas <- lapply(formulas, as.formula)
  
  # determine blocks with no specified formula
  attr(formulas, "has.formula") <- !sapply(formulas, 
                                          is.empty.model.data, 
                                          data = data)
  
  # extend formulas with predictorMatrix
  for (h in seq_along(blocks)) {
    type <- predictorMatrix[h, ]
    predictors <- names(type)[type != 0]
    ff <- extend.formula(formula = formulas[[h]], predictors = predictors, ...)
    formulas[[h]] <- ff
  }
  
  # store
  setup$formulas.arg <- setup$formulas
  setup$formulas <- formulas
  setup
}

is.empty.model.data <- function (x, data) 
{
  tt <- terms(x, data = data)
  (length(attr(tt, "factors")) == 0L) & (attr(tt, "intercept") == 0L)
}
