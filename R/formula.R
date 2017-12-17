#' Extends a formula with predictors
#' 
#' @param formula A formula. If it is 
#' not a formula, the formula is internally reset to \code{~0}.
#' @param predictors A character vector of variable names.
#' @param include.auxiliary A logical that indicates whether the variables
#' listed in \code{predictors} should be added to the formula as main 
#' effects. The default is \code{TRUE}.
#' @param include.intercept A logical that indicated whether the intercept 
#' should be included in the result. The default (\code{FALSE}) may change 
#' in future versions.
#' @return A formula
#' @keywords internal
extend.formula <- function(formula = ~ 0,
                           predictors = NULL,
                           include.auxiliary = TRUE,
                           include.intercept = FALSE, ...) {
  if (!is.formula(formula)) formula <- ~ 0
  
  # handle dot in RHS
  if (hasdot(formula)) {
    if (length(predictors) > 1)
      fr <- as.formula(c("~", paste(predictors, collapse = "+")))
    else 
      fr <- ~ 0
  } else 
    fr <- reformulate(c(".", predictors))
  
  if (include.auxiliary) formula <- update(formula, fr, ...)
  if (include.intercept) formula <- update(formula, ~ . + 1, ...)
  formula
}

is.formula <- function(x){
  inherits(x, "formula")
}

hasdot <- function(f) {
  if(is.recursive(f)) {
    return(any(sapply(as.list(f), hasdot)))
  } else {
    f == as.symbol(".")}
}
