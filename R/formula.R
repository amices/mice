#' Creates a formula that combines form and predictors
#' 
#' @param form A formula, typically specified by the user. If it is 
#' not a formula, the form is internally reset to \code{~0}.
#' @param predictors A character vector of variable names.
#' @param include.predictors A logical that indicates whether the variables
#' listed in \code{predictors} should be added to the result. The default 
#' is \code{TRUE}
#' @param include.intercept A logical that indicated whether the intercept 
#' should be included in the result. The default (\code{FALSE}) may change 
#' in future versions.
#' @return A formula
#' @keywords internal
create.formula <- function(form = ~ 0,
                           predictors = NULL,
                           include.predictors = TRUE,
                           include.intercept = FALSE, ...) {
  if (!is.formula(form)) form = ~ 0
  f <- reformulate(c(".", predictors))
  if (include.predictors) form <- update(form, f, ...)
  if (include.intercept) form <- update(form, ~ . + 1, ...)
  form
}

is.formula <- function(x){
  inherits(x,"formula")
}
