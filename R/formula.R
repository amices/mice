#' Extends a formula with predictors
#' 
#' @param formula A formula. If it is 
#' not a formula, the formula is internally reset to \code{~0}.
#' @param predictors A character vector of variable names.
#' @param auxiliary A logical that indicates whether the variables
#' listed in \code{predictors} should be added to the formula as main 
#' effects. The default is \code{TRUE}.
#' @param include.intercept A logical that indicated whether the intercept 
#' should be included in the result.
#' @return A formula
#' @keywords internal
extend.formula <- function(formula = ~ 0,
                           predictors = NULL,
                           auxiliary = TRUE,
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
  
  if (auxiliary) formula <- update(formula, fr, ...)
  if (include.intercept) formula <- update(formula, ~ . + 1, ...)
  formula
}

#' Name formula list elements
#'
#' This helper function names any unnaned elements in the \code{formula} 
#' list. This is a convenience function.
#' @inheritParams mice
#' @param prefix A character vector of length 1 with the prefix to
#' be using for naming any unnamed blocks with two or more variables.
#' @return Named list of formulas
#' @seealso \code{\link{mice}}
#' @details 
#' This function will name any unnamed list elements specified in 
#' the optional argument \code{formula}. Unnanmed formula's 
#' consisting with just one response variable will be named 
#' after this variable. Unnamed formula's containing more 
#' than one variable will be named by the \code{prefix} 
#' argument, padded by an integer sequence stating at 1.
#' @examples
#' # fully conditionally specified main effects model
#' form1 <- list(bmi ~ age + chl + hyp, 
#'               hyp ~ age + bmi + chl,
#'               chl ~ age + bmi + hyp)
#' form1 <- name.formulas(form1)
#' imp1 <- mice(nhanes, formulas = form1, print = FALSE, m = 1, seed = 12199)
#' 
#' # same model using dot notation
#' form2 <- list(bmi ~ ., hyp ~ ., chl ~ .)
#' form2 <- name.formulas(form2, data = nhanes)
#' imp2 <- mice(nhanes, formulas = form2, print = FALSE, m = 1, seed = 12199)
#' identical(complete(imp1), complete(imp2))
#' 
#' # same model using predictorMatrix
#' imp3 <- mice(nhanes, print = FALSE, m = 1, seed = 12199, auxiliary = TRUE)
#' identical(complete(imp1), complete(imp3))
#' 
#' # multivariate imputation for chl and bmi
#' form4 <- list(chl + bmi ~ ., hyp ~ bmi + age)
#' form4 <- name.formulas(form3, data = nhanes)
#' imp4 <- mice(nhanes, formulas = form3, print = FALSE, m = 1, seed = 71712)
#' @export
name.formulas <- function(formulas, data = NULL, prefix = "F") {
  if (!is.list(formulas))
    stop("Argument `formulas` not a list", call. = FALSE)
  if (!all(sapply(formulas, is.formula)))
    stop("Not all elements in `formulas` are a formula")
  if (is.null(names(formulas))) names(formulas) <- rep("", length(formulas))
  inc <- 1
  for (i in seq_along(formulas)) {
    if (names(formulas)[i] != "") next
    #if (hasdot(formulas[[i]]) && is.null(data)) 
    #  stop("Formula with dot requires `data` argument", call. = FALSE)
    y <- lhs(formulas[[i]])
    if (length(y) == 1) names(formulas)[i] <- y
    else {
      names(formulas)[i] <- paste0(prefix, inc)
      inc <- inc + 1
    }
  }
  formulas
}

# lhs <- function(formula, data) {
#   # taken from mitml
#   ft <- terms(formula, data = data)
#   tl <- attr(ft,"term.labels")
#   vrs <- attr(ft,"variables")[-1]
# 
#   yvrs <- as.character(vrs)[attr(ft,"response")]
#   yvrs <- gsub("[\r\n]","",yvrs)
#   y.fml <- as.formula(paste0("~",yvrs))
#   yvrs <- attr(terms(y.fml), "term.labels")
#   yvrs
# }

lhs <- function(x) all.vars(update(x, . ~ 1))

extract.blocks <- function(formulas) {
  if (!all(sapply(formulas, is.formula))) return(NULL)
  name.blocks(lapply(formulas, lhs))
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
