#' Creates a \code{formulas} object
#'
#' This helper function creates a valid \code{formulas} object. The 
#' \code{formulas} object is an argument to the \code{mice} function. 
#' It is a list of formula's that specifies the target variables and 
#' the predictors by means of the standard \code{~} operator.
#' @param data A \code{data.frame} with the source data
#' @param blocks An optional specification for blocks of variables in 
#' the rows. The default assigns each variable in its own block.
#' @param predictorMatrix A \code{predictorMatrix} specified by the user.
#' @param mode A character vector of length 1, either \code{"formula"} or
#' \code{"type"}, signalling whether the underlying imputation functions 
#' should be call with argument \code{"type"} or \code{"formula"}.
#' @return A list of formula's.
#' @seealso \code{\link{make.blocks}}, \code{\link{make.predictorMatrix}}
#' @examples
#' f1 <- make.formulas(nhanes)
#' f1
#' f2 <- make.formulas(nhanes, blocks = make.blocks(nhanes, "collect"))
#' f2
#' 
#' # for editing, it may be easier to work with the character vector
#' c1 <- as.character(f1)
#' c1
#' 
#' # fold it back into a formula list
#' f3 <- name.formulas(lapply(c1, as.formula))
#' f3
#' 
#' @export
make.formulas <- function(data, blocks = make.blocks(data), 
                          predictorMatrix = NULL, mode = "formula") {
  formulas <- as.list(rep("~ 0", length(blocks)))
  names(formulas) <- names(blocks)
  
  for (h in names(blocks)) {
    y <- blocks[[h]]
    if (is.null(predictorMatrix)) {
      predictors <- colnames(data)
    } else {
      type <- predictorMatrix[h, ]
      predictors <- names(type)[type != 0]
    }
    x <- setdiff(predictors, y)
    formulas[[h]] <- paste(paste(y, collapse = "+"), "~", 
                           paste(c("0", x), collapse = "+"))
  }
  
  formulas <- lapply(formulas, as.formula)
  
  # determine blocks with no specified formula
  attr(formulas, "mode.formula") <- sapply(formulas, 
                                           function(x) mode == "formula")
  formulas
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
#' # same model using repeated multivariate imputation
#' form3 <- name.blocks(list(all = bmi + hyp + chl ~ .))
#' imp3 <- mice(nhanes, formulas = form3, print = FALSE, m = 1, seed = 12199)
#' cmp3 <- complete(imp3)
#' identical(complete(imp1), complete(imp3))
#' 
#' # same model using predictorMatrix
#' imp4 <- mice(nhanes, print = FALSE, m = 1, seed = 12199, auxiliary = TRUE)
#' identical(complete(imp1), complete(imp4))
#' 
#' # different model: multivariate imputation for chl and bmi
#' form5 <- list(chl + bmi ~ ., hyp ~ bmi + age)
#' form5 <- name.formulas(form5, data = nhanes)
#' imp5 <- mice(nhanes, formulas = form5, print = FALSE, m = 1, seed = 71712)
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
  
  formulas <- handle.oldstyle.formulas(formulas, data)
  
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
  attr(formulas, "mode.formula") <- !sapply(formulas, 
                                            is.empty.model.data, 
                                            data = data)
  
  # extend formulas with predictorMatrix
  for (h in names(blocks)) {
    if (is.null(predictorMatrix)) {
      if (length(blocks[[h]]) == 1) predictors <- setdiff(colnames(data), h)
      else predictors <- setdiff(colnames(data), blocks[[h]])
    } else {
      type <- predictorMatrix[h, ]
      predictors <- names(type)[type != 0]
    }
    ff <- extend.formula(formula = formulas[[h]], predictors = predictors, ...)
    formulas[[h]] <- ff
  }
  
  # store
  setup$formulas.arg <- setup$formulas
  setup$formulas <- formulas
  setup
}

#' Extends formula's with predictor matrix settings
#' 
#' @inheritParams mice
#' @return A list of formula's
#' @param auxiliary A logical that indicates whether the variables
#' listed in \code{predictors} should be added to the formula as main 
#' effects. The default is \code{TRUE}.
#' @param include.intercept A logical that indicated whether the intercept 
#' should be included in the result.
#' @keywords internal
extend.formulas <- function(formulas, data, blocks, predictorMatrix = NULL, 
                            auxiliary = TRUE,
                            include.intercept = FALSE, 
                            ...) {
  # Extend formulas with predictorMatrix
  if (is.null(predictorMatrix)) return(formulas)
  for (h in names(blocks)) {
    type <- predictorMatrix[h, ]
    predictors <- names(type)[type != 0]
    ff <- extend.formula(formula = formulas[[h]], 
                         predictors = predictors, 
                         auxiliary = auxiliary, 
                         include.intercept = include.intercept)
    formulas[[h]] <- ff
  }
  formulas
}

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



handle.oldstyle.formulas <- function(formulas, data) {
  # converts old-style character vector to formula list
  oldstyle <- length(formulas) == ncol(data) && is.vector(formulas) && 
    is.character(formulas)
  if (!oldstyle) return(formulas)
  formulas[formulas != ""] <- "~ 0"
  fl <- as.list(formulas)
  names(fl) <- names(formulas)
  fl
}


is.empty.model.data <- function (x, data) 
{
  tt <- terms(x, data = data)
  (length(attr(tt, "factors")) == 0L) & (attr(tt, "intercept") == 0L)
}

lhs <- function(x) all.vars(update(x, . ~ 1))

is.formula <- function(x){
  inherits(x, "formula")
}

hasdot <- function(f) {
  if(is.recursive(f)) {
    return(any(sapply(as.list(f), hasdot)))
  } else {
    f == as.symbol(".")}
}

expand.dots <- function(formula, data) {
  if (!is.formula(formula)) return(formula)
  if (!hasdot(formula)) return(formula)
  
  y <- lhs(formula)
  x <- setdiff(colnames(data), y)
  fs <- paste(paste(y, collapse = "+"), "~", paste(c("0", x), collapse = "+"))
  as.formula(fs)
}
