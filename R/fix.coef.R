#' Fix coefficients and update model
#'
#' Refits a model with a specified set of coefficients.
#'
#' @param model An R model, e.g., produced by \code{lm} or \code{glm}
#' @param beta A numeric vector with \code{length(coef)} model coefficients.
#' If the vector is not named, the coefficients should be
#' given in the same order as in \code{coef(model)}. If the vector is named,
#' the procedure attempts to match on names.
#' @return An updated R model object
#' @author Stef van Buuren, 2018
#' @details
#' The function calculates the linear predictor using the new coefficients,
#' and reformulates the model using the \code{offset}
#' argument. The linear predictor is called
#' \code{offset}, and its coefficient will be \code{1} by definition.
#' The new model only fits the intercept, which should be \code{0}
#' if we set \code{beta = coef(model)}.
#' @examples
#' model0 <- lm(Volume ~ Girth + Height, data = trees)
#' formula(model0)
#' coef(model0)
#' deviance(model0)
#'
#' # refit same model
#' model1 <- fix.coef(model0)
#' formula(model1)
#' coef(model1)
#' deviance(model1)
#'
#' # change the beta's
#' model2 <- fix.coef(model0, beta = c(-50, 5, 1))
#' coef(model2)
#' deviance(model2)
#'
#' # compare predictions
#' plot(predict(model0), predict(model1))
#' abline(0, 1)
#' plot(predict(model0), predict(model2))
#' abline(0, 1)
#'
#' # compare proportion explained variance
#' cor(predict(model0), predict(model0) + residuals(model0))^2
#' cor(predict(model1), predict(model1) + residuals(model1))^2
#' cor(predict(model2), predict(model2) + residuals(model2))^2
#'
#' # extract offset from constrained model
#' summary(model2$offset)
#'
#' # it also works with factors and missing data
#' model0 <- lm(bmi ~ age + hyp + chl, data = nhanes2)
#' model1 <- fix.coef(model0)
#' model2 <- fix.coef(model0, beta = c(15, -8, -8, 2, 0.2))
#' @export
fix.coef <- function(model, beta = NULL) {
  oldcoef <- tidy.coef(model)
  if (is.null(beta)) beta <- oldcoef
  if (length(oldcoef) != length(beta)) {
    stop("incorrect length of 'beta'", call. = FALSE)
  }

  # handle naming
  if (is.null(names(oldcoef))) {
    names(oldcoef) <- make.names(seq_along(oldcoef))
  }
  if (is.null(names(beta))) {
    names(beta) <- names(oldcoef)
  } else {
    diff <- setdiff(names(oldcoef), names(beta))
    if (length(diff) > 0) {
      stop("names not found in 'beta': ", diff, call. = FALSE)
    }
    diff <- setdiff(names(beta), names(oldcoef))
    if (length(diff) > 0) {
      stop("names not found in 'coef(model)': ", diff, call. = FALSE)
    }
  }
  beta <- beta[names(oldcoef)]

  # re-calculate model for new beta's
  data <- model.frame(formula = formula(model), data = model.frame(model))
  mm <- model.matrix(formula(model, fixed.only = TRUE), data = data)
  # Problem: offset cannot be calculated for the Cox model because that does
  # not include the intercept
  if (inherits(model, "coxph")) {
    stop("D3 does not support the Cox model.", call. = FALSE)
  }
  offset <- as.vector(mm %*% beta)
  uf <- . ~ 1
  if (inherits(model, "merMod")) uf <- formula(model, random.only = TRUE)
  upd <- update(model,
    formula. = uf,
    data = cbind(data, offset = offset),
    offset = offset
  )
  upd
}

tidy.coef <- function(model) {
  est <- tidy(model, effects = "fixed")
  coef <- est$estimate
  names(coef) <- est$term
  coef
}
