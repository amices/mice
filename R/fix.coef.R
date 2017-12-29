#'Fix coefficients and update model
#'
#'Refits a model with a specified set of coefficients.
#'
#'@param model An R model, e.g., produced by \code{lm} or \code{glm}
#'@param beta A numeric vector with \code{length(coef)} regression weights
#'@return An updated R model object
#'@author Stef van Buuren
#'@examples
#'fit0 <- lm(Volume ~ Girth + Height, data = trees)
#'coef(fit0)
#'deviance(fit0)
#'
#'# refit same model
#'fit1 <- fix.coef(fit0)
#'coef(fit1)
#'deviance(fit1)
#'
#'# change the beta's
#'fit2 <- fix.coef(fit0, beta = c(-50, 5, 1))
#'coef(fit2)
#'deviance(fit2)
#'
#'plot(predict(fit0), predict(fit1)); abline(0,1)
#'plot(predict(fit0), predict(fit2)); abline(0,1)
#'
#'\dontrun{
#'# it fails on missing data
#'fit0 <- lm(bmi ~ age + hyp + chl, data = nhanes2)
#'fit1 <- fix.coef(fit0)
#'}
#'@export
fix.coef <- function(model, beta = NULL) {
  oldcoef <- coef(model)
  if (is.null(beta)) beta <- oldcoef
  if (length(oldcoef) != length(beta)) 
    stop("incorrect length of 'beta'", call. = FALSE)
  
  # re-calculate model for new beta's
  mm <- model.matrix(formula(model), data = model$model)
  assign.to.global(".offset", as.vector(mm %*% beta))
  mod <- update(model, formula = . ~ 1 + offset(.offset))
  remove(".offset", envir = .GlobalEnv)
  mod
}

#' function loading results in global environment
#' @inheritParams base::assign
#' @keywords internal
#' @note This function will silence R CMD CHECK 
#' "Found the following assignments to the global environment:"
assign.to.global <- function(x, value, pos = 1) {
  assign(x, value, envir = as.environment(pos))
  }