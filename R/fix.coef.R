#'Fix coefficients and update model
#'
#'Refits a model with a specified set of coefficients.
#'
#'@param model An R model, e.g., produced by \code{lm} or \code{glm}
#'@param beta A numeric vector with \code{length(coef)} regression weights
#'@return An updated R model object
#'@author Stef van Buuren, 2018
#'@examples
#'fit0 <- lm(Volume ~ Girth + Height, data = trees)
#'formula(fit0)
#'coef(fit0)
#'deviance(fit0)
#'
#'# refit same model
#'fit1 <- fix.coef(fit0)
#'formula(fit1)
#'coef(fit1)
#'deviance(fit1)
#'
#'# change the beta's
#'fit2 <- fix.coef(fit0, beta = c(-50, 5, 1))
#'coef(fit2)
#'deviance(fit2)
#'
#'# compare predictions
#'plot(predict(fit0), predict(fit1)); abline(0,1)
#'plot(predict(fit0), predict(fit2)); abline(0,1)
#'
#'# compare proportion explained variance
#'cor(predict(fit0), predict(fit0) + residuals(fit0))^2
#'cor(predict(fit1), predict(fit1) + residuals(fit1))^2
#'cor(predict(fit2), predict(fit2) + residuals(fit2))^2
#'
#'# extract offset from constrained model
#'summary(fit2$model$offset)
#'
#'# it also works with factors and missing data
#'fit0 <- lm(bmi ~ age + hyp + chl, data = nhanes2)
#'fit1 <- fix.coef(fit0)
#'fit2 <- fix.coef(fit0, beta = c(15, -8, -8, 2, 0.2))
#'@export
fix.coef <- function(model, beta = NULL) {
  oldcoef <- coef(model)
  if (is.null(beta)) beta <- oldcoef
  if (length(oldcoef) != length(beta)) 
    stop("incorrect length of 'beta'", call. = FALSE)
  if (is.null(names(beta))) names(beta) <- names(oldcoef)
  
  # re-calculate model for new beta's
  beta <- beta[names(oldcoef)]
  mm <- model.matrix(formula(model), data = model$model)
  offset <- as.vector(mm %*% beta)
  update(model, formula. = . ~ 1, 
         data = cbind(model$model, offset = offset),
         offset = offset)
}
