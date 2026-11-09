#' Internal function to apply LASSO selection on imputation model predictors
#'
#' @inheritParams mice.impute.lasso.select.norm
#' @param nfolds The number of folds for the cross-validation of the lasso penalty.
#' The default is 10.
#' @return Matrix with pruned predictor data, same type and size as \code{x}
#'
#' @author Edoardo Costantini (2021)
#' 
#' @keywords internal
#' @noRd
.mice.impute.lasso.select <- function(
    y,
    ry,
    x,
    wy = NULL,
    nfolds = 10,
    ...
) {
  
  x_glmnet <- cbind(1, x)
  xobs <- x_glmnet[ry, , drop = FALSE]
  xmis <- x[wy, ]
  yobs <- y[ry]
  
  # Train imputation model
  # used later in the estiamtion require this.
  cv_lasso <- glmnet::cv.glmnet(
    x = xobs,
    y = yobs,
    family = "gaussian",
    nfolds = nfolds,
    alpha = 1
  )
  
  # Define Active Set
  glmnet_coefs <- as.matrix(coef(cv_lasso, s = "lambda.min"))[, 1]
  AS <- which((glmnet_coefs != 0)[-1]) # Non-zero reg coefficients
  
  # Perform regular norm draw from Bayesian linear model
  xas <- x_glmnet[, AS, drop = FALSE]
  
  return(xas)
}