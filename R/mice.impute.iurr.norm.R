#' Imputation by Indirect Use of (lasso) Regularised (linear) Regression (IURR)
#'
#' Imputes univariate missing data using lasso 2-step linear regression
#'
#' @aliases mice.impute.iurr.norm iurr.norm
#' @inheritParams mice.impute.pmm
#' @param nfolds The number of folds for the cross-validation of the lasso penalty.
#' The default is 10.
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#' @details
#' Uses lasso penalty to identify an active set, then samples imputation model
#' parameter values and uses them to define a posterior predictive distirbution
#' to sample imputations.
#' The method is based on the Iirect Use of Regularized Regression proposed by
#' Zhao & Long (2016) and Deng et al (2016).
#' When using only mice.impute.iurr methods, the user can provide the default
#' predictor matrix. The method will then take care of selecting which variables are
#' important for imputation.
#' @author Edoardo Costantini, 2021
#' @references
#'
#' Deng, Y., Chang, C., Ido, M. S., & Long, Q. (2016). Multiple imputation for
#' general missing data patterns in the presence of high-dimensional data.
#' Scientific reports, 6(1), 1-10.
#'
#' Zhao, Y., & Long, Q. (2016). Multiple imputation in the presence of
#' high-dimensional data. Statistical Methods in Medical Research, 25(5),
#' 2021-2035.
#'
#' @family univariate imputation functions
#' @keywords datagen
#' @export
mice.impute.iurr.norm <- function(y, ry, x, wy = NULL, nfolds = 10, ...) {
  install.on.demand("glmnet", ...)

  # Body
  if (is.null(wy)) wy <- !ry
  xobs <- x[ry, ]
  xmis <- x[wy, ]
  yobs <- y[ry]

  # Train imputation model
  # used later in the estiamtion require this.
  cv_lasso <- glmnet::cv.glmnet(x = xobs[, -1], y = yobs,
                                family = "gaussian",
                                nfolds = nfolds,
                                alpha = 1)

  # Define Active Set
  glmnet_coefs <- as.matrix(coef(cv_lasso,
                                 s = "lambda.min"))[, 1]
  AS <- which(glmnet_coefs != 0)[-1] # Non-zero reg coefficinets

  # MLE estiamtes by Optimize loss function
  lm_dat <- data.frame(cbind(yobs, xobs[, AS, drop = FALSE]))
  lm_fit <- lm(yobs ~ ., data = lm_dat)
  X_mle  <- model.matrix(yobs ~ ., data = lm_dat)
  start_values <- c(coef(lm_fit), stats::sigma(lm_fit))
  MLE_fit <- stats::optim(start_values,
                          .lmLoss,
                          method = "BFGS",
                          hessian = T,
                          Y = yobs, X = X_mle)
  theta <- MLE_fit$par
  OI <- solve(MLE_fit$hessian) # parameters cov matrix

  # Sample parameters
  pdraws_par <- MASS::mvrnorm(1, mu = theta, Sigma = OI)

  # Posterior Predictive Draws
  df_ppd <- data.frame(y_place_holder = 1,
                       xmis[, AS, drop = FALSE])
  x_ppd  <- model.matrix(y_place_holder ~ ., data = df_ppd)
  b_ppd <- pdraws_par[-length(pdraws_par)] # betas for posterior pred dist
  s_ppd <- tail(pdraws_par, 1) # sigma for posterior pred dist
  y_imp <- rnorm(n = nrow(x_ppd),
                 mean = x_ppd %*% b_ppd,
                 sd = s_ppd)
  y_imp
}

# Internal function for lm loss function
.lmLoss <- function(theta, Y, X){
  # credits: https://rpubs.com/YaRrr/MLTutorial
  k <- ncol(X)
  beta <- theta[1:k]
  sigma <- theta[k+1]

  # Check validity of sigma
  if(sigma < 0) {
    # the optimization procedure to stay away from invalid parameter values.
    dev <- 1e7
  } else {
    # calculate (log) likelihood of each data point
    ll <- stats::dnorm(Y, mean = X %*% beta, sd = sigma, log = TRUE)
    # summarize into deviance score
    dev <- -2 * sum(ll)
  }
  # Return
  return(dev)
}