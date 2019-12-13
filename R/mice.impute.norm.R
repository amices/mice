#'Imputation by Bayesian linear regression
#' 
#'Calculates imputations for univariate missing data by Bayesian linear 
#'regression, also known as the normal model.
#'
#'@aliases mice.impute.norm norm
#'@inheritParams mice.impute.pmm
#'@return Vector with imputed data, same type as \code{y}, and of length 
#'\code{sum(wy)}
#'@author Stef van Buuren, Karin Groothuis-Oudshoorn
#'@details
#' Imputation of \code{y} by the normal model by the method defined by 
#' Rubin (1987, p. 167). The procedure is as follows:
#'
#'\enumerate{
#'\item{Calculate the cross-product matrix \eqn{S=X_{obs}'X_{obs}}.}
#'\item{Calculate \eqn{V = (S+{diag}(S)\kappa)^{-1}}, with some small ridge 
#'parameter \eqn{\kappa}.}
#'\item{Calculate regression weights \eqn{\hat\beta = VX_{obs}'y_{obs}.}}
#'\item{Draw a random variable \eqn{\dot g \sim \chi^2_\nu} with \eqn{\nu=n_1 - q}.}
#'\item{Calculate \eqn{\dot\sigma^2 = (y_{obs} - X_{obs}\hat\beta)'(y_{obs} - X_{obs}\hat\beta)/\dot g.}}
#'\item{Draw \eqn{q} independent \eqn{N(0,1)} variates in vector \eqn{\dot z_1}.}
#'\item{Calculate \eqn{V^{1/2}} by Cholesky decomposition.}
#'\item{Calculate \eqn{\dot\beta = \hat\beta + \dot\sigma\dot z_1 V^{1/2}}.}
#'\item{Draw \eqn{n_0} independent \eqn{N(0,1)} variates in vector \eqn{\dot z_2}.}
#'\item{Calculate the \eqn{n_0} values \eqn{y_{imp} = X_{mis}\dot\beta + \dot z_2\dot\sigma}.}
#'}
#'
#'Using \code{mice.impute.norm} for all columns emulates Schafer's NORM method (Schafer, 1997).
#'@references 
#'Rubin, D.B (1987). Multiple Imputation for Nonresponse in Surveys. New York: John Wiley & Sons.
#'
#'Schafer, J.L. (1997). Analysis of incomplete multivariate data. London: Chapman & Hall.
#'@family univariate imputation functions
#'@keywords datagen
#'@export
mice.impute.norm <- function(y, ry, x, wy = NULL, ...) {
  if (is.null(wy)) wy <- !ry
  x <- cbind(1, as.matrix(x))
  parm <- .norm.draw(y, ry, x, ...)
  return(x[wy, ] %*% parm$beta + rnorm(sum(wy)) * parm$sigma)
}

#' Draws values of beta and sigma by Bayesian linear regression
#' 
#' This function draws random values of beta and sigma under the Bayesian 
#' linear regression model as described in Rubin (1987, p. 167). This function
#' can be called by user-specified imputation functions.
#' 
#'@aliases norm.draw .norm.draw
#'@param y Incomplete data vector of length \code{n}
#'@param ry Vector of missing data pattern (\code{FALSE}=missing,
#'\code{TRUE}=observed)
#'@param x Matrix (\code{n} x \code{p}) of complete covariates.
#'@param rank.adjust Argument that specifies whether \code{NA}'s in the 
#'coefficients need to be set to zero. Only relevant when \code{ls.meth = "qr"} 
#'AND the predictor matrix is rank-deficient.
#'@param ... Other named arguments.
#'@return A \code{list} containing components \code{coef} (least squares estimate),
#'\code{beta} (drawn regression weights) and \code{sigma} (drawn value of the 
#'residual standard deviation).
#'@references
#'Rubin, D.B. (1987). \emph{Multiple imputation for nonresponse in surveys}. New York: Wiley.
#'@author Gerko Vink, 2018, for this version, based on earlier versions written 
#'by Stef van Buuren, Karin Groothuis-Oudshoorn, 2017
#'@export
norm.draw <- function(y, ry, x, rank.adjust = TRUE, ...) 
  return(.norm.draw(y, ry, x, rank.adjust = TRUE, ...))

###'@rdname norm.draw
###'@export
.norm.draw <- function (y, ry, x, rank.adjust = TRUE, ...){
  p <- estimice(x[ry, , drop = FALSE], y[ry], ...)
  sigma.star <- sqrt(sum((p$r)^2)/rchisq(1, p$df))
  beta.star <- p$c + (t(chol(sym(p$v))) %*% rnorm(ncol(x))) * sigma.star
  parm <- list(p$c, beta.star, sigma.star, p$ls.meth)
  names(parm) <- c("coef", "beta", "sigma", "estimation")
  if(any(is.na(parm$coef)) & rank.adjust){
    parm$coef[is.na(parm$coef)] <- 0
    parm$beta[is.na(parm$beta)] <- 0
  }
  return(parm)
}

#' Computes least squares parameters
#' 
#' This function computes least squares estimates, variance/covariance matrices, 
#' residuals and degrees of freedom according to ridge regression, QR decomposition 
#' or Singular Value Decomposition. This function is internally called by .norm.draw(), 
#' but can be called by any user-specified imputation function.
#' 
#' When calculating the inverse of the crossproduct of the predictor matrix, 
#' problems may arise. For example, taking the inverse is not possible when the 
#' predictor matrix is rank deficient, or when the estimation problem is 
#' computationally singular. This function detects such error cases and 
#' automatically falls back to adding a ridge penalty to the diagonal of the 
#' crossproduct to allow for proper calculation of the inverse. 
#' 
#'@aliases estimice
#'@param x Matrix (\code{n} x \code{p}) of complete covariates.
#'@param y Incomplete data vector of length \code{n}
#'@param ls.meth the method to use for obtaining the least squares estimates. By 
#'default parameters are drawn by means of QR decomposition. 
#'@param ridge A small numerical value specifying the size of the ridge used. 
#' The default value \code{ridge = 1e-05} represents a compromise between stability
#' and unbiasedness. Decrease \code{ridge} if the data contain many junk variables.
#' Increase \code{ridge} for highly collinear data. 
#'@param ... Other named arguments.
#'@return A \code{list} containing components \code{c} (least squares estimate),
#'\code{r} (residuals), \code{v} (variance/covariance matrix) and \code{df} 
#'(degrees of freedom).
#'@author Gerko Vink, 2018
#'@export
estimice <- function(x, y, ls.meth = "qr", ridge = 1e-05, ...){
  df <- max(length(y) - ncol(x), 1)
  if (ls.meth == "qr"){
    qr <- lm.fit(x = x, y = y)
    c <- t(qr$coef)
    f <- qr$fitted.values
    r <- t(qr$residuals)
    v <- try(solve(as.matrix(crossprod(qr.R(qr$qr)))), silent = TRUE)
    if(inherits(v, "try-error")){ 
      xtx <- as.matrix(crossprod(qr.R(qr$qr)))
      pen <- diag(xtx) * ridge #calculate ridge penalty
      v <- solve(xtx + diag(pen)) #add ridge penalty to allow inverse of v
      mess <- "* A ridge penalty had to be used to calculate the inverse crossproduct of the predictor matrix. Please remove duplicate variables or unique respondent names/numbers from the imputation model. It may be advisable to check the fraction of missing information (fmi) to evaluate the validity of the imputation model"
      updateLog(out = mess, frame = 6)
      if (get("printFlag", parent.frame(search.parents("printFlag"))))
        cat("*") #indicator of added ridge penalty in the printed iteration history
    }
    return(list(c=t(c), r=t(r), v=v, df=df, ls.meth=ls.meth))
  } 
  if (ls.meth == "ridge"){
    xtx <- crossprod(x)
    pen <- ridge * diag(xtx)
    if (length(pen) == 1) 
      pen <- matrix(pen)
    v <- solve(xtx + diag(pen))
    c <- t(y) %*% x %*% v
    r <- y - x %*% t(c)
    return(list(c=t(c), r=r, v=v, df=df, ls.meth=ls.meth))
  }  
  if (ls.meth == "svd"){
    s <- svd(x)
    c <- s$v %*% ((t(s$u) %*% y) / s$d)
    f <- x %*% c
    r <- f - y
    v <- try(solve(s$v %*% diag(s$d)^2 %*% t(s$v)), silent = TRUE)
    if(inherits(v, "try-error")){
      xtx <- s$v %*% diag(s$d)^2 %*% t(s$v)
      pen <- diag(xtx) * ridge #calculate ridge penalty
      v <- solve(xtx + diag(pen)) #add ridge penalty to allow inverse of v
      mess <- "* A ridge penalty had to be used to calculate the inverse crossproduct of the predictor matrix. Please remove duplicate variables or unique respondent names/numbers from the imputation model. It may be advisable to check the fraction of missing information (fmi) to evaluate the validity of the imputation model"
      updateLog(out = mess, frame = 6)
      if (get("printFlag", parent.frame(search.parents("printFlag"))))
        cat("*") #indicator of added ridge penalty in the printed iteration history
    }
    return(list(c=c, r=r, v=v, df=df, ls.meth=ls.meth))
  }
}

search.parents <- function(name, start = 4){
  while(inherits(try(get("printFlag", parent.frame(start)), silent = TRUE), 
                 "try-error")){
    start = start + 1
  }
  start
}