#------------------------------pool-------------------------------

#'Multiple imputation pooling
#'
#'Pools the results of m repeated complete data analysis
#'
#'The function averages the estimates of the complete data model, computes the
#'total variance over the repeated analyses, and computes the relative increase
#'in variance due to nonresponse and the fraction of missing information. The
#'function relies on the availability of \enumerate{ \item the estimates of the
#'model, typically present as 'coefficients' in the fit object \item an
#'appropriate estimate of the variance-covariance matrix of the estimates per
#'analyses (estimated by \code{\link{vcov}}.  } The function pools also
#'estimates obtained with \code{lme()} and \code{lmer()}, BUT only the fixed
#'part of the model.
#'
#'@aliases pool
#'@param object An object of class \code{mira}, produced by \code{with.mids()} or \code{as.mira()}
#'@param method A string describing the method to compute the degrees of
#'freedom.  The default value is "smallsample", which specifies the is
#'Barnard-Rubin adjusted degrees of freedom (Barnard and Rubin, 1999) for small
#'samples. Specifying a different string produces the conventional degrees of
#'freedom as in Rubin (1987).
#'@return An object of class \code{mipo}, which stands for 'multiple imputation
#'pooled outcome'. 
#'@author Stef van Buuren, Karin Groothuis-Oudshoorn, 2009
#'@seealso \code{\link{with.mids}}, \code{\link{as.mira}}, \code{\link{vcov}}
#'@references Barnard, J. and Rubin, D.B. (1999). Small sample degrees of
#'freedom with multiple imputation. \emph{Biometrika}, 86, 948-955.
#'
#'Rubin, D.B. (1987).  \emph{Multiple Imputation for Nonresponse in Surveys}.
#'New York: John Wiley and Sons.
#'
#'van Buuren S and Groothuis-Oudshoorn K (2011). \code{mice}: Multivariate
#'Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical
#'Software}, \bold{45}(3), 1-67. \url{http://www.jstatsoft.org/v45/i03/}
#'
#'Pinheiro, J.C. and Bates, D.M. (2000).  \emph{Mixed-Effects Models in S and
#'S-PLUS}.  Berlin: Springer.
#'@keywords htest
#'@examples
#'
#'# which vcov methods can R find
#'methods(vcov)
#'
#'# 
#'imp <- mice(nhanes)
#'fit <- with(data=imp,exp=lm(bmi~hyp+chl))
#'pool(fit)
#'
#'#Call: pool(object = fit)
#'#
#'#Pooled coefficients:
#'#(Intercept)         hyp         chl 
#'#  22.01313    -1.45578     0.03459 
#'#
#'#Fraction of information about the coefficients missing due to nonresponse: 
#'#(Intercept)         hyp         chl 
#'#    0.29571     0.05639     0.38759 
#'#> summary(pool(fit))
#'#                 est      se       t     df Pr(>|t|)    lo 95    hi 95 missing
#'#(Intercept) 22.01313 4.94086  4.4553 12.016 0.000783 11.24954 32.77673      NA
#'#hyp         -1.45578 2.26789 -0.6419 20.613 0.528006 -6.17752  3.26596       8
#'#chl          0.03459 0.02829  1.2228  9.347 0.251332 -0.02904  0.09822      10
#'#               fmi
#'#(Intercept) 0.29571
#'#hyp         0.05639
#'#chl         0.38759
#'# 
#'
#'@export
pool <- function (object, method = "smallsample")
{
  ### General pooling function for multiple imputation parameters
  ### object: an object of class mira (Multiple Imputed Repeated Analysis)
  ### Based on Rubin's rules (Rubin, 1987);
  #
  ### Stef van Buuren, Karin Groothuis-Oudshoorn, July 1999.
  ### Extended for mle (S3) and mer (S4) objects, KO 2009.
  ### Updated V2.1 - Aug 31, 2009
  ### Updated V2.2 - Jan 13, 2010
  ### Updated V2.4 - Oct 12, 2010
  ### Updated V2.6 - Jan 14, 2011
  ### Updated V2.12 - Mar 19, 2012
  
  ### Check the arguments
  
  call <- match.call()
  if (!is.mira(object))
    stop("The object must have class 'mira'")
  m <- length(object$analyses)
  fa <- getfit(object, 1)
  if (m == 1) {
    warning("Number of multiple imputations m=1. No pooling done.")
    return(fa)
  }
  analyses <- getfit(object)
  
  if (class(fa)[1]=="lme" &&
      !requireNamespace("nlme", quietly = TRUE))
    stop("Package 'nlme' needed fo this function to work. Please install it.", call. = FALSE)
  if ((class(fa)[1]=="mer" || class(fa)[1] == "lmerMod" || inherits(fa,"merMod")) &&  
      !requireNamespace("lme4", quietly = TRUE))
    stop("Package 'lme4' needed fo this function to work. Please install it.", call. = FALSE)
  
  ###   Set up arrays for object.
  
  mess <- try(coef(fa), silent=TRUE)
  if (inherits(mess,"try-error")) stop("Object has no coef() method.")
  mess <- try(vcov(fa), silent=TRUE)
  if (inherits(mess,"try-error")) stop("Object has no vcov() method.")
  
  if (class(fa)[1]=="mer" || class(fa)[1] == "lmerMod" || inherits(fa,"merMod"))  # 14jun2014
  { 
    k <- length(lme4::fixef(fa))
    names <- names(lme4::fixef(fa))
  }
  else if (class(fa)[1]=="polr")          # fixed 17/10/2010
  {
    k <- length(coef(fa))+length(fa$zeta)
    names <- c(names(coef(fa)),names(fa$zeta))
  }
  else
  {
    k <- length(coef(fa))
    names <- names(coef(fa))
  }
  
  qhat <- matrix(NA, nrow = m, ncol = k, dimnames = list(seq_len(m), names))
  u <- array(NA, dim = c(m, k, k), dimnames = list(seq_len(m), names,
                                                   names))
  
  ###   Fill arrays
  
  for (i in seq_len(m)) {
    fit <- analyses[[i]]
    if (class(fit)[1] == "mer")
    {
      qhat[i,] <- lme4::fixef(fit)
      ui <- as.matrix(vcov(fit))
      if (ncol(ui)!=ncol(qhat)) stop("Different number of parameters: class mer, fixef(fit): ",ncol(qhat),", as.matrix(vcov(fit)): ", ncol(ui))
      u[i, ,] <- array(ui, dim = c(1, dim(ui)))
    }
    else if (class(fit)[1] == "lmerMod" || inherits(fa,"merMod"))
    {
      qhat[i,] <- lme4::fixef(fit)
      ui <- vcov(fit)
      if (ncol(ui)!=ncol(qhat)) 
        stop("Different number of parameters: class lmerMod, fixed(fit): ",ncol(qhat),", vcov(fit): ", ncol(ui))
      u[i, ,] <- array(ui, dim = c(1, dim(ui)))
    }
    else if (class(fit)[1] == "lme")
    {
      qhat[i,] <- fit$coefficients$fixed
      ui <- vcov(fit)
      if (ncol(ui)!=ncol(qhat)) stop("Different number of parameters: class lme, fit$coefficients$fixef: ",ncol(qhat),", vcov(fit): ", ncol(ui))
      u[i, ,] <- array(ui, dim = c(1, dim(ui)))
    }
    else if (class(fit)[1] == "polr")
    {
      qhat[i,] <- c(coef(fit),fit$zeta)
      ui <- vcov(fit)
      if (ncol(ui)!=ncol(qhat)) stop("Different number of parameters: class polr, c(coef(fit, fit$zeta): ",ncol(qhat),", vcov(fit): ", ncol(ui))
      u[i, ,] <- array(ui, dim = c(1, dim(ui)))
    }
    else if (class(fit)[1] == "survreg")
    {
      qhat[i,] <- coef(fit)
      ui <- vcov(fit)
      parnames <- dimnames(ui)[[1]]
      select <- !(parnames %in% "Log(scale)")  ## do not pool Log(scale) columns SvB 18/3/12
      ui <- ui[select, select]
      if (ncol(ui)!=ncol(qhat)) stop("Different number of parameters: class survreg, coef(fit): ",ncol(qhat),", vcov(fit): ", ncol(ui))
      u[i, ,] <- array(ui, dim = c(1, dim(ui)))
    }
    else
    {
      qhat[i,] <- coef(fit)
      ui <- vcov(fit)
      ### add rows and columns to ui if qhat is missing
      ui <- expandvcov(qhat[i,], ui)
      if (ncol(ui)!=ncol(qhat)) stop("Different number of parameters: coef(fit): ",ncol(qhat),", vcov(fit): ", ncol(ui))
      u[i, ,] <- array(ui, dim = c(1, dim(ui)))
    }
  }
  
  ###   Within, between and total variances
  
  qbar <- apply(qhat, 2, mean)                              # (3.1.2)
  ubar <- apply(u, c(2, 3), mean)                           # (3.1.3)
  e <- qhat - matrix(qbar, nrow = m, ncol = k, byrow = TRUE)
  b <- (t(e) %*% e)/(m - 1)                                 # (3.1.4)
  t <- ubar + (1 + 1/m) * b                                 # (3.1.5)
  
  ###   Scalar inference quantities
  
  r <- (1 + 1/m) * diag(b/ubar)                             # (3.1.7)
  lambda <- (1 + 1/m) * diag(b/t)
  dfcom <- df.residual(object)
  df <- mice.df(m, lambda, dfcom, method)
  fmi <- (r + 2/(df+3))/(r + 1)                             # fraction of missing information
  
  ###
  names(r) <- names(df) <- names(fmi) <- names(lambda) <- names
  fit <- list(call = call, call1 = object$call, call2 = object$call1,
              nmis = object$nmis, m = m, qhat = qhat, u = u, qbar = qbar,
              ubar = ubar, b = b, t = t, r = r, dfcom = dfcom, df = df,
              fmi = fmi, lambda = lambda)
  oldClass(fit) <- c("mipo", oldClass(object))              ## FEH
  return(fit)
}
