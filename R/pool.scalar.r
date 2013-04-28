# ------------------------------pool.scalar----------------------------

#'Multiple imputation pooling: univariate version
#'
#'Pools univariate estimates of m repeated complete data analysis
#'
#'The function averages the univariate estimates of the complete data model,
#'computes the total variance over the repeated analyses, and computes the
#'relative increase in variance due to nonresponse and the fraction of missing
#'information.
#'
#'@param Q A vector of univariate estimates of m repeated complete data
#'analyses.
#'@param U A vector containing the corresponding m variances of the univariate
#'estimates.
#'@return Returns a list with components. Component \code{m} is the 
#'number of imputations. Component \code{qhat} contains the \code{m} 
#'univariate estimates of repeated complete data analyses.
#'Component \code{u} contains the corresponding \code{m} variances of the univariate estimates.
#'Component \code{qbar} is the pooled univariate estimate, formula (3.1.2) Rubin
#'(1987). Component \code{ubar} is the mean of the variances, formula (3.1.3) Rubin (1987).
#'Component \code{b} is the within imputation variance, formula (3.1.4) Rubin (1987).
#'Component \code{t} is the total variance of the pooled estimated, formula (3.1.5) Rubin
#'(1987).
#'Component \code{r} is the relative increase in variance due to nonresponse, formula
#'(3.1.7) Rubin (1987).
#'Component \code{df} is the degrees of freedom for t reference distribution, formula
#'(3.1.6) Rubin (1987).
#'Component \code{f} is the fraction missing information due to nonresponse, formula
#'(3.1.10) Rubin (1987).
#'@author Karin Groothuis-Oudshoorn and Stef van Buuren, 2009
#'@seealso \code{\link{pool}}
#'@references Rubin, D.B. (1987). Multiple Imputation for Nonresponse in
#'Surveys.  New York: John Wiley and Sons.
#'@keywords htest
#'@examples
#'
#'
#'imp <- mice(nhanes)
#'m <- imp$m
#'Q <- rep(NA,m)
#'U <- rep(NA,m)
#'for (i in 1:m) {
#'    Q[i] <- mean(complete(imp,i)$bmi)
#'    U[i] <- var(complete(imp,i)$bmi)
#'}
#'pool.scalar(Q,U)
#'
#'#pool.scalar(Q,U)
#'#$m
#'#[1] 5
#'#
#'#$qhat
#'#[1] 26.764 26.748 27.024 27.340 26.436
#'#
#'#$u
#'#[1] 17.85490 19.11677 20.61440 21.05750 15.16990
#'#
#'#$qbar
#'#[1] 26.8624
#'#
#'#$ubar
#'#[1] 18.76269
#'#
#'#$b
#'#[1] 0.1147008
#'#
#'#t
#'#[1] 18.90033
#'#
#'#$r
#'#[1] 0.007335885
#'#
#'#$df
#'#[1] 75422.96
#'#
#'#$f
#'#[1] 0.007308785
#'#
#'
#'@export
pool.scalar <- function(Q, U) {
    # Simple pooling function for univariate parameter
    # 
    # Based on Rubin's rules (Rubin, 1987);
    
    m <- length(Q)
    qbar <- mean(Q)  # (3.1.2)
    ubar <- mean(U)  # (3.1.3)
    b <- var(Q)  # (3.1.4)
    t <- ubar + (m + 1) * b/m  # (3.1.5)
    r <- (1 + 1/m) * b/ubar  # (3.1.7)
    df <- (m - 1) * (1 + 1/r)^2  # (3.1.6)
    f <- (r + 2/(df + 3))/(r + 1)  # (3.1.10)
    fit <- list(m = m, qhat = Q, u = U, qbar = qbar, ubar = ubar, b = b, t = t, r = r, df = df, f = f)
    return(fit)
}
