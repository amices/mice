# --------------------------pool.r.squared--------------------------

#'Pooling: R squared
#'
#'Pools R^2 of m repeated complete data models.
#'
#'The function pools the coefficients of determination R^2 or the adjusted
#'coefficients of determination (R^2_a) obtained with the \code{lm} modelling
#'function. For pooling it uses the Fisher \emph{z}-transformation.
#'
#'@param object An object of class 'mira', produced by \code{lm.mids} or
#'\code{with.mids} with \code{lm} as modelling function.
#'@param adjusted A logical value. If adjusted=TRUE then the adjusted R^2 is
#'calculated.  The default value is FALSE.
#'@return Returns a 1x4 table with components. Component \code{est} is the 
#'pooled R^2 estimate. Component \code{lo95} is the 95 \% lower bound of the pooled R^2.
#'Component \code{hi95} is the 95 \% upper bound of the pooled R^2.
#'Component \code{fmi} is the fraction of missing information due to nonresponse.
#'@author Karin Groothuis-Oudshoorn and Stef van Buuren, 2009
#'@seealso \code{\link{pool}},\code{\link{pool.scalar}}
#'@references Harel, O (2009). The estimation of R^2 and adjusted R^2 in
#'incomplete data sets using multiple imputation, Journal of Applied Statistics,
#'36:1109-1118.
#'
#'Rubin, D.B. (1987). Multiple Imputation for Nonresponse in Surveys.  New
#'York: John Wiley and Sons.
#'
#'van Buuren S and Groothuis-Oudshoorn K (2011). \code{mice}: Multivariate
#'Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical
#'Software}, \bold{45}(3), 1-67. \url{http://www.jstatsoft.org/v45/i03/}
#'

#'@keywords htest
#'@examples
#'
#'
#'imp<-mice(nhanes)
#'
#'fit<-lm.mids(chl~age+hyp+bmi,imp)
#'pool.r.squared(fit)
#'pool.r.squared(fit,adjusted=TRUE)
#'
#'#fit<-lm.mids(chl~age+hyp+bmi,imp)
#'#
#'#> pool.r.squared(fit)
#'#          est     lo 95     hi 95       fmi
#'#R^2 0.5108041 0.1479687 0.7791927 0.3024413
#'#
#'#> pool.r.squared(fit,adjusted=TRUE)
#'#          est      lo 95    hi 95       fmi
#'#adj R^2 0.4398066 0.08251427 0.743172 0.3404165
#'#
#'
#'
#'@export
pool.r.squared <- function(object, adjusted = FALSE) {
    # pooled rsquared for multiple imputed datasets.
    # 
    # object: object of class mira based on article of O. Harel (Journal of Applied Statistics, 2009).
    
    call <- match.call()
    if (!is.mira(object)) 
        stop("The object must have class 'mira'")
    if ((m <- length(object$analyses)) < 2) 
        stop("At least two imputations are needed for pooling.\n")
    if (class((object$analyses[[1]]))[1] != "lm") 
        stop("r^2 can only be calculated for results of the 'lm' modelling function")
    # Set up array r2 to store R2 values, Fisher z-transformations of R2 values and its variance.
    analyses <- object$analyses
    m <- length(analyses)
    r2 <- matrix(NA, nrow = m, ncol = 3, dimnames = list(seq_len(m), c("R^2", "Fisher trans F^2", "se()")))
    # Fill arrays
    for (i in seq_len(m)) {
        fit <- analyses[[i]]
        r2[i, 1] <- if (!adjusted) sqrt(summary(fit)$r.squared) else sqrt(summary(fit)$adj.r.squared)
        r2[i, 2] <- 0.5 * log((r2[i, 1] + 1)/(1 - r2[i, 1]))
        r2[i, 3] <- 1/(length(summary(fit)$residuals) - 3)
    }
    # Compute within, between and total variances following Rubin's rules.  with function pool.scalar().
    fit <- pool.scalar(r2[, 2], r2[, 3])
    
    # Make table with results.
    table <- array(((exp(2 * fit$qbar) - 1)/(1 + exp(2 * fit$qbar)))^2, dim = c(1, 4))
    
    dimnames(table) <- if (!adjusted) list("R^2", c("est", "lo 95", "hi 95", "fmi")) else list("adj R^2", c("est", "lo 95", "hi 95", "fmi"))
    
    table[, 2] <- ((exp(2 * (fit$qbar - 1.96 * sqrt(fit$t))) - 1)/(1 + exp(2 * (fit$qbar - 1.96 * sqrt(fit$t)))))^2
    table[, 3] <- ((exp(2 * (fit$qbar + 1.96 * sqrt(fit$t))) - 1)/(1 + exp(2 * (fit$qbar + 1.96 * sqrt(fit$t)))))^2
    table[, 4] <- fit$f
    return(table)
}
