### -----------------------------MICE.IMPUTE.PMM-------------------------

#'Imputation by predictive mean matching
#'
#'Imputes univariate missing data using predictive mean matching
#'
#'Imputation of \code{y} by predictive mean matching, based on Rubin (1987, p.
#'168, formulas a and b).  The procedure is as follows: \enumerate{ \item
#'Estimate beta and sigma by linear regression \item Draw beta* and sigma* from
#'the proper posterior \item Compute predicted values for \code{yobs}beta and
#'\code{ymis}beta* \item For each \code{ymis}, find the observation with
#'closest predicted value, and take its observed value in \code{y} as the
#'imputation.  \item If there is more than one candidate, make a random draw
#'among them.  Note: The matching is done on predicted \code{y}, NOT on
#'observed \code{y}. } 
#'
#'@note \code{mice.impute.pmm2()} was used in \code{mice 2.13} and 
#'after a faster alternative to \code{mice.impute.pmm()}. 
#'Starting with \code{mice 2.14}, \code{mice.impute.pmm()} has been 
#'replaced by \code{mice.impute.pmm2()}. The \code{mice.impute.pmm2()}
#'function will be depricated in future versions of \pkg{mice}.
#'
#'@aliases mice.impute.pmm pmm
#'@param y Numeric vector with incomplete data
#'@param ry Response pattern of \code{y} (\code{TRUE}=observed,
#'\code{FALSE}=missing)
#'@param x Design matrix with \code{length(y)} rows and \code{p} columns
#'containing complete covariates.
#'@param ... Other named arguments.
#'@return Numeric vector of length \code{sum(!ry)} with imputations
#'@author Stef van Buuren, Karin Groothuis-Oudshoorn, 2000, 2012
#'@references Little, R.J.A. (1988), Missing data adjustments in large surveys
#'(with discussion), Journal of Business Economics and Statistics, 6, 287--301.
#'
#'Rubin, D.B. (1987). Multiple imputation for nonresponse in surveys. New York:
#'Wiley.
#'
#'Van Buuren, S., Brand, J.P.L., Groothuis-Oudshoorn C.G.M., Rubin, D.B. (2006)
#'Fully conditional specification in multivariate imputation.  \emph{Journal of
#'Statistical Computation and Simulation}, \bold{76}, 12, 1049--1064.
#'
#'Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}: Multivariate
#'Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical
#'Software}, \bold{45}(3), 1-67. \url{http://www.jstatsoft.org/v45/i03/}
#'@keywords datagen
#'@export
mice.impute.pmm <- function(y, ry, x, ...) 
    # Imputation of y by predictive mean matching, based on
    # Rubin (p. 168, formulas a and b).
    # The procedure is as follows:
    # 1. Draw beta and sigma from the proper posterior
    # 2. Compute predicted values for yobs and ymis
    # 3. For each ymis, find the three observations with closest predicted value, 
    #    sample one randomly, and take its observed y as the imputation.
    # NOTE: The matching is on yhat, NOT on y, which deviates from formula b.
    # ry=TRUE if y observed, ry=FALSE if y missing
    #
    # Authors: S. van Buuren and K. Groothuis-Oudshoorn
# Version 10/2/2010: yhatobs is calculated using the estimated 
#                    rather than the drawn regression weights
#                    this creates between imputation variability 
#                    for the one-predictor case
# Version 06/12/2010 A random draw is made from the closest THREE donors.
# Version 25/04/2012 Extended to work with factors
# version 31/10/2012 Using faster pmm2
{
    x <- cbind(1, as.matrix(x))
    ynum <- y
    if (is.factor(y)) 
        ynum <- as.integer(y)  ## added 25/04/2012
    parm <- .norm.draw(ynum, ry, x, ...)  ## bug fix 10apr2013
    yhatobs <- x[ry, ] %*% parm$coef
    yhatmis <- x[!ry, ] %*% parm$beta
    return(apply(as.array(yhatmis), 1, .pmm.match, yhat = yhatobs, y = y[ry], ...))
}

# -------------------------.PMM.MATCH-------------------------------- 
#' Finds an imputed value from matches in the predictive metric
#' 
#' This function finds matches among the observed data in the predictive 
#' mean metric. It selects the \code{donors} closest matches, randomly 
#' samples one of the donors, and returns the observed value of the
#' match.
#' 
#'@aliases .pmm.match
#'@param z A scalar containing the predicted value for the current case
#'to be imputed.
#'@param yhat A vector containing the predicted values for all cases with an observed 
#'outcome.
#'@param y A vector of \code{length(yhat)} elements containing the observed outcome
#'@param donors The size of the donor pool among which a draw is made. The default is 
#'\code{donors = 5}. Setting \code{donors = 1} always selects the closest match. Values 
#'between 3 and 10 provide the best results. Note: This setting was changed from 
#'3 to 5 in version 2.19, based on simulation work by Tim Morris (UCL).
#'@param \dots Other parameters (not used).
#'@return A scalar containing the observed value of the selected donor.
#'@author Stef van Buuren
#'@references
#'Schenker N \& Taylor JMG (1996) Partially parametric techniques 
#'for multiple imputation. \emph{Computational Statistics and Data Analysis}, 22, 425-446.
#'
#'Little RJA (1988) Missing-data adjustments in large surveys (with discussion). 
#'\emph{Journal of Business Economics and Statistics}, 6, 287-301.
#'
#'@export
.pmm.match <- function(z, yhat = yhat, y = y, donors = 5, ...) {
    d <- abs(yhat - z)
    f <- d > 0
    a1 <- ifelse(any(f), min(d[f]), 1)
    d <- d + runif(length(d), 0, a1/10^10)
    if (donors == 1) 
        return(y[which.min(d)])
    ds <- sort.int(d, partial = donors)
    m <- sample(y[d <= ds[donors]], 1)
    return(m)
}


### -----------------------------MICE.IMPUTE.PMM2------------------------ A faster version of mice.impute.pmm()
mice.impute.pmm2 <- function(y, ry, x, ...) {
    mess <- "Method 'pmm2' is replaced by method 'pmm'"
    stop(mess)
}

