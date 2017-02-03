### -----------------------------MICE.IMPUTE.PMM-------------------------

#'Imputation by predictive mean matching
#'
#'Imputes univariate missing data using predictive mean matching
#'
#'Imputation of \code{y} by predictive mean matching, based on Rubin (1987, p.
#'168, formulas a and b).  The procedure is as follows: 
#'\enumerate{ 
#'\item
#'Estimate beta and sigma by linear regression 
#'\item Draw beta* and sigma* from
#'the proper posterior 
#'\item Compute predicted values for \code{yobs} \code{beta} and
#'\code{ymis} \code{beta*} 
#'\item For each \code{ymis}, find \code{donors} observations with
#'closest predicted values, randomly sample one of these, 
#'and take its observed value in \code{y} as the imputation.  
#'\item Ties are broken by making a random draw
#'among ties.  
#'Note: The matching is done on predicted \code{y}, NOT on
#'observed \code{y}.} 
#'
#'@note Since \code{mice 2.22} the standard \code{mice.impute.pmm()} calls
#'the much faster \code{matcher()} function instead of \code{.pmm.match()}. Since
#'\code{matcher()} uses its own random generator, results cannot be exactly 
#'reproduced. In case where you want the old \code{.pmm.match()}, specify 
#'\code{mice(..., version = "2.21")}.
#'
#'@aliases mice.impute.pmm pmm
#'@param y Numeric vector with incomplete data
#'@param ry Response pattern of \code{y} (\code{TRUE}=observed,
#'\code{FALSE}=missing)
#'@param x Design matrix with \code{length(y)} rows and \code{p} columns
#'containing complete covariates.
#'@param donors The size of the donor pool among which a draw is made. The default is 
#'\code{donors = 5}. Setting \code{donors = 1} always selects the closest match. Values 
#'between 3 and 10 provide the best results. Note: The default was changed from 
#'3 to 5 in version 2.19, based on simulation work by Tim Morris.
#'@param type Type of matching distance. The default choice \code{type = 1} calculates the distance between the predicted value of \code{yobs} and the drawn values of \code{ymis}. Other choices are \code{type = 0} (distance between predicted values) and \code{type = 2} (distance between drawn values). The current version supports only \code{type = 1}.
#'@param ridge The ridge penalty applied in \code{.norm.draw()} to prevent problems with multicollinearity. The default is \code{ridge = 1e-05}, which means that 0.01 percent of the diagonal is added to the cross-product. Larger ridges may result in more biased estimates. For highly noisy data (e.g. many junk variables), set \code{ridge = 1e-06} or even lower to reduce bias. For highly collinear data, set \code{ridge = 1e-04} or higher.
#'@param version A character variable indicating the version to be used. Specifying \code{version = "2.21"} calls \code{.pmm.match()} instead of the default
#'\code{matcher()} function.
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
mice.impute.pmm <- function(y, ry, x, donors = 5, type = 1, 
                            ridge = 1e-05, version = "", ...) 
{
    x <- cbind(1, as.matrix(x))
    ynum <- y
    if (is.factor(y)) ynum <- as.integer(y)  ## added 25/04/2012
    parm <- .norm.draw(ynum, ry, x, ridge = ridge, ...)  ## bug fix 10apr2013
    yhatobs <- x[ry, ] %*% parm$coef
    yhatmis <- x[!ry, ] %*% parm$beta
    if (version == "2.21") 
        return(apply(as.array(yhatmis), 1, 
                     .pmm.match, 
                     yhat = yhatobs, 
                     y = y[ry], 
                     donors = donors, ...))
    idx <- matcher(yhatobs, yhatmis, k = donors)
    return(y[ry][idx])
}

# -------------------------.PMM.MATCH-------------------------------- 
#' Finds an imputed value from matches in the predictive metric
#' 
#' This function finds matches among the observed data in the predictive 
#' mean metric. It selects the \code{donors} closest matches, randomly 
#' samples one of the donors, and returns the observed value of the
#' match.
#' 
#' Not used after \code{mice 2.21}. The \code{mice.impute.pmm()} function 
#' now calls the much faster \code{C} function \code{matcher} instead of 
#' \code{.pmm.match()}. Use \code{mice(..., version = "2.21")} to call
#' \code{.pmm.match()}
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
#'@rdname pmm.match
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
    donors <- min(donors, length(d))
    donors <- max(donors, 1)
    ds <- sort.int(d, partial = donors)
    m <- sample(y[d <= ds[donors]], 1)
    return(m)
}


### -----------------------------MICE.IMPUTE.PMM2------------------------ A faster version of mice.impute.pmm()
mice.impute.pmm2 <- function(y, ry, x, ...) {
    mess <- "Method 'pmm2' is replaced by method 'pmm'"
    stop(mess)
}

