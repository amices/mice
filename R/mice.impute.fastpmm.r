### -----------------------------MICE.IMPUTE.FASTPMM-------------------------

#'Imputation by fast predictive mean matching
#'
#'Imputes univariate missing data using fast predictive mean matching
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
#'@note The \code{mice.impute.fastpmm()} function is an experimental
#'version of the standard \code{mice.impute.pmm()} function. 
#'In \code{mice 2.22} both are equivalent. In future versions of 
#'\code{mice} the \code{mice.impute.fastpmm()} function may be 
#'subject to additional optimizations. This is an experimental feature.
#'
#'@aliases mice.impute.fastpmm fastpmm
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
#'@param version A character variable indicating the version. Currently unused.
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
#'@seealso \code{\link{mice.impute.pmm}}
#'@export
mice.impute.fastpmm <- function(y, ry, x, donors = 5, type = 1, 
                                ridge = 1e-05, version = "", ...) 
{
    x <- cbind(1, as.matrix(x))
    ynum <- y
    if (is.factor(y)) 
        ynum <- as.integer(y)  ## added 25/04/2012
    parm <- .norm.draw(ynum, ry, x, ridge = ridge, ...)  ## bug fix 10apr2013
    yhatobs <- x[ry, ] %*% parm$coef
    yhatmis <- x[!ry, ] %*% parm$beta
    idx <- matcher(yhatobs, yhatmis, k = donors)
    return(y[ry][idx])
}

