### -----------------------------MICE.IMPUTE.FASTPMM-------------------------

#'Imputation by fast predictive mean matching
#'
#'Imputes univariate missing data using fast predictive mean matching
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
#'@note \code{mice.impute.fastpmm()} was introduced on \code{mice 2.19} and 
#'after a faster alternative to \code{mice.impute.pmm()}.
#'This is an experimental feature.
#'
#'@aliases mice.impute.fastpmm fastpmm
#'@param y Numeric vector with incomplete data
#'@param ry Response pattern of \code{y} (\code{TRUE}=observed,
#'\code{FALSE}=missing)
#'@param x Design matrix with \code{length(y)} rows and \code{p} columns
#'containing complete covariates.
#'@param donors Size of the set of potential donors from which a random draw is made. The default is \code{donors = 5}.
#'@param type Type of matching distance. The default choice \code{type = 1} calculates the distance between the predicted value of \code{yobs} and the drawn values of \code{ymis}. Other choices are \code{type = 0} (distance between predicted values) and \code{type = 2} (distance between drawn values).
#'@param ridge The ridge penalty applied in \code{.norm.draw()} to prevent problems with multicollinearity. The default is \code{ridge = 1e-05}, which means that 0.01 percent of the diagonal is added to the cross-product. Larger ridges may result in more biased estimates. For highly noisy data (e.g. many junk variables), set \code{ridge = 1e06} or lower to reduce bias. 
#'@param ... Other named arguments.
#'@return Numeric vector of length \code{sum(!ry)} with imputations
#'@author Stef van Buuren, Karin Groothuis-Oudshoorn, 2000, 2012
#'@references Little, R.J.A. (1988), Missing data adjustments in large surveys
#'(with discussion), Journal of Business Economics and Statistics, 6, 287--301.
#'
#'Rubin, D.B. (1987). Multiple imputation for nonresponse in surveys. New York:
#'Wiley.
#'
#'@keywords datagen
#'@export
mice.impute.fastpmm <- function(y, ry, x, donors = 5, type = 1, ridge = 1e-05, ...) 
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
    # Author: S. van Buuren
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

