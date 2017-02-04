# --------------------MICE.IMPUTE.POLYREG-----------------------------

#'Imputation by polytomous regression - ordered
#'
#'Imputes missing data in a categorical variable using polytomous regression
#'
#'By default, ordered factors with more than two levels are imputed by
#'\code{mice.impute.polr}.
#'
#'The function \code{mice.impute.polr()} imputes for ordered categorical response
#'variables by the proportional odds logistic regression (polr) model. The
#'function repeatedly applies logistic regression on the successive splits. The
#'model is also known as the cumulative link model.
#'
#'The algorithm of \code{mice.impute.polr} uses the function \code{polr()} from
#'the \code{MASS} package.
#'
#'In order to avoid bias due to perfect prediction, the algorithm augment the
#'data according to the method of White, Daniel and Royston (2010).
#'
#'The call to \code{polr} might fail, usually because the data are very sparse.
#'In that case, \code{multinom} is tried as a fallback, and a record is written
#'to the \code{loggedEvents} component of the \code{mids} object.
#'
#'@aliases mice.impute.polr
#'@param y Incomplete data vector of length \code{n}
#'@param ry Vector of missing data pattern (\code{FALSE}=missing,
#'\code{TRUE}=observed)
#'@param x Matrix (\code{n} x \code{p}) of complete covariates.
#'@param nnet.maxit Tuning parameter for \code{nnet()}.
#'@param nnet.trace Tuning parameter for \code{nnet()}.
#'@param nnet.MaxNWts Tuning parameter for \code{nnet()}.
#'@param ... Other named arguments.
#'@return A vector of length \code{nmis} with imputations.
#'@author Stef van Buuren, Karin Groohuis-Oudshoorn, 2000-2010
#'@seealso \code{\link{mice}}, \code{\link[nnet]{multinom}},
#'\code{\link[MASS]{polr}}
#'@references
#'
#'Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}: Multivariate
#'Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical
#'Software}, \bold{45}(3), 1-67. \url{http://www.jstatsoft.org/v45/i03/}
#'
#'Brand, J.P.L. (1999) \emph{Development, implementation and evaluation of
#'multiple imputation strategies for the statistical analysis of incomplete
#'data sets.} Dissertation. Rotterdam: Erasmus University.
#'
#'White, I.R., Daniel, R. Royston, P. (2010). Avoiding bias due to perfect
#'prediction in multiple imputation of incomplete categorical variables.
#'\emph{Computational Statistics and Data Analysis}, 54, 2267-2275.
#'
#'Venables, W.N. & Ripley, B.D. (2002). \emph{Modern applied statistics with
#'S-Plus (4th ed)}. Springer, Berlin.
#'@keywords datagen
#'@export
#'
#--------------------MICE.IMPUTE.POLR-----------------------------

mice.impute.polr <- function (y, ry, x, nnet.maxit=100, nnet.trace=FALSE, nnet.MaxNWts=1500, ...)
{
    ### added 08/12/2010
    x <- as.matrix(x)
    aug <- augment(y, ry, x, ...)
    x <- aug$x
    y <- aug$y
    ry <- aug$ry
    w <- aug$w
    xy <- cbind.data.frame(y = y, x = x)
    
    ## polr may fail on sparse data. We revert to multinom in such cases. 
    fit <- try(suppressWarnings(polr(formula(xy), data = xy[ry, ], weights=w[ry],...)), silent=TRUE)
    if (inherits(fit, "try-error")) {
        fit <- multinom(formula(xy), data=xy[ry,],
                        weights=w[ry],
                        maxit=nnet.maxit, trace=nnet.trace, MaxNWts=nnet.MaxNWts, ...)
        updateLog(meth="multinom", frame=3)
    }
    post <- predict(fit, xy[!ry, ], type = "probs")
    if (sum(!ry) == 1) 
        post <- matrix(post, nrow = 1, ncol = length(post))
    fy <- as.factor(y)
    nc <- length(levels(fy))
    un <- rep(runif(sum(!ry)), each = nc)
    if (is.vector(post)) 
        post <- matrix(c(1 - post, post), ncol = 2)
    draws <- un > apply(post, 1, cumsum)
    idx <- 1 + apply(draws, 2, sum)
    return(levels(fy)[idx])
}
