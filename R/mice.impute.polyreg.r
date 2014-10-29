# --------------------MICE.IMPUTE.POLYREG-----------------------------

#'Imputation by polytomous regression - unordered
#'
#'Imputes missing data in a categorical variable using polytomous regression
#'
#'By default, unordered factors with more than two levels are imputed by
#'\code{mice.impute.polyreg()}. 'The function \code{mice.impute.polyreg()} 
#'imputes categorical response 
#'variables by the Bayesian polytomous regression model. See J.P.L. Brand
#'(1999), Chapter 4, Appendix B.
#'
#'The method consists of the following steps: \enumerate{ \item Fit categorical
#'response as a multinomial model \item Compute predicted categories \item Add
#'appropriate noise to predictions.  }
#'
#'The algorithm of \code{mice.impute.polyreg} uses the function
#'\code{multinom()} from the \code{nnet} package.
#'
#'In order to avoid bias due to perfect prediction, the algorithm augment the
#'data according to the method of White, Daniel and Royston (2010).
#'
#'@aliases mice.impute.polyreg
#'@param y Incomplete data vector of length \code{n}
#'@param ry Vector of missing data pattern (\code{FALSE}=missing,
#'\code{TRUE}=observed)
#'@param x Matrix (\code{n} x \code{p}) of complete covariates.
#'@param nnet.maxit Tuning parameter for \code{nnet()}.
#'@param nnet.trace Tuning parameter for \code{nnet()}.
#'@param nnet.maxNWts Tuning parameter for \code{nnet()}.
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
mice.impute.polyreg <- function(y, ry, x, nnet.maxit = 100, nnet.trace = FALSE, nnet.maxNWts = 1500, ...) {
    # mice.impute.polyreg
    #
    # Imputation for categorical response variables by the Bayesian
    # polytomous regression model. See J.P.L. Brand (1999), Chapter 4,
    # Appendix B.
    #
    # The method consists of the following steps:
    # 1. Fit categorical response as a multinomial model
    # 2. Compute predicted categories
    # 3. Add appropriate noise to predictions.
    #
    # This algorithm uses the function multinom from the libraries nnet and MASS
    # (Venables and Ripley).
    #
    # Authors: Stef van Buuren, Karin Groothuis-Oudshoorn, 2000
    #
    # SvB May 2009   # augmented data added for stability
    # SvB Dec 2010   # assign(data) hack removed
    x <- as.matrix(x)
    aug <- augment(y, ry, x, ...)
    x <- aug$x
    y <- aug$y
    ry <- aug$ry
    w <- aug$w
    
    fy <- as.factor(y)
    nc <- length(levels(fy))
    un <- rep(runif(sum(!ry)), each = nc)
    
    #
    xy <- cbind.data.frame(y = y, x = x)  # fixed SvB 6/12/2010
    # 28/10/2014 fix, if there are no predictors append intercept
    if (ncol(x) == 0L) xy <- data.frame(xy, int = 1)
    fit <- multinom(formula(xy), data = xy[ry,,drop = FALSE ], 
                    weights = w[ry], maxit = nnet.maxit, trace = nnet.trace, maxNWts = nnet.maxNWts, ...)
    post <- predict(fit, xy[!ry, ], type = "probs")
    if (sum(!ry) == 1) post <- matrix(post, nrow = 1, ncol = length(post))  # SvB 14 sept 2009
    
    if (is.vector(post)) 
        post <- matrix(c(1 - post, post), ncol = 2)
    draws <- un > apply(post, 1, cumsum)
    idx <- 1 + apply(draws, 2, sum)
    
    return(levels(fy)[idx])
}
