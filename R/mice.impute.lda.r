# --------------------MICE.IMPUTE.LDA-----------------------------

#'Imputation by linear discriminant analysis
#'
#'Imputes univariate missing data using linear discriminant analysis
#'
#'Imputation of categorical response variables by linear discriminant analysis.
#'This function uses the Venables/Ripley functions \code{lda()} and
#'\code{predict.lda()} to compute posterior probabilities for each incomplete
#'case, and draws the imputations from this posterior.
#'
#'@param y Incomplete data vector of length \code{n}
#'@param ry Vector of missing data pattern (\code{FALSE}=missing,
#'\code{TRUE}=observed)
#'@param x Matrix (\code{n} x \code{p}) of complete covariates.
#'@param ... Other named arguments.
#'@return A vector of length \code{nmis} with imputations.
#'@note This function can be called from within the Gibbs sampler by specifying
#''lda' in the \code{method} argument of \code{mice()}.  This method is usually
#'faster and uses fewer resources than calling the function
#'\code{\link{mice.impute.polyreg}}.
#'@section Warning: The function does not incorporate the variability of the
#'discriminant weight, so it is not 'proper' in the sense of Rubin. For small
#'samples and rare categories in the \code{y}, variability of the imputed data
#'could therefore be somewhat underestimated.
#'@author Stef van Buuren, Karin Groothuis-Oudshoorn, 2000
#'@seealso \code{\link{mice}}, \code{link{mice.impute.polyreg}},
#'\code{\link[MASS]{lda}}
#'@references Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}:
#'Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#'Statistical Software}, \bold{45}(3), 1-67.
#'\url{http://www.jstatsoft.org/v45/i03/}
#'
#'Brand, J.P.L. (1999). Development, Implementation and Evaluation of Multiple
#'Imputation Strategies for the Statistical Analysis of Incomplete Data Sets.
#'Ph.D. Thesis, TNO Prevention and Health/Erasmus University Rotterdam. ISBN
#'90-74479-08-1.
#'
#'Venables, W.N. & Ripley, B.D. (1997). Modern applied statistics with S-PLUS
#'(2nd ed). Springer, Berlin.
#'@keywords datagen
#'@export
mice.impute.lda <- function(y, ry, x, ...) {
    # mice.impute.lda
    #
    # Imputation of categorical response variables by linear discriminant
    # analysis. This function uses the Venables/Ripley functions
    # lda and predict.lda to compute posterior probabilities for
    # each incomplete case, and draws the imputations from this
    # posterior.
    #
    # Authors: Stef van Buuren, Karin Groothuis-Oudshoorn, 11 okt 1999
    # V2: Adapted SvB June 2009 to include bootstrap - disabled since
    # bootstrapping it may easily to constant variables within groups.
    # which will be difficult to detect when bootstrapped
    
    fy <- as.factor(y)
    nc <- length(levels(fy))
    
    #   SvB June 2009 - take bootstrap sample of training data
    #   idx <- sample((1:length(y))[ry], size=sum(ry), replace=TRUE)
    #   x[ry,] <- x[idx,]
    #   y[ry] <- y[idx]
    #   end bootstrap
    
    fit <- lda(x, fy, subset = ry)
    post <- predict(fit, x[!ry, , drop = FALSE])$posterior
    un <- rep(runif(sum(!ry)), each = nc)
    idx <- 1 + apply(un > apply(post, 1, cumsum), 2, sum)
    return(levels(fy)[idx])
}
