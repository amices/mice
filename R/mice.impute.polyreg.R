#'Imputation of unordered data by polytomous regression
#' 
#'Imputes missing data in a categorical variable using polytomous regression
#'
#'@aliases mice.impute.polyreg
#'@inheritParams mice.impute.pmm
#'@param nnet.maxit Tuning parameter for \code{nnet()}.
#'@param nnet.trace Tuning parameter for \code{nnet()}.
#'@param nnet.MaxNWts Tuning parameter for \code{nnet()}.
#'@return Vector with imputed data, same type as \code{y}, and of length 
#'\code{sum(wy)}
#'@author Stef van Buuren, Karin Groothuis-Oudshoorn, 2000-2010
#'@details
#'The function \code{mice.impute.polyreg()} imputes categorical response 
#'variables by the Bayesian polytomous regression model. See J.P.L. Brand
#'(1999), Chapter 4, Appendix B.
#'
#'By default, unordered factors with more than two levels are imputed by
#'\code{mice.impute.polyreg()}.
#'
#'The method consists of the following steps: 
#'\enumerate{ 
#'\item Fit categorical response as a multinomial model 
#'\item Compute predicted categories 
#'\item Add appropriate noise to predictions
#'}
#'
#'The algorithm of \code{mice.impute.polyreg} uses the function
#'\code{multinom()} from the \code{nnet} package.
#'
#'In order to avoid bias due to perfect prediction, the algorithm augment the
#'data according to the method of White, Daniel and Royston (2010).
#'@seealso \code{\link{mice}}, \code{\link[nnet]{multinom}},
#'\code{\link[MASS]{polr}}
#'@references
#'
#'Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}: Multivariate
#'Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical
#'Software}, \bold{45}(3), 1-67. \url{https://www.jstatsoft.org/v45/i03/}
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
#'@family univariate imputation functions
#'@keywords datagen
#'@export
mice.impute.polyreg <- function(y, ry, x, wy = NULL, nnet.maxit = 100, 
                                nnet.trace = FALSE, nnet.MaxNWts = 1500, ...)
{
  install.on.demand("nnet", ...)
  if (is.null(wy)) 
    wy <- !ry
  
  # augment data to evade issues with perfect prediction
  x <- as.matrix(x)
  aug <- augment(y, ry, x, wy)
  x <- aug$x
  y <- aug$y
  ry <- aug$ry
  wy <- aug$wy
  w <- aug$w
  
  fy <- as.factor(y)
  nc <- length(levels(fy))
  un <- rep(runif(sum(wy)), each = nc)
  
  xy <- cbind.data.frame(y = y, x = x)
  
  if (ncol(x) == 0L) 
    xy <- data.frame(xy, int = 1)
  
  # escape with same impute if the dependent does not vary
  cat.has.all.obs <- table(y[ry]) == sum(ry)
  if (any(cat.has.all.obs)) return(rep(levels(fy)[cat.has.all.obs], sum(wy)))
  
  fit <- nnet::multinom(formula(xy), data = xy[ry, , drop = FALSE], weights = w[ry], 
                        maxit = nnet.maxit, trace = nnet.trace, MaxNWts = nnet.MaxNWts, 
                        ...)
  post <- predict(fit, xy[wy, , drop = FALSE], type = "probs")
  if (sum(wy) == 1) 
    post <- matrix(post, nrow = 1, ncol = length(post))
  if (is.vector(post))
    post <- matrix(c(1 - post, post), ncol = 2)
  draws <- un > apply(post, 1, cumsum)
  idx <- 1 + apply(draws, 2, sum)
  
  return(levels(fy)[idx])
}
