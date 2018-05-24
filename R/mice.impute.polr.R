#'Imputation of ordered data by polytomous regression
#' 
#'Imputes missing data in a categorical variable using polytomous regression
#'@aliases mice.impute.polr
#'@inheritParams mice.impute.pmm
#'@param nnet.maxit Tuning parameter for \code{nnet()}.
#'@param nnet.trace Tuning parameter for \code{nnet()}.
#'@param nnet.MaxNWts Tuning parameter for \code{nnet()}.
#'@return Vector with imputed data, same type as \code{y}, and of length 
#'\code{sum(wy)}
#'@details
#'The function \code{mice.impute.polr()} imputes for ordered categorical response
#'variables by the proportional odds logistic regression (polr) model. The
#'function repeatedly applies logistic regression on the successive splits. The
#'model is also known as the cumulative link model.
#'
#'By default, ordered factors with more than two levels are imputed by
#'\code{mice.impute.polr}.
#'
#'The algorithm of \code{mice.impute.polr} uses the function \code{polr()} from
#'the \code{MASS} package.
#'
#'In order to avoid bias due to perfect prediction, the algorithm augment the
#'data according to the method of White, Daniel and Royston (2010).
#'
#'The call to \code{polr} might fail, usually because the data are very sparse.
#'In that case, \code{multinom} is tried as a fallback, and a record is written
#'to the \code{loggedEvents} component of the \code{\link{mids}} object.
#'
#'@author Stef van Buuren, Karin Groothuis-Oudshoorn, 2000-2010
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
mice.impute.polr <- function(y, ry, x, wy = NULL, nnet.maxit = 100, 
                             nnet.trace = FALSE, nnet.MaxNWts = 1500, ...)
{
  if (is.null(wy)) wy <- !ry
  
  # augment data to evade issues with perfect prediction
  x <- as.matrix(x)
  aug <- augment(y, ry, x, wy)
  x <- aug$x
  y <- aug$y
  ry <- aug$ry
  wy <- aug$wy
  w <- aug$w
  xy <- cbind.data.frame(y = y, x = x)
  
  ## polr may fail on sparse data. We revert to multinom in such cases.
  fit <- try(suppressWarnings(polr(formula(xy), 
                                   data = xy[ry, , drop = FALSE], 
                                   weights = w[ry], ...)), silent = TRUE)
  if (inherits(fit, "try-error"))
  {
    fit <- multinom(formula(xy), data = xy[ry, , drop = FALSE], 
                    weights = w[ry], 
                    maxit = nnet.maxit, trace = nnet.trace, 
                    MaxNWts = nnet.MaxNWts, ...)
  }
  post <- predict(fit, xy[wy, , drop = FALSE], type = "probs")
  if (sum(wy) == 1) 
    post <- matrix(post, nrow = 1, ncol = length(post))
  fy <- as.factor(y)
  nc <- length(levels(fy))
  un <- rep(runif(sum(wy)), each = nc)
  if (is.vector(post))
    post <- matrix(c(1 - post, post), ncol = 2)
  draws <- un > apply(post, 1, cumsum)
  idx <- 1 + apply(draws, 2, sum)
  return(levels(fy)[idx])
}
