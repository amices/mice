#'Imputation by random forests
#'
#'Imputes univariate missing data using random forests.
#'
#'@aliases mice.impute.rf
#'@inheritParams mice.impute.pmm
#'@param ntree The number of trees to grow. The default is 10.
#'@param \dots Other named arguments passed down to \code{randomForest()} and
#'\code{randomForest:::randomForest.default()}.
#'@return Vector with imputed data, same type as \code{y}, and of length 
#'\code{sum(wy)}
#'@details
#'Imputation of \code{y} by random forests. The method 
#'calls \code{randomForrest()} which implements Breiman's random forest 
#'algorithm (based on Breiman and Cutler's original Fortran code) 
#'for classification and regression. See Appendix A.1 of Doove et al. 
#'(2014) for the definition of the algorithm used. 
#'@note An alternative implementation was independently 
#'developed by Shah et al (2014). This were available as 
#'functions \code{CALIBERrfimpute::mice.impute.rfcat} and 
#'\code{CALIBERrfimpute::mice.impute.rfcont} (now archived).
#'Simulations by Shah (Feb 13, 2014) suggested that 
#'the quality of the imputation for 10 and 100 trees was identical, 
#'so mice 2.22 changed the default number of trees from \code{ntree = 100} to 
#'\code{ntree = 10}.
#'@author Lisa Doove, Stef van Buuren, Elise Dusseldorp, 2012
#'@references 
#'
#' Doove, L.L., van Buuren, S., Dusseldorp, E. (2014), Recursive partitioning 
#' for missing data imputation in the presence of interaction Effects. 
#' Computational Statistics \& Data Analysis, 72, 92-104.
#' 
#' Shah, A.D., Bartlett, J.W., Carpenter, J., Nicholas, O., Hemingway, H. (2014),
#' Comparison of random forest and parametric imputation models for 
#' imputing missing data using MICE: A CALIBER study. American Journal 
#' of Epidemiology, doi: 10.1093/aje/kwt312. 
#'
#'Van Buuren, S. (2018). 
#'\href{https://stefvanbuuren.name/fimd/sec-cart.html}{\emph{Flexible Imputation of Missing Data. Second Edition.}}
#'Chapman & Hall/CRC. Boca Raton, FL.
#'@seealso \code{\link{mice}}, \code{\link{mice.impute.cart}}, 
#'\code{\link[randomForest]{randomForest}}
#'@family univariate imputation functions
#'@keywords datagen
#'@examples
#'library("lattice")
#'
#'imp <- mice(nhanes2, meth = "rf", ntree = 3)
#'plot(imp)
#'
#'@export
mice.impute.rf <- function(y, ry, x, wy = NULL, ntree = 10, ...)
{
  if (is.null(wy)) wy <- !ry
  if (!requireNamespace("randomForest", quietly = TRUE))
    stop("Package 'randomForest' needed fo this function 
             to work. Please install it.", 
         call. = FALSE)
  onetree <- function(xobs, xmis, yobs, ...)
  {
    fit <- randomForest::randomForest(x = xobs, 
                                      y = yobs, 
                                      ntree = 1, ...)
    leafnr <- predict(object = fit, newdata = xobs, nodes = TRUE)
    nodes <- predict(object = fit, newdata = xmis, nodes = TRUE)
    donor <- lapply(nodes, function(s) yobs[leafnr == s])
    return(donor)
  }
  ntree <- max(1, ntree)  # safety
  nmis <- sum(wy)
  xobs <- x[ry, , drop = FALSE]
  xmis <- x[wy, , drop = FALSE]
  yobs <- y[ry]
  
  forest <- sapply(seq_len(ntree), FUN = function(s) onetree(xobs, xmis, yobs, ...))
  if (nmis == 1) forest <- array(forest, dim = c(1, ntree))
  impute <- apply(forest, MARGIN = 1, FUN = function(s) sample(unlist(s), 1))
  return(impute)
}
