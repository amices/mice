#'Imputation by random forests
#'
#'Imputes univariate missing data using random forests.
#'
#'Imputation of \code{y} by random forests. The method 
#'calls \code{randomForrest()} which implements Breiman's random forest 
#'algorithm (based on Breiman and Cutler's original Fortran code) 
#'for classification and regression. See Appendix A.1 of Doove et al. 
#'(2014) for the definition 
#'of the algorithm used. An alternative implementation was independently 
#'developed by Shah et al (2014), and is available in the package 
#'\code{CALIBERrfimpute}.
#'
#'@aliases mice.impute.rf
#'@param y Numeric vector with incomplete data
#'@param ry Response pattern of \code{y} (\code{TRUE} = observed,
#'\code{FALSE} = missing)
#'@param x Design matrix with \code{length(y)} rows and \code{p} columns
#'containing complete covariates.
#'@param ntree The number of trees to grow. The default is 100.
#'@param ... Other named arguments passed down to \code{randomForest()} and
#'\code{randomForest:::randomForest.default()}.
#'@return Numeric vector of length \code{sum(!ry)} with imputations
#'@author Lisa Doove, Stef van Buuren, Elise Dusseldorp, 2012
#'@importFrom randomForest randomForest
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
#' Van Buuren, S.(2012), Flexible imputation of missing data, Boca Raton, FL:
#' Chapman & Hall/CRC.
#'
#'@seealso \code{\link{mice}}, \code{\link{mice.impute.cart}}, 
#'\code{\link[randomForest]{randomForest}},
#'\code{\link[CALIBERrfimpute]{mice.impute.rfcat}}, 
#'\code{\link[CALIBERrfimpute]{mice.impute.rfcont}}
#'@examples
#'library("lattice")
#'
#'imp <- mice(nhanes2, meth = "rf", ntree = 3)
#'plot(imp)
#'
#'@keywords datagen
#'@export
mice.impute.rf <- function(y, ry, x, ntree = 100, ...)
{
    ntree <- max(1, ntree)  # safety
    xobs <- x[ry, ]
    xmis <- x[!ry, ]
    yobs <- y[ry]
    onetree <- function(xobs, xmis, yobs, ...)
    {
        fit <- randomForest(x = xobs, 
                            y = yobs, 
                            ntree = 1, ...)
        leafnr <- predict(object = fit, newdata = xobs, nodes = TRUE)
        nodes <- predict(object = fit, newdata = xmis, nodes = TRUE)
        donor <- lapply(nodes, function(s) yobs[leafnr == s])
        return(donor)
    }
    forest <- sapply(1:ntree, FUN = function(s) onetree(xobs, xmis, yobs, ...))
    impute <- apply(forest, MARGIN = 1, FUN = function(s) sample(unlist(s), 1))
    return(impute)
}
