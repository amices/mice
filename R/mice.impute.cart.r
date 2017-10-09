#'Imputation by classification and regression trees
#'
#'Imputes univariate missing data using classification and regression trees.
#'
#'@aliases mice.impute.cart cart
#'
#'@inheritParams mice.impute.pmm
#'@return Vector with imputed data, same type as \code{y}, and of length 
#'\code{sum(wy)}
#'@param minbucket The minimum number of observations in any terminal node used.  
#'See \code{\link{rpart.control}} for details.
#'@param cp Complexity parameter. Any split that does not decrease the overall
#'lack of fit by a factor of cp is not attempted. See \code{\link{rpart.control}} 
#'for details.
#'@param ... Other named arguments passed down to \code{rpart()}.
#'@return Numeric vector of length \code{sum(!ry)} with imputations
#'@details
#'Imputation of \code{y} by classification and regression trees. The procedure
#'is as follows: 
#'\enumerate{ 
#'\item Fit a classification or regression tree by recursive partitioning;
#'\item For each \code{ymis}, find the terminal node they end up according to the fitted tree;
#'\item Make a random draw among the member in the node, and take the observed value from that 
#'draw as the imputation.
#'}
#'@seealso \code{\link{mice}}, \code{\link{mice.impute.rf}}, 
#'\code{\link[rpart]{rpart}}, \code{\link[rpart]{rpart.control}} 
#'@author Lisa Doove, Stef van Buuren, Elise Dusseldorp, 2012
#'@references 
#'
#' Doove, L.L., van Buuren, S., Dusseldorp, E. (2014), Recursive partitioning 
#' for missing data imputation in the presence of interaction Effects. 
#' Computational Statistics \& Data Analysis, 72, 92-104.
#' 
#'Breiman, L., Friedman, J. H., Olshen, R. A., and Stone, C. J.
#'(1984), Classification and regression trees, Monterey, CA: Wadsworth &
#'Brooks/Cole Advanced Books & Software.
#'
#'Van Buuren, S.(2012), Flexible imputation of missing data, Boca Raton, FL:
#'CRC/Chapman & Hall.
#'
#'@family univariate imputation functions
#'@examples
#'require(rpart)
#'
#'imp <- mice(nhanes2, meth = 'cart', minbucket = 4)
#'plot(imp)
#'
#'@keywords datagen
#'@export
mice.impute.cart <- function(y, ry, x, wy = NULL, minbucket = 5, cp = 1e-04, 
                             ...)
{
  if (is.null(wy)) 
    wy <- !ry
  minbucket <- max(1, minbucket)
  xobs <- x[ry, , drop = FALSE]
  xmis <- x[wy, , drop = FALSE]
  yobs <- y[ry]
  if (!is.factor(yobs))
  {
    fit <- rpart(yobs ~ ., data = cbind(yobs, xobs), method = "anova", 
                 control = rpart.control(minbucket = minbucket, cp = cp, ...))
    leafnr <- floor(as.numeric(row.names(fit$frame[fit$where, ])))
    fit$frame$yval <- as.numeric(row.names(fit$frame))
    nodes <- predict(object = fit, newdata = xmis)
    donor <- lapply(nodes, function(s) yobs[leafnr == s])
    impute <- vapply(seq_along(donor), function(s) sample(donor[[s]], 1), numeric(1))
  } else
  {
    fit <- rpart(yobs ~ ., data = cbind(yobs, xobs), method = "class", 
                 control = rpart.control(minbucket = minbucket, cp = cp, ...))
    nodes <- predict(object = fit, newdata = xmis)
    impute <- apply(nodes, MARGIN = 1, 
                    FUN = function(s) sample(colnames(nodes), 
                                             size = 1, prob = s))
  }
  return(impute)
}
