#'Imputation by classification and regression trees
#'
#'Imputes univariate missing data using classification and regression trees.
#'
#'Imputation of \code{y} by classification and regression trees. The procedure
#'is as follows: 
#'\enumerate{ 
#'\item Fit a classification or regression tree by recursive partitioning;
#'\item For each \code{ymis}, find the terminal node they end up according to the fitted tree;
#'\item Make a random draw among the member in the node, and take the observed value from that 
#'draw as the imputation.
#'}
#'
#'@aliases mice.impute.cart cart
#'@param y Numeric vector with incomplete data
#'@param ry Response pattern of \code{y} (\code{TRUE} = observed,
#'\code{FALSE} = missing)
#'@param x Design matrix with \code{length(y)} rows and \code{p} columns
#'containing complete covariates.
#'@param minbucket The minimum number of observations in any terminal node used.  
#'See \code{\link{rpart.control}} for details.
#'@param cp Complexity parameter. Any split that does not decrease the overall
#'lack of fit by a factor of cp is not attempted. See \code{\link{rpart.control}} 
#'for details.
#'@param ... Other named arguments passed down to \code{rpart()}.
#'@return Numeric vector of length \code{sum(!ry)} with imputations
#'@author Lisa Doove, Stef van Buuren, Elise Dusseldorp, 2012
#'@references 
#'
#' Doove, L.L., van Buuren, S., Dusseldorp, E. (2013). Recursive partitioning 
#' for missing data imputation in the presence of interaction Effects. Submitted
#' for publication.
#' 
#'Breiman, L., Friedman, J. H., Olshen, R. A., and Stone, C. J.
#'(1984), Classification and regression trees, Monterey, CA: Wadsworth &
#'Brooks/Cole Advanced Books & Software.
#'
#'Van Buuren, S.(2012), Flexible imputation of missing data, Boca Raton, FL:
#'Chapman & Hall/CRC.
#'
#'@examples
#'
#'imp <- mice(nhanes2, meth = "cart", minbucket = 4)
#'plot(imp)
#'
#'@keywords datagen
#'@export
mice.impute.cart <- function(y, ry, x, minbucket = 5, cp = 1e-04, ...) {
    minbucket <- max(1, minbucket)  # safety
    xobs <- x[ry, ]
    xmis <- x[!ry, ]
    yobs <- y[ry]
    if (!is.factor(yobs)) {
        fit <- rpart(yobs ~ ., data = cbind(yobs, xobs), method = "anova", 
                     control = rpart.control(minbucket = minbucket, cp = cp), ...)
        leafnr <- floor(as.numeric(row.names(fit$frame[fit$where,])))
        fit$frame$yval <- as.numeric(row.names(fit$frame))
        nodes <- predict(object = fit, newdata = xmis)
        donor <- lapply(nodes, function(s) yobs[leafnr == s])
        impute <- sapply(1:length(donor), function(s) sample(donor[[s]], 1))
    } else {
        fit <- rpart(yobs ~ ., data = cbind(yobs, xobs), method = "class", 
                     control = rpart.control(minbucket = minbucket, cp = cp), ...)
        nodes <- predict(object = fit, newdata = xmis)
        impute <- apply(nodes, MARGIN=1, FUN=function(s) sample(colnames(nodes),size=1, prob=s))
    }
    return(impute)
}
