# --------------------------------QUICKPRED------------------------------------

#'Quick selection of predictors from the data
#'
#'Selects predictors according to simple statistics
#'
#'This function creates a predictor matrix using the variable selection
#'procedure described in Van Buuren et al.~(1999, p.~687--688). The function is
#'designed to aid in setting up a good imputation model for data with many
#'variables.
#'
#'Basic workings: The procedure calculates for each variable pair (i.e.
#'target-predictor pair) two correlations using all available cases per pair.
#'The first correlation uses the values of the target and the predictor
#'directly. The second correlation uses the (binary) response indicator of the
#'target and the values of the predictor. If the largest (in absolute value) of
#'these correlations exceeds \code{mincor}, the predictor will be added to the
#'imputation set.  The default value for \code{mincor} is 0.1.
#'
#'In addition, the procedure eliminates predictors whose proportion of usable
#'cases fails to meet the minimum specified by \code{minpuc}. The default value
#'is 0, so predictors are retained even if they have no usable case.
#'
#'Finally, the procedure includes any predictors named in the \code{include}
#'argument (which is useful for background variables like age and sex) and
#'eliminates any predictor named in the \code{exclude} argument. If a variable
#'is listed in both \code{include} and \code{exclude} arguments, the
#'\code{include} argument takes precedence.
#'
#'Advanced topic: \code{mincor} and \code{minpuc} are typically specified as
#'scalars, but vectors and squares matrices of appropriate size will also work.
#'Each element of the vector corresponds to a row of the predictor matrix, so
#'the procedure can effectively differentiate between different target
#'variables. Setting a high values for can be useful for auxilary, less
#'important, variables. The set of predictor for those variables can remain
#'relatively small. Using a square matrix extends the idea to the columns, so
#'that one can also apply cellwise thresholds.
#'
#'@param data Matrix or data frame with incomplete data.
#'@param mincor A scalar, numeric vector (of size \code{ncol(data))} or numeric
#'matrix (square, of size \code{ncol(data)} specifiying the minimum
#'threshold(s) against which the absolute correlation in the data is compared.
#'@param minpuc A scalar, vector (of size \code{ncol(data))} or matrix (square,
#'of size \code{ncol(data)} specifiying the minimum threshold(s) for the
#'proportion of usable cases.
#'@param include A string or a vector of strings containing one or more
#'variable names from \code{names(data)}. Variables specified are always
#'included as a predictor.
#'@param exclude A string or a vector of strings containing one or more
#'variable names from \code{names(data)}. Variables specified are always
#'excluded as a predictor.
#'@param method A string specifying the type of correlation. Use
#'\code{'pearson'} (default), \code{'kendall'} or \code{'spearman'}. Can be
#'abbreviated.
#'@return A square binary matrix of size \code{ncol(data)}.
#'@author Stef van Buuren, Aug 2009
#'@seealso \code{\link{mice}}, \code{\link[=mids-class]{mids}}
#'@references van Buuren, S., Boshuizen, H.C., Knook, D.L. (1999) Multiple
#'imputation of missing blood pressure covariates in survival analysis.
#'\emph{Statistics in Medicine}, \bold{18}, 681--694.
#'
#'van Buuren, S. and Groothuis-Oudshoorn, K. (2011). \code{mice}: Multivariate
#'Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical
#'Software}, \bold{45}(3), 1-67. \url{http://www.jstatsoft.org/v45/i03/}
#'@keywords misc
#'@examples
#'
#'
#'# default: include all predictors with absolute correlation over 0.1
#'quickpred(nhanes)
#'
#'# all predictors with absolute correlation over 0.4
#'quickpred(nhanes, mincor=0.4)
#'
#'# include age and bmi, exclude chl
#'quickpred(nhanes, mincor=0.4, inc=c('age','bmi'), exc='chl')
#'
#'# only include predictors with at least 30% usable cases
#'quickpred(nhanes, minpuc=0.3)
#'
#'# use low threshold for bmi, and high thresholds for hyp and chl
#'pred <- quickpred(nhanes, mincor=c(0,0.1,0.5,0.5))
#'pred
#'
#'# use it directly from mice
#'imp <- mice(nhanes, pred=quickpred(nhanes, minpuc=0.25, include='age'))
#'
#'@export
quickpred <- function(data, mincor = 0.1, minpuc = 0, include = "", exclude = "", method = "pearson") {
    # automatic predictor selection according to Van Buuren et al (1999)
    
    # argument checking
    if (!(is.matrix(data) || is.data.frame(data))) 
        stop("Data should be a matrix or data frame")
    if ((nvar <- ncol(data)) < 2) 
        stop("Data should contain at least two columns")
    
    # initialize
    predictorMatrix <- matrix(0, nrow = nvar, ncol = nvar, dimnames = list(names(data), names(data)))
    x <- data.matrix(data)
    r <- !is.na(x)
    
    # include predictors with 1) pairwise correlation among data 2) pairwise correlation of data with response indicator
    # higher than mincor
    suppressWarnings(v <- abs(cor(x, use = "pairwise.complete.obs", method = method)))
    v[is.na(v)] <- 0
    suppressWarnings(u <- abs(cor(y = x, x = r, use = "pairwise.complete.obs", method = method)))
    u[is.na(u)] <- 0
    maxc <- pmax(v, u)
    predictorMatrix[maxc > mincor] <- 1
    
    # exclude predictors with a percentage usable cases below minpuc
    p <- md.pairs(data)
    puc <- p$mr/(p$mr + p$mm)
    predictorMatrix[puc < minpuc] <- 0
    
    # exclude predictors listed in the exclude argument
    yz <- pmatch(exclude, names(data))
    predictorMatrix[, yz] <- 0
    
    # include predictors listed in the include argument
    yz <- pmatch(include, names(data))
    predictorMatrix[, yz] <- 1
    
    # some final processing
    diag(predictorMatrix) <- 0
    predictorMatrix[colSums(!r) == 0, ] <- 0
    
    return(predictorMatrix)
}
