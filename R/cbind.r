# ------------------------------cbind.mids--------------------------------

#'Columnwise combination of a \code{mids} object.
#'
#'This function combines two \code{mids} objects columnwise into a single
#'object of class \code{mids}, or combines a \code{mids} object with a \code{vector},
#'\code{matrix}, \code{factor} or \code{data.frame} columnwise into an object of class \code{mids}.
#'The number of rows in the (incomplete) data \code{x$data} and \code{y} (or
#'\code{y$data} if \code{y} is a \code{mids} object) should be equal. If
#'\code{y} is a \code{mids} object then the number of imputations in \code{x}
#'and \code{y} should be equal. Note: If \code{y} is a vector or factor its
#'original name is lost and it will be denoted with \code{y} in the \code{mids}
#'object.
#'
#'@param x A \code{mids} object.
#'@param y A \code{mids} object or a \code{data.frame}, \code{matrix}, 
#'\code{factor} or \code{vector}.
#'@param \dots Additional \code{data.frame}, \code{matrix}, \code{vector} or \code{factor}. 
#'These can be given as named arguments.
#'@return An S3 object of class \code{mids}
#'@note 
#'Component \code{call} is a vector, with first argument the \code{mice()} statement
#'that created \code{x} and second argument the call to \code{cbind.mids()}.
#'Component \code{data} is the code{cbind} of the (incomplete) data in \code{x$data}
#'and \code{y$data}. Component \code{m} is the number of imputations. 
#'Component \code{nmis} is an array containing the number of missing observations per
#'column.
#'Component \code{imp} is a list of \code{nvar} components with the generated multiple
#'imputations.  Each part of the list is a \code{nmis[j]} by \code{m} matrix of
#'imputed values for variable \code{j}. The original data of \code{y} will be
#'copied into this list, including the missing values of \code{y} then \code{y}
#'is not imputed.
#'Component \code{method} is a vector of strings of \code{length(nvar)} specifying the
#'elementary imputation method per column. If \code{y} is a \code{mids} object this
#'vector is a combination of \code{x$method} and \code{y$method}, otherwise
#'this vector is \code{x$method} and for the columns of \code{y} the method is
#'set to \code{''}. 
#'Component \code{predictorMatrix} is a square matrix of size \code{ncol(data)}
#'containing integer data specifying the predictor set. If \code{x} and
#'\code{y} are \code{mids} objects then the predictor matrices of \code{x} and
#'\code{y} are combined with zero matrices on the off-diagonal blocks.
#'Otherwise the variables in \code{y} are included in the predictor matrix of
#'\code{x} such that \code{y} is not used as predictor(s) and not imputed as
#'well.
#'Component \code{visitSequence} is the sequence in which columns are visited. The same
#'as \code{x$visitSequence}.
#'Component \code{seed} is the seed value of the solution, \code{x$seed}.
#'Component \code{iteration} is the last Gibbs sampling iteration number,
#'\code{x$iteration}.
#'Component \code{lastSeedValue} is the most recent seed value, \code{x$lastSeedValue}
#'Component \code{chainMean} is the combination of \code{x$chainMean} and
#'\code{y$chainMean}. If \code{y$chainMean} does not exist this element equals
#'\code{x$chainMean}.
#'Component \code{chainVar} is the combination of \code{x$chainVar} and \code{y$chainVar}.
#'If \code{y$chainVar} does not exist this element equals \code{x$chainVar}.
#'Component \code{pad} is a list containing various settings of the padded imputation
#'model, i.e. the imputation model after creating dummy variables.  This list
#'is defined by combining \code{x$pad} and \code{y$pad} if \code{y} is a
#'\code{mids} object. Otherwise, it is defined by the settings of \code{x} and
#'the combination of the data \code{x$data} and \code{y}.
#'Component \code{loggedEvents} is set to \code{x$loggedEvents}.
#'If a column of \code{y} is categorical this is ignored in the
#'padded model since that column is not used as predictor for another column. 
#'@author Karin Groothuis-Oudshoorn, Stef van Buuren, 2009
#'@seealso \code{\link{rbind.mids}}, \code{\link{ibind}}, \code{\link[=mids-class]{mids}}
#'@keywords manip
#'@examples
#'
#'# append 'forgotten' variable bmi to imp
#'temp <- boys[,c(1:3,5:9)]
#'imp  <- mice(temp,maxit=1,m=2)
#'imp2 <- cbind.mids(imp, data.frame(bmi=boys$bmi))
#'
#'# append maturation score to imp (numerical)
#'mat  <- (as.integer(temp$gen) + as.integer(temp$phb)
#' + as.integer(cut(temp$tv,breaks=c(0,3,6,10,15,20,25))))
#'imp2 <- cbind.mids(imp, as.data.frame(mat))
#'
#'# append maturation score to imp (factor)
#'# known issue: new column name is 'y', not 'mat'
#'mat  <- as.factor(mat)
#'imp2 <- cbind.mids(imp, mat)
#'
#'# append data frame with two columns to imp
#'temp2 <- data.frame(bmi=boys$bmi,mat=as.factor(mat))
#'imp2  <- cbind.mids(imp, temp2)
#'
#'# combine two mids objects
#'impa <- mice(temp, maxit=1, m=2)
#'impb <- mice(temp2, maxit=2, m=2)
#'
#'# first a then b
#'impab <- cbind.mids(impa, impb)
#'
#'# first b then a
#'impba <- cbind.mids(impb, impa)
#'
#'@export
cbind.mids <- function(x, y, ...) {
    # This function combines x and y columnwise into a single mids object.
    # x must be a midsobject
    # y can be a vector, matrix, factor, dataframe or also a mids object.
    # It is allowed to combine more than two objects when y is not a mids object.
    # KO 08/09.
    
    call <- match.call()
    if (!is.mids(y)) 
        y <- cbind.data.frame(y, ...)
    
    # The data in y is converted to a dataframe.
    if (is.matrix(y)) 
        y <- as.data.frame(y)
    if (is.vector(y)) 
        y <- as.data.frame(y)
    if (is.factor(y)) 
        y <- as.data.frame(y)
    
    if (is.data.frame(y)) {
        if (nrow(y) != nrow(x$data)) 
            stop("The two datasets do not have the same length\n")
        
        varnames <- c(dimnames(x$data)[[2]], dimnames(y)[[2]])
        # Call is a vector, with first argument the mice statement and second argument the call to cbind.mids.
        call <- c(x$call, call)
        
        # The data in x (x$data) and y are combined together.
        data <- cbind(x$data, y)
        
        # The number of imputations in the new midsobject is equal to that in x.
        m <- x$m
        
        # count the number of missing data in y
        nmis <- c(x$nmis, colSums(is.na(y)))
        
        # The original data of y will be copied into the multiple imputed dataset, including the missing values of y.
        r <- (!is.na(y))
        imp <- vector("list", ncol(y))
        for (j in 1:ncol(y)) {
            imp[[j]] <- as.data.frame(matrix(NA, nrow = sum(!r[, j]), ncol = x$m))
            dimnames(imp[[j]]) <- list(row.names(y)[r[, j] == FALSE], 1:m)
        }
        imp <- c(x$imp, imp)
        names(imp) <- varnames
        
        # The imputation method for (columns in) y will be set to ''.
        method <- c(x$method, rep("", ncol(y)))
        names(method) <- c(names(x$method), colnames(y))
        
        # The variable(s) in y are included in the predictorMatrix.  y is not used as predictor as well as not imputed.
        predictorMatrix <- rbind(x$predictorMatrix, matrix(0, ncol = ncol(x$predictorMatrix), nrow = ncol(y)))
        predictorMatrix <- cbind(predictorMatrix, matrix(0, ncol = ncol(y), nrow = nrow(x$predictorMatrix) + ncol(y)))
        dimnames(predictorMatrix) <- list(varnames, varnames)
        
        # The visitSequence is taken as in x$visitSequence.
        visitSequence <- x$visitSequence
        
        # The post vector for (columns in) y will be set to ''.
        post <- c(x$post, rep("", ncol(y)))
        names(post) <- c(names(x$post), colnames(y))
        
        # seed, lastSeedvalue, number of iterations, chainMean and chainVar is taken as in mids object x.
        seed <- x$seed
        lastSeedvalue <- x$lastSeedvalue
        iteration <- x$iteration
        chainMean = x$chainMean
        chainVar = x$chainVar
        
        # padModel for the data to be binded with x.  Remark, if a column of y is categorical this is ignored in padModel since
        # that column is not used as predictor for another column.
        
        pad <- padModel(data, method, predictorMatrix, visitSequence, post, nmis, nvar = ncol(data))
        
        loggedEvents <- x$loggedEvents
        
        x <- list(call = call, data = data, m = m, nmis = nmis, imp = imp, method = method, predictorMatrix = predictorMatrix, 
                  visitSequence = visitSequence, post = post, seed = seed, iteration = iteration, lastSeedvalue = lastSeedvalue, 
                  chainMean = chainMean, chainVar = chainVar, pad = pad, loggedEvents = loggedEvents)
    }
    
    if (is.mids(y)) {
        
        if (nrow(y$data) != nrow(x$data)) 
            stop("The two datasets do not have the same length\n")
        if (x$m != y$m) 
            stop("The two mids objects should have the same number of imputations\n")
        
        # Call is a vector, with first argument the mice statement and second argument the call to cbind.mids.
        call <- c(x$call, call)
        
        # The data in x$data and y$data are combined together.
        data <- cbind(x$data, y$data)
        varnames <- c(dimnames(x$data)[[2]], dimnames(y$data)[[2]])
        
        m <- x$m
        nmis <- c(x$nmis, y$nmis)
        imp <- c(x$imp, y$imp)
        method <- c(x$method, y$method)
        
        # The predictorMatrices of x and y are combined with zero matrices on the off diagonal blocks.
        predictorMatrix <- rbind(x$predictorMatrix, matrix(0, ncol = ncol(x$predictorMatrix), nrow = nrow(y$predictorMatrix)))
        predictorMatrix <- cbind(predictorMatrix, rbind(matrix(0, ncol = ncol(y$predictorMatrix), nrow = nrow(x$predictorMatrix)), 
                                                        y$predictorMatrix))
        dimnames(predictorMatrix) <- list(varnames, varnames)
        
        # As visitSequence is taken first the order for x and after that from y.
        visitSequence <- c(x$visitSequence, y$visitSequence + max(x$visitSequence))
        
        post <- c(x$post, y$post)
        # For the elements seed, lastSeedvalue and iteration the values from midsobject x are copied.
        seed <- x$seed
        lastSeedvalue <- x$lastSeedvalue
        iteration <- x$iteration
        
        # The padModel is defined by just combining both padModels as defined above.
        padData <- cbind(x$pad$data, y$pad$data)
        varnamesPad <- c(dimnames(x$pad$predictorMatrix)[[1]], dimnames(y$pad$predictorMatrix)[[1]])
        padPredictorMatrix <- rbind(x$pad$predictorMatrix, matrix(0, ncol = ncol(x$pad$predictorMatrix), nrow = nrow(y$pad$predictorMatrix)))
        padPredictorMatrix <- cbind(padPredictorMatrix, rbind(matrix(0, ncol = ncol(y$pad$predictorMatrix), nrow = nrow(x$pad$predictorMatrix)), 
                                                              y$pad$predictorMatrix))
        dimnames(padPredictorMatrix) <- list(varnamesPad, varnamesPad)
        
        padMethod <- c(x$pad$method, y$pad$method)
        padVisitSequence <- c(x$pad$visitSequence, y$pad$visitSequence + max(x$pad$visitSequence))
        padPost <- c(x$pad$post, y$pad$post)
        padCategories <- rbind(x$pad$categories, y$pad$categories)
        pad <- list(data = padData, predictorMatrix = padPredictorMatrix, method = padMethod, visitSequence = padVisitSequence, 
                    post = padPost, categories = padCategories)
        
        # the chainMean and chainVar vectors for x and y are combined.
        chainMean <- array(data = NA, dim = c(dim(x$chainMean)[1] + dim(y$chainMean)[1], iteration, m), dimnames = list(c(dimnames(x$chainMean)[[1]], 
                                                                                                                          dimnames(y$chainMean)[[1]]), dimnames(x$chainMean)[[2]], dimnames(x$chainMean)[[3]]))
        chainMean[1:dim(x$chainMean)[1], , ] <- x$chainMean
        if (iteration <= dim(y$chainMean)[2]) 
            chainMean[(dim(x$chainMean)[1] + 1):dim(chainMean)[1], , ] <- y$chainMean[, 1:iteration, ] else chainMean[(dim(x$chainMean)[1] + 1):dim(chainMean)[1], 1:dim(y$chainMean)[2], ] <- y$chainMean
        
        chainVar <- array(data = NA, dim = c(dim(x$chainVar)[1] + dim(y$chainVar)[1], iteration, m), dimnames = list(c(dimnames(x$chainVar)[[1]], 
                                                                                                                       dimnames(y$chainVar)[[1]]), dimnames(x$chainVar)[[2]], dimnames(x$chainVar)[[3]]))
        chainVar[1:dim(x$chainVar)[1], , ] <- x$chainVar
        if (iteration <= dim(y$chainVar)[2]) 
            chainVar[(dim(x$chainVar)[1] + 1):dim(chainVar)[1], , ] <- y$chainVar[, 1:iteration, ] else chainVar[(dim(x$chainVar)[1] + 1):dim(chainVar)[1], 1:dim(y$chainVar)[2], ] <- y$chainVar
        
        loggedEvents <- x$loggedEvents
        
        x <- list(call = call, data = data, m = m, nmis = nmis, imp = imp, method = method, predictorMatrix = predictorMatrix, 
                  visitSequence = visitSequence, post = post, seed = seed, iteration = iteration, lastSeedvalue = lastSeedvalue, 
                  chainMean = chainMean, chainVar = chainVar, pad = pad, loggedEvents = loggedEvents)
    }
    
    oldClass(x) <- "mids"
    return(x)
}
