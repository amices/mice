#'Rowwise combination of a \code{mids} object.
#'
#'Append \code{mids} objects by rows
#'
#'This function combines two \code{mids} objects rowwise into a single
#'\code{mids} object or combines a \code{mids} object and a vector, matrix,
#'factor or dataframe rowwise into a \code{mids} object. The number of columns
#'in the (incomplete) data \code{x$data} and \code{y} (or \code{y$data} if
#'\code{y} is a \code{mids} object) should be equal. If \code{y} is a
#'\code{mids} object then the number of imputations in \code{x} and \code{y}
#'should be equal.
#'
#'@param x A \code{mids} object.
#'@param y A \code{mids} object or a \code{data.frame}, \code{matrix}, \code{factor} 
#'or \code{vector}.
#'@param \dots Additional \code{data.frame}, \code{matrix}, \code{vector} or \code{factor}. 
#'These can be given as named arguments.
#'@return An S3 object of class \code{mids}
#'@note Component \code{call} is a vector, with first argument the \code{mice()} statement
#'that created \code{x} and second argument the call to \code{rbind.mids()}. Component 
#'\code{data} is the rowwise combination of the (incomplete) data in \code{x}
#'and \code{y}.
#'Component \code{m} is equal to \code{x$m}. 
#'Component \code{nmis} is an array containing the number of missing observations per
#'column, defined as \code{x$nmis} + \code{y$nmis}. 
#'Component \code{imp} is a list of \code{nvar} components with the generated multiple
#'imputations.  Each part of the list is a \code{nmis[j]} by \code{m} matrix of
#'imputed values for variable \code{j}. If \code{y} is a \code{mids} object
#'then \code{imp[[j]]} equals \code{rbind(x$imp[[j]], y$imp[[j]])}; otherwise
#'the original data of \code{y} will be copied into this list, including the
#'missing values of \code{y} then \code{y} is not imputed.
#'Component \code{method} is a vector of strings of \code{length(nvar)} specifying the
#'elementary imputation method per column defined as \code{x$method}. 
#'Component \code{predictorMatrix} is a square matrix of size \code{ncol(data)}
#'containing the predictor set defined as
#'\code{x$predictorMatrix}. 
#'Component \code{visitSequence} is the sequence in which columns are visited, defined
#'as \code{x$visitSequence}.
#'Component \code{seed} is the seed value of the solution, \code{x$seed}.
#'Component \code{iteration} is the last Gibbs sampling iteration number,
#'\code{x$iteration}. 
#'Component \code{lastSeedValue} is the most recent seed value, \code{x$lastSeedValue}
#'Component \code{chainMean} is set to \code{NA}. 
#'Component \code{chainVar} is set to \code{NA}.
#'Component \code{pad} is set to \code{x$pad}, a list containing various settings of the
#'padded imputation model, i.e. the imputation model after creating dummy
#'variables.
#'Component \code{loggedEvents} is set to \code{x$loggedEvents}.
#'@author Karin Groothuis-Oudshoorn, Stef van Buuren, 2009
#'@seealso \code{\link{cbind.mids}}, \code{\link{ibind}}, \code{\link[=mids-class]{mids}}
#'@references van Buuren S and Groothuis-Oudshoorn K (2011). \code{mice}:
#'Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#'Statistical Software}, \bold{45}(3), 1-67.
#'\url{http://www.jstatsoft.org/v45/i03/}
#'@keywords manip
#'@export
rbind.mids <- function(x, y, ...) {
    # This function combines x and y rowwise into a single midsobject.
    # x is a midsobject; y should be
    # 1) a dataframe with the same (number of) columns as x$data; in this case y 
    # is combined with x$data with rbind() and the list elements of x are adjusted.
    # 2) y is a midsobject with the same underlying multiple imputation model as x but based on a
    # different data subset (with exactly the same variable(names) as in x$data). In this case the data is
    # combined with rbind and the other listelements of the midsobject are adjusted. Beware that 
    # imputations in y are generated independently of x and by combining them could be dangerous. 
    #
    # It is allowed to combine more than two objects when y is not a midsobject.
    # KO 08/09.    
    call <- match.call()
    if (!is.mids(y)) 
        y <- rbind.data.frame(y, ...)
    
    # Then y is matrix, y is converted into a dataframe.
    if (is.matrix(y)) 
        y <- as.data.frame(y)
    
    if (is.data.frame(y)) {
        if (ncol(y) != ncol(x$data)) 
            stop("The two datasets do not have the same number of columns\n")
        
        varnames <- c(dimnames(x$data)[[2]])
        
        # Call is a vector, with first argument the mice statement and second argument the call to cbind.mids.
        call <- c(x$call, call)
        
        # The data in x (x$data) and y are combined together.
        data <- rbind(x$data, y)
        
        # The number of imputations in the new midsobject is equal to that in x.
        m <- x$m
        
        # count the number of missing data in y and add them to x$nmis.
        nmis <- x$nmis + colSums(is.na(y))
        
        # The listelements method, post, predictorMatrix, visitSequence will be copied from x.
        method <- x$method
        post <- x$post
        predictorMatrix <- x$predictorMatrix
        visitSequence <- x$visitSequence
        
        # The original data of y will be copied into the multiple imputed dataset, including the missing values of y.
        r <- (!is.na(y))
        imp <- vector("list", ncol(y))
        for (j in visitSequence) {
            imp[[j]] <- rbind(x$imp[[j]], as.data.frame(matrix(NA, nrow = sum(!r[, j]), ncol = x$m, dimnames = list(row.names(y)[r[, 
                                                                                                                                   j] == FALSE], 1:m))))
        }
        names(imp) <- varnames
        
        # seed, lastSeedvalue, number of iterations, chainMean and chainVar is taken as in mids object x.
        seed <- x$seed
        lastSeedvalue <- x$lastSeedvalue
        iteration <- x$iteration
        chainMean = x$chainMean
        chainVar = x$chainVar
        
        # padModel for the data rbind(x$data,y).
        
        pad <- padModel(data, method, predictorMatrix, visitSequence, post, nmis, nvar = ncol(data))
        
        loggedEvents <- x$loggedEvents
        
        x <- list(call = call, data = data, m = m, nmis = nmis, imp = imp, method = method, predictorMatrix = predictorMatrix, 
                  visitSequence = visitSequence, post = post, seed = seed, iteration = iteration, lastSeedvalue = lastSeedvalue, 
                  chainMean = chainMean, chainVar = chainVar, pad = pad, loggedEvents = loggedEvents)
    }
    
    if (is.mids(y)) {
        if (ncol(y$data) != ncol(x$data)) 
            stop("The two datasets do not have the same number of columns.\n")
        if (!all(c(dimnames(x$data)[[2]]) == c(dimnames(y$data)[[2]]))) 
            stop("The two datasets do not have the same variable(names).\n")
        if (!(x$m == y$m)) 
            stop("The number of imputations differ between x and y.\n")
        
        if (!all(x$method == y$method)) 
            warning("Methods vector is not equal in x and y; y$method is ignored.\n")
        if (!all(x$predictorMatrix == y$predictorMatrix)) 
            warning("Predictormatrix is not equal in x and y; y$predictorMatrix is ignored\n.")
        if (!all(x$visitSequence == y$visitSequence)) 
            warning("Visitsequence is not equal in x and y; y$visitSequence is ignored\n.")
        if (!all(x$post == y$post)) 
            warning("The post vector is not equal in x and y; y$post is ignored\n")
        #if (!all(x$pad$categories == y$pad$categories)) 
        #    warning("The categories in the padmodels are not equal in x and y; y$pad is ignored.\n")
        # Altered GV July 31, 2013 
        if (nrow(x$pad$categories) != nrow(y$pad$categories)){
          warning("The number of categories in the padmodels are not equal in x and y; y$pad is ignored.\n")
        } else {
          if (!all(x$pad$categories == y$pad$categories)){
            warning("The categories in the padmodels are not equal in x and y; y$pad is ignored.\n")
          }  
        }    
        
        varnames <- c(dimnames(x$data)[[2]])
        
        # Call is a vector, with first argument the mice statement and second argument the call to cbind.mids.
        call <- match.call()
        call <- c(x$call, call)
        
        # The data in x (x$data) and y are combined together.
        data <- rbind(x$data, y$data)
        
        # The number of imputations in the new midsobject is equal to that in x.
        m <- x$m
        
        # count the number of missing data in y and add them to x$nmis.
        nmis <- x$nmis + y$nmis
        
        # The listelements method, post, predictorMatrix, visitSequence will be copied from x.
        method <- x$method
        post <- x$post
        predictorMatrix <- x$predictorMatrix
        visitSequence <- x$visitSequence
        
        # The original data of y will be binded into the multiple imputed dataset, including the imputed values of y.
        imp <- vector("list", ncol(x$data))
        #for (j in 1:ncol(x$data)) {
        #    imp[[j]] <- rbind(x$imp[[j]], y$imp[[j]])
        #}
        # Altered GV July 31, 2013
        for (j in 1:ncol(x$data)) {
          if(!is.null(x$imp[[j]]) | !is.null(y$imp[[j]])){
            imp[[j]] <- rbind(x$imp[[j]], y$imp[[j]])  
          }
        }
        names(imp) <- varnames
        
        # seed, lastSeedvalue, number of iterations, chainMean and chainVar is taken as in mids object x.
        seed <- x$seed
        lastSeedvalue <- x$lastSeedvalue
        iteration <- x$iteration
        chainMean = NA
        chainVar = NA
        
        # padModel for the data rbind(x$data,y).
        
        pad <- padModel(data, method, predictorMatrix, visitSequence, post, nmis, nvar = ncol(data))

        loggedEvents <- x$loggedEvents
        
        x <- list(call = call, data = data, m = m, nmis = nmis, imp = imp, method = method, predictorMatrix = predictorMatrix, 
                  visitSequence = visitSequence, post = post, seed = seed, iteration = iteration, lastSeedvalue = lastSeedvalue, 
                  chainMean = chainMean, chainVar = chainVar, pad = pad, loggedEvents = loggedEvents)
    }
    
    
    oldClass(x) <- "mids"
    return(x)
}
