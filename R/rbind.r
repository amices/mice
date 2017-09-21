#'Rowwise combination of a \code{mids} object.
#'
#'Append \code{mids} objects by rows
#'
#'This function combines two \code{mids} objects rowwise into a single
#'\code{mids} object, or combines a \code{mids} object with a vector, matrix,
#'factor or dataframe rowwise into a \code{mids} object. The columns
#'in the (incomplete) data \code{x$data} and \code{y} (or \code{y$data} if
#'\code{y} is a \code{mids} object) should match. If \code{y} is a
#'\code{mids} object, then \code{rbind} only works if the number of 
#'multiple imputations in \code{x} and \code{y} is equal.
#'
#'@param x A \code{mids} object.
#'@param y A \code{mids} object, or a \code{data.frame}, \code{matrix}, \code{factor} 
#'or \code{vector}.
#'@param \dots Additional \code{data.frame}, \code{matrix}, \code{vector} or \code{factor}. 
#'These can be given as named arguments.
#'@return An S3 object of class \code{mids}
#'@note The function construct the elements of the new \code{mids} object as follows:
#'\tabular{ll}{
#'\code{call}     \tab Vector, \code{call[1]} creates \code{x}, \code{call[2]} is call to \code{rbind.mids}\cr
#'\code{data}     \tab Rowwise combination of the (incomplete) data in \code{x} and \code{y}\cr
#'\code{where}    \tab Rowwise combination of \code{where} arguments\cr
#'\code{m}        \tab Equals \code{x$m}\cr
#'\code{nmis}     \tab \code{x$nmis} + \code{y$nmis}\cr
#'\code{imp}      \tab Equals \code{rbind(x$imp[[j]], y$imp[[j]])} if \code{y} is \code{mids} object; otherwise
#'the data of \code{y} will be copied\cr
#'\code{method}   \tab Taken from \code{x$method}\cr
#'\code{predictorMatrix} \tab Taken from \code{x$predictorMatrix}\cr
#'\code{visitSequence}   \tab Taken from \code{x$visitSequence}\cr
#'\code{seed}            \tab Taken from \code{x$seed}\cr
#'\code{iteration}       \tab Taken from \code{x$iteration}\cr
#'\code{lastSeedValue}   \tab Taken from \code{x$lastSeedValue}\cr
#'\code{chainMean}       \tab Set to \code{NA}\cr
#'\code{chainVar}        \tab Set to \code{NA}\cr
#'\code{pad}             \tab Recreated by \code{padModel()}\cr
#'\code{loggedEvents}    \tab Taken from \code{x$loggedEvents}
#'}
#'@author Karin Groothuis-Oudshoorn, Stef van Buuren
#'@seealso \code{\link{cbind.mids}}, \code{\link{ibind}}, \code{\link[=mids-class]{mids}}
#'@references van Buuren S and Groothuis-Oudshoorn K (2011). \code{mice}:
#'Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#'Statistical Software}, \bold{45}(3), 1-67.
#'\url{http://www.jstatsoft.org/v45/i03/}
#'@keywords manip
#'@examples
#'imp1 <- mice(nhanes[1:13, ], m = 2, maxit = 1, print = FALSE)
#'imp5 <- mice(nhanes[1:13, ], m = 2, maxit = 2, print = FALSE)
#'mylist <- list(age = NA, bmi = NA, hyp = NA, chl = NA)
#'
#'nrow(complete(rbind(imp1, imp5)))
#'nrow(complete(rbind(imp1, mylist)))
#'
#'# Note: If one of the arguments is a data.frame 
#'# we need to explicitly call mice:::rbind.mids()
#'nrow(complete(mice:::rbind.mids(imp1, data.frame(mylist))))
#'nrow(complete(mice:::rbind.mids(imp1, complete(imp5))))
#'@export
rbind.mids <- function(x, y = NULL, ...) {
  call <- match.call()
  if (!is.mids(y)) {
    # Combine y and dots into data.frame
    if (is.null(y)) {
      y <- rbind.data.frame(...) 
    } else {
      y <- rbind.data.frame(y, ...)
    }

    # Then y is matrix, y is converted into a dataframe.
    # if (is.matrix(y)) 
    #   y <- as.data.frame(y)
    
    if (is.data.frame(y)) {
      if (ncol(y) != ncol(x$data)) 
        stop("Datasets have different number of columns")
    }
    
    varnames <- colnames(x$data)
    
    # Call is a vector, with first argument the mice statement and second argument the call to cbind.mids.
    call <- c(x$call, call)
    
    # The data in x (x$data) and y are combined together.
    data <- rbind(x$data, y)
    
    # where argument
    where <- x$where
    
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
      imp[[j]] <- 
        rbind(x$imp[[j]], 
              as.data.frame(matrix(NA, nrow = sum(!r[, j]), ncol = x$m, 
                                   dimnames = list(row.names(y)[r[, j] == FALSE], 1:m))))
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
    
    x <- list(call = call, data = data, where = where, m = m, nmis = nmis, imp = imp, method = method, predictorMatrix = predictorMatrix, 
              visitSequence = visitSequence, post = post, seed = seed, iteration = iteration, lastSeedvalue = lastSeedvalue, 
              chainMean = chainMean, chainVar = chainVar, pad = pad, loggedEvents = loggedEvents)
  }
  
  if (is.mids(y)) {
    if (ncol(y$data) != ncol(x$data))
      stop("Datasets have different number of columns")
    if (!identical(colnames(x$data),  colnames(y$data)))
      stop("Datasets have different variable names")
    if (!identical(sapply(x$data, is.factor),  sapply(y$data, is.factor)))
      stop("Datasets have different factor variables")
    if (x$m != y$m)
      stop("Number of imputations differ")
    
    warned <- FALSE
    if (!identical(x$method, y$method) && !warned) {
      warning("`methods` not equal; ignores y$method")
      warned <- TRUE
    }
    if (!identical(x$predictorMatrix, y$predictorMatrix) && !warned) {
      warning("`predictorMatrix` not equal; ignores y$predictorMatrix")
      warned <- TRUE
    }
    if (!identical(x$visitSequence, y$visitSequence) && !warned) {
      warning("`visitSequence` not equal; ignores y$visitSequence")
      warned <- TRUE
    }
    if (!identical(x$post, y$post) && !warned) {
      warning("`post` not equal; ignores y$post")
      warned <- TRUE
    }
    if (!identical(x$pad$categories, y$pad$categories) && !warned) {
      warning("`pad$categories` not equal; ignores y$pad")
      warned <- TRUE
    }
    
    varnames <- colnames(x$data)
    
    # Call is a vector, with first argument the mice statement and second argument the call to cbind.mids.
    call <- match.call()
    call <- c(x$call, call)
    
    # The data in x (x$data) and y are combined together.
    data <- rbind(x$data, y$data)
    
    # Where argument
    where <- rbind(x$where, y$where)
    
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
    for (j in seq_len(ncol(x$data))) {
      if(!is.null(x$imp[[j]]) || !is.null(y$imp[[j]])) {
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
    
    x <- list(call = call, data = data, where = where, m = m, nmis = nmis, 
              imp = imp, method = method, predictorMatrix = predictorMatrix, 
              visitSequence = visitSequence, post = post, seed = seed, 
              iteration = iteration, lastSeedvalue = lastSeedvalue, 
              chainMean = chainMean, chainVar = chainVar, pad = pad, 
              loggedEvents = loggedEvents)
  }
  
  oldClass(x) <- "mids"
  return(x)
}
