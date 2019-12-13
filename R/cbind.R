#'Combine \code{mids} objects by columns
#'
#'This function combines two \code{mids} objects columnwise into a single
#'object of class \code{mids}, or combines a single \code{mids} object with 
#'a \code{vector}, \code{matrix}, \code{factor} or \code{data.frame} 
#'columnwise into a \code{mids} object.
#'
#'\emph{Pre-requisites:} If \code{y} is a \code{mids}-object, the rows 
#'of \code{x$data} and \code{y$data} should match, as well as the number 
#'of imputations (\code{m}). Other \code{y} are transformed into a 
#'\code{data.frame} whose rows should match with \code{x$data}.
#'
#'The function renames any duplicated variable or block names by 
#'appending \code{".1"}, \code{".2"} to duplicated names.
#' 
#'@param x A \code{mids} object.
#'@param y A \code{mids} object, or a \code{data.frame}, \code{matrix}, 
#'\code{factor} or \code{vector}.
#'@param \dots Additional \code{data.frame}, \code{matrix}, \code{vector} 
#'or \code{factor}. These can be given as named arguments.
#'@return An S3 object of class \code{mids}
#'@note
#'The function constructs the elements of the new \code{mids} object as follows:
#'\tabular{ll}{
#'\code{data}     \tab Columnwise combination of the data in \code{x} and \code{y}\cr
#'\code{imp}      \tab Combines the imputed values from \code{x} and \code{y}\cr
#'\code{m}        \tab Taken from \code{x$m}\cr
#'\code{where}    \tab Columnwise combination of \code{x$where} and \code{y$where}\cr
#'\code{blocks}   \tab Combines \code{x$blocks} and \code{y$blocks}\cr
#'\code{call}     \tab Vector, \code{call[1]} creates \code{x}, \code{call[2]} 
#'is call to \code{cbind.mids}\cr
#'\code{nmis}     \tab Equals \code{c(x$nmis, y$nmis)}\cr
#'\code{method}   \tab Combines \code{x$method} and \code{y$method}\cr
#'\code{predictorMatrix} \tab Combination with zeroes on the off-diagonal blocks\cr
#'\code{visitSequence}   \tab Combined as \code{c(x$visitSequence, y$visitSequence)}\cr
#'\code{formulas}  \tab Combined as \code{c(x$formulas, y$formulas)}\cr
#'\code{post}      \tab Combined as \code{c(x$post, y$post)}\cr
#'\code{blots}     \tab Combined as \code{c(x$blots, y$blots)}\cr
#'\code{seed}            \tab Taken from \code{x$seed}\cr
#'\code{iteration}       \tab Taken from \code{x$iteration}\cr
#'\code{lastSeedValue}   \tab Taken from \code{x$lastSeedValue}\cr
#'\code{chainMean}       \tab Combined from \code{x$chainMean} and \code{y$chainMean}\cr
#'\code{chainVar}        \tab Combined from \code{x$chainVar} and \code{y$chainVar}\cr
#'\code{loggedEvents}    \tab Taken from \code{x$loggedEvents}\cr
#'\code{version}    \tab Current package version\cr
#'\code{date}       \tab Current date\cr
#'}
#'
#'@author Karin Groothuis-Oudshoorn, Stef van Buuren
#'@seealso \code{\link{cbind}}, \code{\link{rbind.mids}}, \code{\link{ibind}}, 
#'\code{\link[=mids-class]{mids}}
#'@keywords manip
#'@examples
#'
#'# impute four variables at once (default)
#'imp <- mice(nhanes, m = 1, maxit = 1, print = FALSE)
#'imp$predictorMatrix
#'
#'# impute two by two
#'data1 <- nhanes[, c("age", "bmi")]
#'data2 <- nhanes[, c("hyp", "chl")]
#'imp1 <- mice(data1, m = 2, maxit = 1, print = FALSE)
#'imp2 <- mice(data2, m = 2, maxit = 1, print = FALSE)
#'
#'# Append two solutions
#'imp12 <- cbind(imp1, imp2)
#'
#'# This is a different imputation model
#'imp12$predictorMatrix
#'
#'# Append the other way around
#'imp21 <- cbind(imp2, imp1)
#'imp21$predictorMatrix
#'
#'# Append 'forgotten' variable chl
#'data3 <- nhanes[, 1:3]
#'imp3  <- mice(data3, maxit = 1,m = 2, print = FALSE)
#'imp4 <- cbind(imp3, chl = nhanes$chl)
#'
#'# Of course, chl was not imputed
#'head(complete(imp4))
#'
#'# Combine mids object with data frame
#'imp5 <- cbind(imp3, nhanes2)
#'head(complete(imp5))
cbind.mids <- function(x, y = NULL, ...) {
  call <- match.call()
  dots <- list(...)
  if (is.mids(y)) return(cbind.mids.mids(x, y, call = call))
  
  if ((is.null(y) || length(y) == 0L) && length(dots) == 0L) return(x)
  n <- nrow(x$data)
  if (length(y) == 1L) y <- rep(y, n)
  if (length(y) == 0L && length(dots) > 0L) y <- cbind.data.frame(dots)
  else if (length(y) > 0L && length(dots) == 0L) y <- cbind.data.frame(y)
  else y <- cbind.data.frame(y, dots)
  
  # Call is a vector, with first argument the mice statement 
  # and second argument the call to cbind.mids.
  call <- c(x$call, call)
  
  if (nrow(y) != nrow(x$data)) 
    stop("arguments imply differing number of rows: ", 
         c(nrow(x$data), ", ", nrow(y)))
  data <- cbind(x$data, y)
  varnames <- make.unique(colnames(data))
  colnames(data) <- varnames
  
  # where argument
  where <- cbind(x$where, matrix(FALSE, nrow = nrow(x$where), ncol = ncol(y)))
  colnames(where) <- varnames
  
  # blocks: no renaming needed because all block definition will 
  # refer to varnames[1:ncol(x$data)] only, and are hence unique
  # but we do need to rename duplicate block names
  yblocks <- vector("list", length = ncol(y))
  blocks <- c(x$blocks, yblocks)
  xynames <- c(names(x$blocks), colnames(y))
  blocknames <- make.unique(xynames)
  names(blocknames) <- xynames
  names(blocks) <- blocknames
  ct <- c(attr(x$blocks, "calltype"), rep("type", ncol(y)))
  names(ct) <- blocknames
  attr(blocks, "calltype") <- ct
  
  m <- x$m
  
  # count the number of missing data in y
  nmis <- c(x$nmis, colSums(is.na(y)))
  names(nmis) <- varnames
  
  # imp: original data of y will be copied into the multiple imputed dataset, 
  # including the missing values of y.
  r <- (!is.na(y))
  f <- function(j) {
    m <- matrix(NA,
                nrow = sum(!r[, j]),
                ncol = x$m,
                dimnames = list(row.names(y)[!r[, j]], seq_len(m)))
    as.data.frame(m)
  }
  imp <- lapply(seq_len(ncol(y)), f)
  imp <- c(x$imp, imp)
  names(imp) <- varnames
  
  # The imputation method for (columns in) y will be set to ''.
  method <- c(x$method, rep.int("", ncol(y)))
  names(method) <- blocknames
  
  # The variable(s) in y are included in the predictorMatrix. 
  # y is not used as predictor as well as not imputed.
  predictorMatrix <- rbind(x$predictorMatrix, 
                           matrix(0, 
                                  ncol = ncol(x$predictorMatrix), 
                                  nrow = ncol(y)))
  predictorMatrix <- cbind(predictorMatrix, 
                           matrix(0, 
                                  ncol = ncol(y), 
                                  nrow = nrow(x$predictorMatrix) + ncol(y)))
  rownames(predictorMatrix) <- blocknames
  colnames(predictorMatrix) <- varnames
  
  visitSequence <- x$visitSequence
  formulas <- x$formulas
  post <- c(x$post, rep.int("", ncol(y)))
  names(post) <- varnames
  blots <- x$blots
  
  # seed, lastSeedvalue, number of iterations, chainMean and chainVar 
  # is taken as in mids object x.
  seed <- x$seed
  lastSeedvalue <- x$lastSeedvalue
  iteration <- x$iteration
  chainMean <- x$chainMean
  chainVar <- x$chainVar
  
  loggedEvents <- x$loggedEvents
  
  ## save, and return
  midsobj <- list(data = data, imp = imp, m = m,
                  where = where, blocks = blocks, 
                  call = call, nmis = nmis, 
                  method = method,
                  predictorMatrix = predictorMatrix,
                  visitSequence = visitSequence, 
                  formulas = formulas, post = post, 
                  blots = blots, seed = seed, 
                  iteration = iteration,
                  lastSeedValue = .Random.seed, 
                  chainMean = chainMean,
                  chainVar = chainVar, 
                  loggedEvents = loggedEvents, 
                  version = packageVersion("mice"),
                  date = Sys.Date())
  oldClass(midsobj) <- "mids"
  return(midsobj)
}


cbind.mids.mids <- function(x, y, call) {
  if (!is.mids(y)) stop("Argument `y` not a mids object")
  
  if (nrow(y$data) != nrow(x$data)) 
    stop("The two datasets do not have the same length\n")
  if (x$m != y$m) 
    stop("The two mids objects should have the same number of imputations\n")
  
  # Call is a vector, with first argument the mice statement 
  # and second argument the call to cbind.mids.
  call <- c(x$call, call)
  
  # The data in x$data and y$data are combined together.
  # make variable names unique
  data <- cbind(x$data, y$data)
  xynames <- c(colnames(x$data), colnames(y$data))
  varnames <- make.unique(xynames)
  names(varnames) <- xynames
  names(data) <- varnames
  
  where <- cbind(x$where, y$where)
  colnames(where) <- varnames
  
  # rename variable names within each x$blocks and y$blocks
  xnew <- varnames[1:ncol(x$data)]
  ynew <- varnames[-(1:ncol(x$data))]
  xblocks <- x$blocks
  yblocks <- y$blocks
  for (i in names(xblocks)) xblocks[[i]] <- unname(xnew[xblocks[[i]]])
  for (i in names(yblocks)) yblocks[[i]] <- unname(ynew[yblocks[[i]]])
  blocks <- c(xblocks, yblocks)
  xynames <- c(names(xblocks), names(yblocks))
  blocknames <- make.unique(xynames)
  names(blocknames) <- xynames
  names(blocks) <- blocknames
  ct <- c(attr(xblocks, "calltype"), attr(yblocks, "calltype"))
  names(ct) <- blocknames
  attr(blocks, "calltype") <- ct
  
  m <- x$m
  nmis <- c(x$nmis, y$nmis)
  names(nmis) <- varnames
  imp <- c(x$imp, y$imp)
  names(imp) <- varnames
  method <- c(x$method, y$method)
  names(method) <- blocknames
  
  # The predictorMatrices of x and y are combined with zero matrices 
  # on the off diagonal blocks.
  predictorMatrix <- rbind(x$predictorMatrix, 
                           matrix(0, 
                                  ncol = ncol(x$predictorMatrix), 
                                  nrow = nrow(y$predictorMatrix)))
  predictorMatrix <- cbind(predictorMatrix, 
                           rbind(matrix(0, 
                                        ncol = ncol(y$predictorMatrix), 
                                        nrow = nrow(x$predictorMatrix)), 
                                 y$predictorMatrix))
  rownames(predictorMatrix) <- blocknames
  colnames(predictorMatrix) <- varnames
  
  # As visitSequence is taken first the order for x and after that from y.
  # take care that duplicate names need to be renamed
  xnew <- blocknames[1:length(x$blocks)]
  ynew <- blocknames[-(1:length(x$blocks))]
  visitSequence <- unname(c(xnew[x$visitSequence], ynew[y$visitSequence]))
  
  formulas <- c(x$formulas, y$formulas)
  names(formulas) <- blocknames
  post <- c(x$post, y$post)
  names(post) <- varnames
  blots <- c(x$blots, y$blots)
  names(blots) <- blocknames
  
  # For the elements seed, lastSeedvalue and iteration the values 
  # from midsobject x are copied.
  seed <- x$seed
  lastSeedvalue <- x$lastSeedvalue
  iteration <- x$iteration
  
  # the chainMean and chainVar vectors for x and y are combined.
  chainMean <- array(data = NA, 
                     dim = c(dim(x$chainMean)[1] + dim(y$chainMean)[1], iteration, m), 
                     dimnames = list(c(dimnames(x$chainMean)[[1]], 
                                       dimnames(y$chainMean)[[1]]), 
                                     dimnames(x$chainMean)[[2]], 
                                     dimnames(x$chainMean)[[3]]))
  chainMean[seq_len(dim(x$chainMean)[1]), , ] <- x$chainMean
  
  if (iteration <= dim(y$chainMean)[2]) {
    chainMean[(dim(x$chainMean)[1] + 1):dim(chainMean)[1], , ] <- y$chainMean[, seq_len(iteration), ] 
  } else { 
    chainMean[(dim(x$chainMean)[1] + 1):dim(chainMean)[1], seq_len(dim(y$chainMean)[2]), ] <- y$chainMean
  }
  
  chainVar <- array(data = NA, dim = c(dim(x$chainVar)[1] + dim(y$chainVar)[1], iteration, m), 
                    dimnames = list(c(dimnames(x$chainVar)[[1]], 
                                      dimnames(y$chainVar)[[1]]), 
                                    dimnames(x$chainVar)[[2]], 
                                    dimnames(x$chainVar)[[3]]))
  chainVar[seq_len(dim(x$chainVar)[1]), , ] <- x$chainVar
  
  if (iteration <= dim(y$chainVar)[2]) {
    chainVar[(dim(x$chainVar)[1] + 1):dim(chainVar)[1], , ] <- y$chainVar[, seq_len(iteration), ] 
  } else { 
    chainVar[(dim(x$chainVar)[1] + 1):dim(chainVar)[1], seq_len(dim(y$chainVar)[2]), ] <- y$chainVar
  }
  
  loggedEvents <- x$loggedEvents
  
  midsobj <- list(data = data, imp = imp, m = m,
                  where = where, blocks = blocks, 
                  call = call, nmis = nmis, 
                  method = method,
                  predictorMatrix = predictorMatrix,
                  visitSequence = visitSequence, 
                  formulas = formulas, post = post, 
                  blots = blots, seed = seed, 
                  iteration = iteration,
                  lastSeedValue = .Random.seed, 
                  chainMean = chainMean,
                  chainVar = chainVar, 
                  loggedEvents = loggedEvents, 
                  version = packageVersion("mice"),
                  date = Sys.Date())
  oldClass(midsobj) <- "mids"
  return(midsobj)
}

