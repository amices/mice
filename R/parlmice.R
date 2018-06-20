#'Wrapper function that runs MICE in parallel
#'
#'This is a wrapper function for \code{\link{mice}}, using multiple cores to 
#'execute \code{\link{mice}} in parallel. As a result, the imputation 
#'procedure can be sped up, which may be useful in general.
#'
#'This function relies on package \code{\link{parallel}}, which is a base
#'package for R versions 2.14.0 and later. We have chosen to use parallel function 
#'\code{parLapply} to allow the use of \code{parlmice} on Mac, Linux and Windows
#'systems. For the same reason, we use the Parallel Socket Cluster (PSOCK) type by default. 
#'
#'On systems other than Windows, it can be hugely beneficial to change the cluster type to 
#'\code{FORK}, as it generally results in improved memory handling. When memory issues 
#'arise on a Windows system, we advise to store the multiply imputed datasets, 
#'clean the memory by using \code{\link{rm}} and \code{\link{gc}} and make another 
#'run using the same settings.
#'
#'This wrapper function combines the output of \code{\link{parLapply}} with
#'function \code{\link{ibind}} in \code{\link{mice}}. A \code{mids} object is returned
#'and can be used for further analyses. 
#'
#'Note that if a seed value is desired, the seed should be entered to this function
#'with argument \code{seed}. Seed values outside the wrapper function (in an 
#'R-script or passed to \code{\link{mice}}) will not result to reproducible results. 
#'We refer to the manual of \code{\link{parallel}} for an explanation on this matter.  
#'
#'@aliases parlmice
#'@param data A data frame or matrix containing the incomplete data. Similar to 
#'the first argument of \code{\link{mice}}.
#'@param n.core A scalar indicating the number of cores that should be used. 
#'@param n.imp.core A scalar indicating the number of imputations per core. 
#'@param cluster.seed A scalar to be used as the seed value. It is recommended to put the 
#'seed value here and not outside this function, as otherwise the parallel processes
#'will be performed with separate, random seeds. 
#'@param m The number of desired imputated datasets. By default $m=5$ as with \code{mice}
#'@param cl.type The cluster type. Default value is \code{"PSOCK"}. Posix machines (linux, Mac)
#'generally benefit from much faster cluster computation if \code{type} is set to \code{type = "FORK"}. 
#'@param seed A scalar to be used as the seed value for the mice algorithm within 
#'each parallel stream. Please note that the imputations will be the same for all 
#'streams and, hence, this should be used if and only if \code{n.core = 1} and 
#'if it is desired to obtain the same output as under \code{mice}. 
#'@param where A data frame or matrix with logicals of the same dimensions 
#'as \code{data} indicating where in the data the imputations should be 
#'created. The default, \code{where = is.na(data)}, specifies that the
#'missing data should be imputed. The \code{where} argument may be used to 
#'overimpute observed data, or to skip imputations for selected missing values.
#'@param blocks List of vectors with variable names per block. List elements 
#'may be named to identify blocks. Variables within a block are 
#'imputed by a multivariate imputation method
#'(see \code{method} argument). By default each variable is placed 
#'into its own block, which is effectively
#'fully conditional specification (FCS) by univariate models 
#'(variable-by-variable imputation). Only variables whose names appear in 
#'\code{blocks} are imputed. The relevant columns in the \code{where} 
#'matrix are set to \code{FALSE} of variables that are not block members. 
#'A variable may appear in multiple blocks. In that case, it is 
#'effectively re-imputed each time that it is visited.
#'@param method Can be either a single string, or a vector of strings with
#'length \code{length(blocks)}, specifying the imputation method to be
#'used for each column in data. If specified as a single string, the same
#'method will be used for all blocks. The default imputation method (when no
#'argument is specified) depends on the measurement level of the target column,
#'as regulated by the \code{defaultMethod} argument. Columns that need
#'not be imputed have the empty method \code{""}. See details.
#'@param predictorMatrix A numeric matrix of \code{length(blocks)} rows 
#'and \code{ncol(data)} columns, containing 0/1 data specifying 
#'the set of predictors to be used for each target column.
#'Each row corresponds to a variable block, i.e., a set of variables 
#'to be imputed. A value of \code{1} means that the column
#'variable is used as a predictor for the target block (in the rows). 
#'By default, the \code{predictorMatrix} is a square matrix of \code{ncol(data)}
#'rows and columns with all 1's, except for the diagonal. 
#'Note: For two-level imputation models (which have \code{"2l"} in their names)
#'other codes (e.g, \code{2} or \code{-2}) are also allowed.
#'@param visitSequence A vector of block names of arbitrary length, specifying the
#'sequence of blocks that are imputed during one iteration of the Gibbs 
#'sampler. A block is a collection of variables. All variables that are 
#'members of the same block are imputed 
#'when the block is visited. A variable that is a member of multiple blocks 
#'is re-imputed within the same iteration. 
#'The default \code{visitSequence = "roman"} visits the blocks (left to right)
#'in the order in which they appear in \code{blocks}. 
#'One may also use one of the following keywords: \code{"arabic"} 
#'(right to left), \code{"monotone"} (ordered low to high proportion 
#'of missing data) and \code{"revmonotone"} (reverse of monotone). 
#'@param formulas A named list of formula's, or expressions that
#'can be converted into formula's by \code{as.formula}. List elements
#'correspond to blocks. The block to which the list element applies is 
#'identified by its name, so list names must correspond to block names.
#'The \code{formulas} argument is an alternative to the 
#'\code{predictorMatrix} argument that allows for more flexibility in 
#'specifying imputation models, e.g., for specifying interaction terms. 
#'@param blots A named \code{list} of \code{alist}'s that can be used 
#'to pass down arguments to lower level imputation function. The entries
#'of element \code{blots[[blockname]]} are passed down to the function
#'called for block \code{blockname}.
#'@param post A vector of strings with length \code{ncol(data)} specifying
#'expressions as strings. Each string is parsed and 
#'executed within the \code{sampler()} function to post-process 
#'imputed values during the iterations. 
#'The default is a vector of empty strings, indicating no post-processing.
#'@param defaultMethod A vector of length 4 containing the default
#'imputation methods for 1) numeric data, 2) factor data with 2 levels, 3) 
#'factor data with > 2 unordered levels, and 4) factor data with > 2 
#'ordered levels. By default, the method uses 
#'\code{pmm}, predictive mean matching (numeric data) \code{logreg}, logistic
#'regression imputation (binary data, factor with 2 levels) \code{polyreg},
#'polytomous regression imputation for unordered categorical data (factor > 2
#'levels) \code{polr}, proportional odds model for (ordered, > 2 levels).
#'@param maxit A scalar giving the number of iterations. The default is 5.
#'@param data.init A data frame of the same size and type as \code{data},
#'without missing data, used to initialize imputations before the start of the
#'iterative process.  The default \code{NULL} implies that starting imputation
#'are created by a simple random draw from the data. Note that specification of
#'\code{data.init} will start all \code{m} Gibbs sampling streams from the same
#'imputation.
#'@param ... Named arguments that are passed down to function \code{\link{mice}} or
#'\code{\link{makeCluster}}. 
#'
#'@return A mids object as defined by \code{\link{mids-class}}
#'
#'@author Gerko Vink, 2018, based on an earlier version by Rianne Schouten and Gerko Vink, 2017.
#'@seealso \code{\link{parallel}}, \code{\link{parLapply}}, \code{\link{makeCluster}},
#'\code{\link{mice}}, \code{\link{mids-class}}
#'@references 
#'Schouten, R. and Vink, G. (2017). parlmice: faster, paraleller, micer. 
#'\url{https://gerkovink.github.io/parlMICE/Vignette_parlMICE.html}
#'
#'#'Van Buuren, S. (2018). 
#'\href{https://stefvanbuuren.name/fimd/parallel-computation.html}{\emph{Flexible Imputation of Missing Data. Second Edition.}}
#'Chapman & Hall/CRC. Boca Raton, FL.
#'
#'@examples
#'# 150 imputations in dataset nhanes, performed by 3 cores  
#'\donttest{
#'imp1 <- parlmice(data = nhanes, n.core = 3, n.imp.core = 50)
#'# Making use of arguments in mice. 
#'imp2 <- parlmice(data = nhanes, method = "norm.nob", m = 100)
#'imp2$method
#'fit <- with(imp2, lm(bmi ~ hyp))
#'pool(fit)
#' }
#' 
#'@export
parlmice <- function(data, m = 5, n.core = NULL, n.imp.core = NULL, cluster.seed = NA,  
                     seed = NA, cl.type = "PSOCK", method, predictorMatrix, where, 
                     blocks, visitSequence, formulas, blots, post, defaultMethod, 
                     maxit, data.init, ...){ 
  # check form of data and m
  data <- check.dataform(data)
  m <- check.m(m)
  
  # check if data complete
  if (sum(is.na(data)) == 0){
    stop("Data has no missing values")
  }
  
  # check if arguments match CPU specifications
  if (!is.null(n.core)){
    if(n.core > parallel::detectCores()){ 
      stop("Number of cores specified is greater than the number of logical cores in your CPU")
    }
  } 
  
  # determine course of action when not all arguments specified
  if (!is.null(n.core) & is.null(n.imp.core)){
    n.imp.core = m
    warning(paste("Number of imputations per core not specified: n.imp.core = m =", m, "has been used"))
  }
  if (is.null(n.core) & !is.null(n.imp.core)){
    n.core = parallel::detectCores() - 1
    warning(paste("Number of cores not specified. Based on your machine a value of n.core =", parallel::detectCores()-1, "is chosen"))
  }
  if (is.null(n.core) & is.null(n.imp.core)) {
    specs <- match.cluster(n.core = parallel::detectCores() - 1, m = m)
    n.core = specs$cores
    n.imp.core = specs$imps
  }
  if (!is.na(seed)){
    if(n.core > 1){
      warning("Be careful; the specified seed is equal for all imputations. Please consider specifying cluster.seed instead.")
    }
  } 
  
  # create arguments to export to cluster
  args <- match.call(mice)
  args[[1]] <- NULL
  args$data <- as.name("data")
  args$m <- n.imp.core
  if(!missing(visitSequence)){args$visitSequence <- visitSequence}
  if(!missing(method)){args$method <- method}
  if(!missing(predictorMatrix)){args$predictorMatrix <- predictorMatrix}
  if(!missing(where)){args$where <- where}
  if(!missing(formulas)){args$formulas <- formulas}
  if(!missing(blocks)){args$blocks <- blocks}
  if(!missing(blots)){args$blots <- blots}
  if(!missing(post)){args$post <- post}
  if(!missing(data.init)){args$data.init <- data.init}
  
  # make computing cluster
  cl <- parallel::makeCluster(n.core, type = cl.type)
  parallel::clusterExport(cl, 
                          varlist = names(args), 
                          envir = environment())
  parallel::clusterExport(cl, 
                          varlist = "do.call")
  parallel::clusterEvalQ(cl, library(mice))
  if (!is.na(cluster.seed)) {
    parallel::clusterSetRNGStream(cl, cluster.seed)
  }
  
  # generate imputations
  imps <- parallel::parLapply(cl = cl, X = 1:n.core, function(x) do.call(mice, as.list(args), envir = environment()))
  parallel::stopCluster(cl)
  
  # postprocess clustered imputation into a mids object
  imp <- imps[[1]]
  if (length(imps) > 1) {
    for (i in 2:length(imps)) {
      imp <- ibind(imp, imps[[i]])
    }
  }
  for(i in 1:length(imp$imp)){ #let imputation matrix correspond to grand m
    colnames(imp$imp[[i]]) <- 1:imp$m
  }
  
  return(imp)
}

match.cluster <- function(n.core, m){
  cores <- 1:n.core
  imps <- 1:m
  out <- data.frame(results = as.vector(cores %*% t(imps)),
                    cores = cores,
                    imps = rep(imps, each = n.core))
  which  <- out[out[, "results"] == m, ]
  which[order(which$cores, decreasing = T), ][1, 2:3]
}
