#'Multivariate Imputation by Chained Equations (MICE)
#'
#'Generates Multivariate Imputations by Chained Equations (MICE)
#'
#'Generates multiple imputations for incomplete multivariate data by Gibbs
#'sampling. Missing data can occur anywhere in the data. The algorithm imputes
#'an incomplete column (the target column) by generating 'plausible' synthetic
#'values given other columns in the data. Each incomplete column must act as a
#'target column, and has its own specific set of predictors. The default set of
#'predictors for a given target consists of all other columns in the data. For
#'predictors that are incomplete themselves, the most recently generated
#'imputations are used to complete the predictors prior to imputation of the
#'target column.
#'
#'A separate univariate imputation model can be specified for each column. The
#'default imputation method depends on the measurement level of the target
#'column. In addition to these, several other methods are provided. You can
#'also write their own imputation functions, and call these from within the
#'algorithm.
#'
#'The data may contain categorical variables that are used in a regressions on
#'other variables. The algorithm creates dummy variables for the categories of
#'these variables, and imputes these from the corresponding categorical
#'variable. The extended model containing the dummy variables is called the
#'padded model. Its structure is stored in the list component \code{pad}.
#'
#'Built-in univariate imputation methods are:
#'
#'\tabular{lll}{
#'\code{pmm}          \tab any     \tab Predictive mean matching\cr
#'\code{midastouch}   \tab any     \tab Weighted predictive mean matching\cr
#'\code{sample}       \tab any     \tab Random sample from observed values\cr
#'\code{cart}         \tab any     \tab Classification and regression trees\cr
#'\code{rf}           \tab any     \tab Random forest imputations\cr
#'\code{mean}         \tab numeric \tab Unconditional mean imputation\cr
#'\code{norm}         \tab numeric \tab Bayesian linear regression\cr
#'\code{norm.nob}     \tab numeric \tab Linear regression ignoring model error\cr
#'\code{norm.boot}    \tab numeric \tab Linear regression using bootstrap\cr
#'\code{norm.predict} \tab numeric \tab Linear regression, predicted values\cr
#'\code{quadratic}    \tab numeric \tab Imputation of quadratic terms\cr
#'\code{ri}           \tab numeric \tab Random indicator for nonignorable data\cr
#'\code{logreg}       \tab binary  \tab Logistic regression\cr
#'\code{logreg.boot}  \tab binary  \tab Logistic regression with bootstrap\cr
#'\code{polr}         \tab ordered \tab Proportional odds model\cr
#'\code{polyreg}      \tab unordered\tab Polytomous logistic regression\cr
#'\code{lda}          \tab unordered\tab Linear discriminant analysis\cr
#'\code{2l.norm}      \tab numeric  \tab Level-1 normal heteroskedastic\cr
#'\code{2l.lmer}      \tab numeric  \tab Level-1 normal homoscedastic, lmer\cr
#'\code{2l.pan}       \tab numeric  \tab Level-1 normal homoscedastic, pan\cr
#'\code{2lonly.mean}  \tab numeric  \tab Level-2 class mean\cr
#'\code{2lonly.norm}  \tab numeric  \tab Level-2 class normal\cr
#'\code{2lonly.pmm}   \tab any      \tab Level-2 class predictive mean matching
#'}
#'
#'These corresponding functions are coded in the \code{mice} library under
#'names \code{mice.impute.method}, where \code{method} is a string with the
#'name of the univariate imputation method name, for example \code{norm}. The
#'\code{method} argument specifies the methods to be used.  For the \code{j}'th
#'column, \code{mice()} calls the first occurence of
#'\code{paste('mice.impute.', method[j], sep = '')} in the search path.  The
#'mechanism allows uses to write customized imputation function,
#'\code{mice.impute.myfunc}. To call it for all columns specify
#'\code{method='myfunc'}.  To call it only for, say, column 2 specify
#'\code{method=c('norm','myfunc','logreg',\dots{})}.
#'
#'\emph{Passive imputation:} \code{mice()} supports a special built-in method,
#'called passive imputation. This method can be used to ensure that a data
#'transform always depends on the most recently generated imputations.  In some
#'cases, an imputation model may need transformed data in addition to the
#'original data (e.g. log, quadratic, recodes, interaction, sum scores, and so
#'on).
#'
#'Passive imputation maintains consistency among different transformations of
#'the same data. Passive imputation is invoked if \code{~} is specified as the
#'first character of the string that specifies the univariate method.
#'\code{mice()} interprets the entire string, including the \code{~} character,
#'as the formula argument in a call to \code{model.frame(formula,
#'data[!r[,j],])}. This provides a simple mechanism for specifying determinstic
#'dependencies among the columns. For example, suppose that the missing entries
#'in variables \code{data$height} and \code{data$weight} are imputed. The body
#'mass index (BMI) can be calculated within \code{mice} by specifying the
#'string \code{'~I(weight/height^2)'} as the univariate imputation method for
#'the target column \code{data$bmi}.  Note that the \code{~} mechanism works
#'only on those entries which have missing values in the target column. You
#'should make sure that the combined observed and imputed parts of the target
#'column make sense. An easy way to create consistency is by coding all entries
#'in the target as \code{NA}, but for large data sets, this could be
#'inefficient.  Note that you may also need to adapt the default
#'\code{predictorMatrix} to evade linear dependencies among the predictors that
#'could cause errors like \code{Error in solve.default()} or \code{Error:
#'system is exactly singular}. Though not strictly needed, it is often useful
#'to specify \code{visitSequence} such that the column that is imputed by the
#'\code{~} mechanism is visited each time after one of its predictors was
#'visited. In that way, deterministic relation between columns will always be
#'synchronized.
#'
#'@param data A data frame or a matrix containing the incomplete data.  Missing
#'values are coded as \code{NA}.
#'@param m Number of multiple imputations. The default is \code{m=5}.
#'@param where A data frame or matrix with logicals of the same dimensions 
#'as \code{data} indicating where in the data the imputations should be 
#'created. The default, \code{where = is.na(data)}, specifies that the
#'missing data should be imputed. The \code{where} argument may be used to 
#'overimpute observed data, or to skip imputations for selected missing values.
#'@param blocks List of vectors with variable names per block. List elements 
#'may be named to identify blocks. Variables within a block are 
#'imputed simultaneously by a joint imputation method
#'(see \code{method} argument). By default each variable has its 
#'own blocks, which is effectively
#'fully conditional specification (FCS) by univariate models 
#'(variable-by-variable imputation). Only variables whose names appear in 
#'\code{blocks} are imputed. A variable may appear in multiple blocks, 
#'so it is effectively re-imputed within the iteration.
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
#'@param visitSequence A vector of integers of arbitrary length, specifying the
#'column indices of the visiting sequence. The visiting sequence is the column
#'order that is used to impute the data during one pass through the data. A
#'column may be visited more than once. All incomplete columns that are used as
#'predictors should be visited, or else the function will stop with an error.
#'The default sequence \code{1:ncol(data)} implies that columns are imputed
#'from left to right. It is possible to specify one of the keywords
#'\code{'roman'} (left to right), \code{'arabic'} (right to left),
#'\code{'monotone'} (sorted in increasing amount of missingness) and
#'\code{'revmonotone'} (reverse of monotone). The keyword should be supplied as
#'a string and may be abbreviated.
#'@param post A vector of strings with length \code{length(blocks)}, specifying
#'expressions. Each string is parsed and executed within the \code{sampler()}
#'function to postprocess imputed values during the iterations. 
#'The default is a vector of empty strings 
#'(\code{vector("character", length(blocks))}), indicating no post-processing.
#'@param form A vector of strings with length \code{length(blocks)}, specifying
#'formulae. The \code{form} argument is an alternative to the 
#'\code{predictorMatrix} argument that allows for more flexibility in 
#'specifying imputation models, e.g., for specifying interaction terms. 
#'Each string is parsed and executed within the \code{sampler()}
#'function to create terms for the predictor. The default is to do nothing. 
#'When specified, the \code{form} argument takes precedence over the 
#'\code{predictMatrix} argument. Terms in the formulae should 
#'match variables names of the \code{data} argument.
#'@param defaultMethod A vector of length 4 containing the default
#'imputation methods for 1) numeric data, 2) factor data with 2 levels, 3) 
#'factor data with > 2 unordered levels, and 4) factor data with > 2 
#'ordered levels. By default, the method uses 
#'\code{pmm}, predictive mean matching (numeric data) \code{logreg}, logistic
#'regression imputation (binary data, factor with 2 levels) \code{polyreg},
#'polytomous regression imputation for unordered categorical data (factor > 2
#'levels) \code{polr}, proportional odds model for (ordered, > 2 levels).
#'@param maxit A scalar giving the number of iterations. The default is 5.
#'@param printFlag If \code{TRUE}, \code{mice} will print history on console.
#'Use \code{print=FALSE} for silent computation.
#'@param seed An integer that is used as argument by the \code{set.seed()} for
#'offsetting the random number generator. Default is to leave the random number
#'generator alone.
#'@param data.init A data frame of the same size and type as \code{data},
#'without missing data, used to initialize imputations before the start of the
#'iterative process.  The default \code{NULL} implies that starting imputation
#'are created by a simple random draw from the data. Note that specification of
#'\code{data.init} will start all \code{m} Gibbs sampling streams from the same
#'imputation.
#'@param ... Named arguments that are passed down to the univariate imputation
#'functions.
#'
#'@return Returns an S3 object of class \code{\link[=mids-class]{mids}}
#'        (multiply imputed data set)
#'@author Stef van Buuren \email{stef.vanbuuren@@tno.nl}, Karin
#'Groothuis-Oudshoorn \email{c.g.m.oudshoorn@@utwente.nl}, 2000-2010, with
#'contributions of Alexander Robitzsch, Gerko Vink, Shahab Jolani, 
#'Roel de Jong, Jason Turner, Lisa Doove, 
#'John Fox, Frank E. Harrell, and Peter Malewski.
#'@seealso \code{\link[=mids-class]{mids}}, \code{\link{with.mids}},
#'\code{\link{set.seed}}, \code{\link{complete}}
#'@references Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}:
#'Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#'Statistical Software}, \bold{45}(3), 1-67.
#'\url{http://www.jstatsoft.org/v45/i03/}
#'
#'van Buuren, S. (2012). \emph{Flexible Imputation of Missing Data.} Boca
#'Raton, FL: Chapman & Hall/CRC Press.
#'
#'Van Buuren, S., Brand, J.P.L., Groothuis-Oudshoorn C.G.M., Rubin, D.B. (2006)
#'Fully conditional specification in multivariate imputation.  \emph{Journal of
#'Statistical Computation and Simulation}, \bold{76}, 12, 1049--1064.
#'
#'Van Buuren, S. (2007) Multiple imputation of discrete and continuous data by
#'fully conditional specification.  \emph{Statistical Methods in Medical
#'Research}, \bold{16}, 3, 219--242.
#'
#'Van Buuren, S., Boshuizen, H.C., Knook, D.L. (1999) Multiple imputation of
#'missing blood pressure covariates in survival analysis.  \emph{Statistics in
#'Medicine}, \bold{18}, 681--694.
#'
#'Brand, J.P.L. (1999) \emph{Development, implementation and evaluation of
#'multiple imputation strategies for the statistical analysis of incomplete
#'data sets.} Dissertation. Rotterdam: Erasmus University.
#'@keywords iteration
#'@examples
#'
#'
#'# do default multiple imputation on a numeric matrix
#'imp <- mice(nhanes)
#'imp
#'
#'# list the actual imputations for BMI
#'imp$imp$bmi
#'
#'# first completed data matrix
#'complete(imp)
#'
#'
#'# imputation on mixed data with a different method per column
#'
#'mice(nhanes2, meth=c('sample','pmm','logreg','norm'))
#'
#'@export
mice <- function(data, m = 5, 
                 method = vector("character", length(blocks)),
                 predictorMatrix = matrix(1, nrow = length(blocks), ncol = ncol(data)),
                 where = is.na(data),
                 blocks = make.blocks(data),
                 visitSequence = NULL,
                 form = vector("character", length(blocks)),
                 post = vector("character", length(blocks)),
                 defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
                 maxit = 5, printFlag = TRUE, seed = NA,
                 data.init = NULL, ...) {
  
  # Error checks
  if (!(is.matrix(data) || is.data.frame(data)))
    stop("Data should be a matrix or data frame")
  if (ncol(data) < 2)
    stop("Data should contain at least two columns")
  if (!(is.matrix(where) || is.data.frame(where)))
    stop("Argument `where` not a matrix or data frame")
  if (!all(dim(data) == dim(where)))
    stop("Arguments `data` and `where` not of same size")

  # list for storing current computational state
  state <- list(it = 0, im = 0, dep = "", meth = "", log = FALSE)
  
  # data frame for storing the event log
  loggedEvents <- data.frame(it = 0, im = 0, dep = "", meth = "", out = "")
  
  # Initialize local variables
  call <- match.call()
  if (!is.na(seed)) set.seed(seed)
  data <- as.data.frame(data)
  setup <- list(blocks = blocks, 
                nwhere = apply(where, 2, sum),
                nimp = nimp(where, blocks),
                nmis = apply(is.na(data), 2, sum),
                visitSequence = visitSequence, 
                method = method,
                defaultMethod = defaultMethod,
                predictorMatrix = predictorMatrix,
                form = form, post = post, 
                nvar = ncol(data), 
                varnames = colnames(data))

  # Checks and edits on the arguments
  setup <- check.visitSequence(setup, where)
  setup <- check.method(setup, data)
  setup <- check.predictorMatrix(setup)
  setup <- check.data(setup, data, ...)
  setup <- check.form(setup)
  setup <- check.post(setup)
  
  ## Initialize imputation array imp, etc.
  imp <- initialize.imp(data, m, where, setup, data.init)
  
  # Iterate
  from <- 1
  to <- from + maxit - 1
  q <- sampler(data, m, where, imp, setup, c(from, to), printFlag, ...)
  
  if (!state$log) loggedEvents <- NULL
  if (state$log) row.names(loggedEvents) <- seq_len(nrow(loggedEvents))
  
  ## save, and return
  midsobj <- list(data = data, imp = q$imp, m = m,
                  where = where, blocks = setup$blocks, 
                  call = call, nmis = setup$nmis, 
                  method = setup$method,
                  predictorMatrix = setup$predictorMatrix,
                  visitSequence = setup$visitSequence, 
                  form = setup$form, post = setup$post, seed = seed, 
                  iteration = q$iteration,
                  lastSeedValue = .Random.seed, 
                  chainMean = q$chainMean,
                  chainVar = q$chainVar, 
                  loggedEvents = loggedEvents)
  oldClass(midsobj) <- "mids"
  return(midsobj)
}
