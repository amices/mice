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
#'@param method Can be either a single string, or a vector of strings with
#'length \code{ncol(data)}, specifying the univariate imputation method to be
#'used for each column in data. If specified as a single string, the same
#'method will be used for all columns.  The default imputation method (when no
#'argument is specified) depends on the measurement level of the target column
#'and are specified by the \code{defaultMethod} argument.  Columns that need
#'not be imputed have the empty method \code{''}.  See details for more
#'information.
#'@param predictorMatrix A square matrix of size \code{ncol(data)} containing
#'0/1 data specifying the set of predictors to be used for each target column.
#'Rows correspond to target variables (i.e. variables to be imputed), in the
#'sequence as they appear in data. A value of '1' means that the column
#'variable is used as a predictor for the target variable (in the rows). The
#'diagonal of \code{predictorMatrix} must be zero. The default for
#'\code{predictorMatrix} is that all other columns are used as predictors
#'(sometimes called massive imputation). Note: For two-level imputation codes
#''2' and '-2' are also allowed.
#'@param where A data frame or matrix with logicals of the same dimensions 
#'as \code{data} indicating where in the data the imputations should be 
#'created. The default, \code{where = is.na(data)}, specifies that the
#'missing data should be imputed. The \code{where} argument may be used to 
#'overimpute observed data, or to skip imputations for selected missing values.
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
#'@param post A vector of strings with length \code{ncol(data)}, specifying
#'expressions. Each string is parsed and executed within the \code{sampler()}
#'function to postprocess imputed values.  The default is to do nothing,
#'indicated by a vector of empty strings \code{''}.
#'@param form A vector of strings with length \code{ncol(data)}, specifying
#'formulae. Each string is parsed and executed within the \code{sampler()}
#'function to create terms for the predictor.  The default is to do nothing,
#'indicated by a vector of empty strings \code{''}.  The main value
#'lies in the easy specification of interaction terms.  The user must
#'ensure that the set of variables in the formula match those in
#'\code{predictors}.
#'@param defaultMethod A vector of three strings containing the default
#'imputation methods for numerical columns, factor columns with 2 levels, and
#'columns with (unordered or ordered) factors with more than two levels,
#'respectively. If nothing is specified, the following defaults will be used:
#'\code{pmm}, predictive mean matching (numeric data) \code{logreg}, logistic
#'regression imputation (binary data, factor with 2 levels) \code{polyreg},
#'polytomous regression imputation for unordered categorical data (factor >= 2
#'levels) \code{polr}, proportional odds model for (ordered, >= 2 levels)
#'@param maxit A scalar giving the number of iterations. The default is 5.
#'@param diagnostics A Boolean flag. If \code{TRUE}, diagnostic information
#'will be appended to the value of the function. If \code{FALSE}, only the
#'imputed data are saved. The default is \code{TRUE}.
#'@param printFlag If \code{TRUE}, \code{mice} will print history on console.
#'Use \code{print=FALSE} for silent computation.
#'@param seed An integer that is used as argument by the \code{set.seed()} for
#'offsetting the random number generator. Default is to leave the random number
#'generator alone.
#'@param imputationMethod Same as \code{method} argument. Included for
#'backwards compatibility.
#'@param defaultImputationMethod Same as \code{defaultMethod} argument.
#'Included for backwards compatibility.
#'@param data.init A data frame of the same size and type as \code{data},
#'without missing data, used to initialize imputations before the start of the
#'iterative process.  The default \code{NULL} implies that starting imputation
#'are created by a simple random draw from the data. Note that specification of
#'\code{data.init} will start the \code{m} Gibbs sampling streams from the same
#'imputations.
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
                 method = vector("character", length = ncol(data)),
                 predictorMatrix = (1 - diag(1, ncol(data))),
                 where = is.na(data),
                 visitSequence = NULL,
                 form = vector("character", length = ncol(data)),
                 post = vector("character", length = ncol(data)),
                 defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
                 maxit = 5, diagnostics = TRUE, printFlag = TRUE, seed = NA,
                 imputationMethod = NULL, defaultImputationMethod = NULL,
                 data.init = NULL, ...) {
  
  # Start with some preliminary calculations and error checks
  call <- match.call()
  if (!is.na(seed)) 
    set.seed(seed)
  if (!(is.matrix(data) || is.data.frame(data)))
    stop("Data should be a matrix or data frame")
  nvar <- ncol(data)
  if (nvar < 2)
    stop("Data should contain at least two columns")
  data <- as.data.frame(data)
  nmis <- apply(is.na(data), 2, sum)
  varnames <- colnames(data)
  
  if (!(is.matrix(where) || is.data.frame(where)))
    stop("Argument `where` not a matrix or data frame")
  if (!all(dim(data) == dim(where)))
    stop("Arguments `data` and `where` not of same size")
  nwhere <- apply(where, 2, sum)
  #if (sum(where) == 0)
  #  stop("No locations to impute")
  #if (any(is.na(data) & !where))
  #  stop("Found where == FALSE for some missing values. Not supported. ")
  
  # list for storing current computational state
  state <- list(it = 0, im = 0, co = 0, dep = "", meth = "", log = FALSE)
  
  # data frame for storing the event log
  loggedEvents <- data.frame(it = 0, im = 0, co = 0, dep = "", meth = "",
                             out = "")
  
  # Legacy handling
  if (!is.null(imputationMethod))
    method <- imputationMethod
  if (!is.null(defaultImputationMethod))
    defaultMethod <- defaultImputationMethod
  
  # Perform various validity checks on the specified arguments
  setup <- list(visitSequence = visitSequence, method = method,
                defaultMethod = defaultMethod,
                predictorMatrix = predictorMatrix,
                form = form, post = post, nvar = nvar,
                nmis = nmis, nwhere = nwhere,
                varnames = varnames)
  setup <- check.visitSequence(setup, where)
  setup <- check.method(setup, data)
  setup <- check.predictorMatrix(setup)
  setup <- check.data(setup, data, ...)
  
  ## Pad the imputation model with dummy variables for the factors
  method <- setup$method
  predictorMatrix <- setup$predictorMatrix
  visitSequence <- setup$visitSequence
  post <- setup$post
  p <- padModel(data, method, predictorMatrix, visitSequence, 
                form, post, nvar)
  
  ## Initialize where matrix r, imputation array imp, etc. 
  r <- (!is.na(p$data))
  imp <- vector("list", ncol(p$data))
  if (m > 0) {
    ## Initializes the imputed values
    for (j in visitSequence) {
      y <- data[, j]
      ry <- r[, j]
      wy <- where[, j]
      imp[[j]] <- as.data.frame(matrix(NA, nrow = sum(wy), ncol = m))
      dimnames(imp[[j]]) <- list(row.names(data)[wy], 1:m)
      if (method[j] != "") {
        # for incomplete variables that are imputed
        for (i in seq_len(m)) {
          if (nmis[j] < nrow(data)) {
            if (is.null(data.init)) {
              imp[[j]][, i] <- mice.impute.sample(y, ry, wy = wy, ...)
            } else {
              imp[[j]][, i] <- data.init[wy, j]
            }
          } else imp[[j]][, i] <- rnorm(nrow(data))
        }
      }
    }
  }
  
  # Iterate.
  from <- 1
  to <- from + maxit - 1
  q <- sampler(p, data, where, m, imp, r, visitSequence, c(from, to), printFlag, ...)
  
  ## restore the original NA's in the data
  for (j in p$visitSequence) {
    p$data[!r[, j], j] <- NA
  }

  ## delete data and imputations of automatic dummy variables
  imp <- q$imp[seq_len(nvar)]
  names(imp) <- varnames
  names(method) <- varnames
  names(form) <- varnames
  names(post) <- varnames
  names(visitSequence) <- varnames[visitSequence]
  if (!state$log)
    loggedEvents <- NULL
  if (state$log)
    row.names(loggedEvents) <- seq_len(nrow(loggedEvents))
  
  ## save, and return
  midsobj <- list(call = call, data = as.data.frame(p$data[, seq_len(nvar)]), 
                  where = where, m = m,
                  nmis = nmis, imp = imp, method = method,
                  predictorMatrix = predictorMatrix,
                  visitSequence = visitSequence, form = form, post = post,
                  seed = seed, iteration = q$iteration,
                  lastSeedValue = .Random.seed, chainMean = q$chainMean,
                  chainVar = q$chainVar, loggedEvents = loggedEvents,
                  pad = p)
  if (!diagnostics)
    midsobj$pad <- NULL
  oldClass(midsobj) <- "mids"
  return(midsobj)
}

