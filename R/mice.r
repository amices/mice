
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
#'Built-in elementary imputation methods are:
#'
#'\describe{
#'\item{pmm}{Predictive mean matching (any)}
#'\item{norm}{Bayesian linear regression (numeric)}
#'\item{norm.nob}{Linear regression ignoring model error (numeric)}
#'\item{norm.boot}{Linear regression using bootstrap (numeric)}
#'\item{norm.predict}{Linear regression, predicted values (numeric)}
#'\item{mean}{Unconditional mean imputation (numeric)}
#'\item{2l.norm}{Two-level normal imputation (numeric)}
#'\item{2l.pan}{Two-level normal imputation using pan (numeric)}
#'\item{2lonly.mean}{Imputation at level-2 of the class mean (numeric)}
#'\item{2lonly.norm}{Imputation at level-2 by Bayesian linear regression (numeric)}
#'\item{2lonly.pmm}{Imputation at level-2 by Predictive mean matching (any)}
#'\item{quadratic}{Imputation of quadratic terms (numeric)}
#'\item{logreg}{Logistic regression (factor, 2 levels)}
#'\item{logreg.boot}{Logistic regression with bootstrap}
#'\item{polyreg}{Polytomous logistic regression (factor, >= 2 levels)}
#'\item{polr}{Proportional odds model (ordered, >=2 levels)}
#'\item{lda}{Linear discriminant analysis (factor, >= 2 categories)}
#'\item{cart}{Classification and regression trees (any)}
#'\item{rf}{Random forest imputations (any)}
#'\item{ri}{Random indicator method for nonignorable data (numeric)}
#'\item{sample}{Random sample from the observed values (any)}
#'\item{fastpmm}{Experimental: Fast predictive mean matching using C++ (any)}
#'}
#'
#'
#'These corresponding functions are coded in the \code{mice} library under
#'names \code{mice.impute.method}, where \code{method} is a string with the
#'name of the elementary imputation method name, for example \code{norm}. The
#'\code{method} argument specifies the methods to be used.  For the \code{j}'th
#'column, \code{mice()} calls the first occurence of
#'\code{paste('mice.impute.',method[j],sep='')} in the search path.  The
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
#'first character of the string that specifies the elementary method.
#'\code{mice()} interprets the entire string, including the \code{~} character,
#'as the formula argument in a call to \code{model.frame(formula,
#'data[!r[,j],])}. This provides a simple mechanism for specifying determinstic
#'dependencies among the columns. For example, suppose that the missing entries
#'in variables \code{data$height} and \code{data$weight} are imputed. The body
#'mass index (BMI) can be calculated within \code{mice} by specifying the
#'string \code{'~I(weight/height^2)'} as the elementary imputation method for
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
#'length \code{ncol(data)}, specifying the elementary imputation method to be
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
#'ensure that the set of variables in the formula match those in \code{predictors}.
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
#'@param ... Named arguments that are passed down to the elementary imputation
#'functions.
#'
#'@return Returns an S3 object of class \code{\link[=mids-class]{mids}} (multiply imputed data set)
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
#'@import methods
#'@importFrom MASS eqscplot lda mvrnorm polr truehist
#'@importFrom nnet multinom nnet
#'@importFrom utils packageDescription
#'@importFrom graphics plot
#'@importFrom rpart rpart rpart.control
#'@importFrom Rcpp sourceCpp
#'@useDynLib mice
#'@examples
#'
#'
#'# do default multiple imputation on a numeric matrix
#'imp <- mice(nhanes)
#'imp
#'
#'# list the actual imputations for BMI
#'imp$imputations$bmi
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
mice <- function(data, m = 5, method = vector("character", length = ncol(data)), predictorMatrix = (1 - diag(1, ncol(data))),
    visitSequence = (1:ncol(data))[apply(is.na(data), 2, any)],
    form = vector("character", length = ncol(data)),
    post = vector("character", length = ncol(data)), defaultMethod = c("pmm",
        "logreg", "polyreg", "polr"), maxit = 5, diagnostics = TRUE, printFlag = TRUE, seed = NA, imputationMethod = NULL,
    defaultImputationMethod = NULL, data.init = NULL, ...)
{
    # MICE - Multivariate Imputation by Chained Equations
    #
    # Main algorithm for imputing datasets.  Authors: K. Groothuis-Oudshoorn and S. van Buuren TNO Quality of Life, Leiden
    # The Netherlands
    #
    # Copyright (c) 2010 TNO Quality of Life, Leiden
    #
    # ------------------------------CHECK.VISITSEQUENCE------------------------
    check.visitSequence <- function(setup) {
        nmis <- setup$nmis
        nvar <- setup$nvar
        visitSequence <- setup$visitSequence
        if (!is.numeric(visitSequence)) {
            code <- pmatch(visitSequence, c("roman", "arabic", "monotone", "revmonotone"))
            if (!is.na(code) && code == 1)
                visitSequence <- (1:nvar)[nmis > 0]
            if (!is.na(code) && code == 2)
                visitSequence <- rev((1:nvar)[nmis > 0])
            if (!is.na(code) && code == 3)
                visitSequence <- order(nmis)[(sum(nmis == 0) + 1):length(nmis)]  # SvB sept 2011
            if (!is.na(code) && code == 4)
                visitSequence <- rev(order(nmis)[(sum(nmis == 0) + 1):length(nmis)])
            if (is.na(code))
                stop("Argument visitSequence not recognized.\n")
        }
        if (all(nmis[visitSequence] == 0))
            stop(paste("No missing values found."))
        flags <- nmis == 0 & is.element(1:nvar, visitSequence)
        if (any(flags))
            visitSequence <- visitSequence[!flags]
        visitSequence <- visitSequence[visitSequence <= nvar]
        visitSequence <- visitSequence[visitSequence >= 1]
        if (length(visitSequence) == 0)
            stop(paste("No missing values found."))
        setup$visitSequence <- visitSequence
        return(setup)
    }

    ## ------------------------------CHECK.predictorMatrix-------------------------------
    check.predictorMatrix <- function(setup) {
        ## checks the predictorMatrix makes consistency edits of the predictormatrix
        pred <- setup$predictorMatrix
        varnames <- setup$varnames
        nmis <- setup$nmis
        nvar <- setup$nvar
        vis <- setup$visitSequence
        method <- setup$method
        post <- setup$post

        if (!is.matrix(pred))
            stop("Argument 'predictorMatrix' not a matrix.")
        if (nvar != nrow(pred) | nvar != ncol(pred))
            stop(paste("The predictorMatrix has", nrow(pred), "rows and", ncol(pred), "columns. Both should be", nvar, "."))
        dimnames(pred) <- list(varnames, varnames)
        diag(pred) <- 0
        
        ## stop if there is a class variable with missing data # SvB 25apr13
        isclassvar <- apply(pred == -2, 2, any)
        
        for (j in 1:nvar) {
            if (method[j] == "" & any(pred[, j] != 0) & nmis[j] > 0) {
                out <- varnames[j]
                updateLog(out = out)
                pred[, j] <- 0
                vis <- vis[vis != j]
                post[j] <- ""
                if (isclassvar[j]) stop("Removed an incomplete class variable.")  ## SvB 25apr13
            }
            if (nmis[j] == 0 & any(pred[j, ] != 0))
                pred[j, ] <- 0
        }

        setup$predictorMatrix <- pred
        setup$visitSequence <- vis
        setup$post <- post
        return(setup)
    }

    ## ------------------------------CHECK.method-------------------------------

    check.method <- function(setup, data) {
        ## check method, set defaults if appropriate
        method <- setup$method
        defaultMethod <- setup$defaultMethod
        visitSequence <- setup$visitSequence
        nmis <- setup$nmis
        nvar <- setup$nvar

        if (all(method == "")) {
            # the default argument
            for (j in visitSequence) {
                y <- data[, j]
                if (is.numeric(y))
                  method[j] <- defaultMethod[1] else if (nlevels(y) == 2)
                  method[j] <- defaultMethod[2] else if (is.ordered(y) & nlevels(y) > 2)
                  method[j] <- defaultMethod[4] else if (nlevels(y) > 2)
                  method[j] <- defaultMethod[3] else if (is.logical(y))
                  method[j] <- defaultMethod[2]  # SvB 18/1/2010
 else method[j] <- defaultMethod[1]
            }
        }
        ## expand user's imputation method to all visited columns
        ##
        ## single string supplied by user (note implicit assumption of two columns)
        if (length(method) == 1) {
            if (is.passive(method))
                stop("Cannot have a passive imputation method for every column.")
            method <- rep(method, nvar)
        }
        ## if user specifies multiple methods, check the length of the argument
        if (length(method) != nvar)
            stop(paste("The length of method (", length(method), ") does not match the number of columns in the data (", nvar,
                ").", sep = ""))
        ## check whether the elementary imputation methods are actually available on the search path
        active <- !is.passive(method) & nmis > 0 & !(method == "")
        ## BEGIN patch by Gerko Vink, 22sep2011
        passive.check <- is.passive(method) & nmis > 0 & !(method == "")
        check <- all(active == FALSE) & any(passive.check != FALSE)
        if (check)
            fullNames <- rep("mice.impute.passive", length(method[passive.check])) else fullNames <- paste("mice.impute", method[active], sep = ".")
        ## END patch
        notFound <- !sapply(fullNames, exists, mode = "function", inherit = TRUE)  ## SVB 6 Feb 2004
        if (any(notFound))
            stop(paste("The following functions were not found:", paste(fullNames[notFound], collapse = ", ")))
        ## type checks on built-in imputation methods Added SvB June 2009

        for (j in visitSequence) {
            y <- data[, j]
            vname <- dimnames(data)[[2]][j]
            mj <- method[j]
            mlist <- list(m1 = c("logreg", "logreg.boot", "polyreg", "lda", "polr"), 
                          m2 = c("norm", "norm.nob", "norm.predict", "norm.boot", "mean", 
                                 "2l.norm", "2L.norm", "2l.pan", "2L.pan", "2lonly.pan", "quadratic", "ri"), 
                          m3 = c("norm", "norm.nob", "norm.predict", "norm.boot", "mean", 
                                 "2l.norm", "2L.norm", "2l.pan", "2L.pan", "2lonly.pan", "quadratic", "logreg", "logreg.boot"))
            
            if (is.numeric(y) & (mj %in% mlist$m1)) 
                warning("Type mismatch for variable ", vname, "\nImputation method ", mj, " is for categorical data.", "\nIf you want that, turn variable ", 
                  vname, " into a factor,", "\nand store your data in a data frame.", call. = FALSE) else if (is.factor(y) & nlevels(y) == 2 & (mj %in% mlist$m2)) 
                warning("Type mismatch for variable ", vname, "\nImputation method ", mj, " is not for factors.", call. = FALSE) else if (is.factor(y) & nlevels(y) > 2 & (mj %in% mlist$m3)) 
                warning("Type mismatch for variable ", vname, "\nImputation method ", mj, " is not for factors with three or more levels.", 
                  call. = FALSE)
        }

        setup$method <- method
        return(setup)
    }
    ## ------------------------------CHECK.data-------------------------------

    check.data <- function(setup, data, allow.na = FALSE, ...) {

        pred <- setup$predictorMatrix
        nvar <- setup$nvar
        varnames <- setup$varnames
        meth <- setup$method
        vis <- setup$visitSequence
        
        ## stop if the class variable is a factor 25apr2013
        isclassvar <- apply(pred == -2, 2, any)
        for (j in 1:nvar) {
            if (isclassvar[j] & is.factor(data[,j])) 
                stop("Class variable (column ", j,
                     ") cannot be factor. Convert to numeric by as.integer()")        
        }
        ## remove constant variables but leave passive variables untouched
        for (j in 1:nvar) {
            if (!is.passive(meth[j])) {
                d.j <- data[,j]
                v <- if(is.character(d.j)) NA else var(as.numeric(d.j), na.rm = TRUE)
                constant <- if (allow.na) {
                    if(is.na(v)) FALSE  # SvB 10/3/2011
                    else (v < 1000 * .Machine$double.eps)  # SvB 10/3/2011
                } else is.na(v) || v < 1000 * .Machine$double.eps
                didlog <- FALSE
                if (constant && any(pred[, j] != 0)) {
                  out <- varnames[j]
                  pred[, j] <- 0
                  updateLog(out = out, meth = "constant")
                  didlog <- TRUE
                }
                if (constant && meth[j] != "") {
                  out <- varnames[j]
                  pred[j, ] <- 0
                  if (!didlog)
                    updateLog(out = out, meth = "constant")
                  meth[j] <- ""
                  vis <- vis[vis != j]
                  post[j] <- ""
                }
            }
        }

        ## remove collinear variables
        ispredictor <- apply(pred != 0, 2, any)  # SvB 16/3/11
        if (any(ispredictor))
            droplist <- find.collinear(data[, ispredictor, drop = FALSE], ...) else droplist <- NULL
        if (length(droplist) > 0) {
            for (k in 1:length(droplist)) {
                j <- which(varnames %in% droplist[k])
                didlog <- FALSE
                if (any(pred[, j] != 0)) {
                  # remove as predictor
                  out <- varnames[j]
                  pred[, j] <- 0
                  updateLog(out = out, meth = "collinear")
                  didlog <- TRUE
                }
                if (meth[j] != "") {
                  out <- varnames[j]
                  pred[j, ] <- 0
                  if (!didlog)
                    updateLog(out = out, meth = "collinear")
                  meth[j] <- ""
                  vis <- vis[vis != j]
                  post[j] <- ""
                }
            }
        }

        setup$predictorMatrix <- pred
        setup$visitSequence <- vis
        setup$post <- post
        setup$meth <- meth
        return(setup)
    }

    ## Start with some preliminary calculations and error checks
    call <- match.call()
    if (!is.na(seed))
        set.seed(seed)  ## FEH 1apr02
    if (!(is.matrix(data) | is.data.frame(data)))
        stop("Data should be a matrix or data frame")
    if ((nvar <- ncol(data)) < 2)
        stop("Data should contain at least two columns")
    data <- as.data.frame(data)
    nmis <- apply(is.na(data), 2, sum)
    if (sum(nmis) == 0)
        stop("No missing values found")
    varnames <- dimnames(data)[[2]]

    ## list for storing current computational state
    state <- list(it = 0, im = 0, co = 0, dep = "", meth = "", log = FALSE)

    ## data frame for storing the event log
    loggedEvents <- data.frame(it = 0, im = 0, co = 0, dep = "", meth = "", out = "")

    ## Perform various validity checks on the specified arguments
    if (!is.null(imputationMethod))
        method <- imputationMethod
    if (!is.null(defaultImputationMethod))
        defaultMethod <- defaultImputationMethod

    setup <- list(visitSequence = visitSequence, method = method,
                  defaultMethod = defaultMethod,
                  predictorMatrix = predictorMatrix,
                  form = form,
        post = post, nvar = nvar, nmis = nmis, varnames = varnames)
    setup <- check.visitSequence(setup)
    setup <- check.method(setup, data)
    setup <- check.predictorMatrix(setup)
    setup <- check.data(setup, data, ...)

    ## Pad the imputation model with dummy variables for the factors
    method <- setup$method
    predictorMatrix <- setup$predictorMatrix
    visitSequence <- setup$visitSequence
    post <- setup$post
    p <- padModel(data, method, predictorMatrix, visitSequence, form, post, nmis, nvar)
    if (sum(duplicated(names(p$data))) > 0)
        stop("Column names of padded data should be unique")

    ## Initialize response matrix r, imputation array imp, as well as some other stuff
    r <- (!is.na(p$data))
    imp <- vector("list", ncol(p$data))
    if (m > 0) {

        ## Initializes the imputed values
        for (j in visitSequence) {
            imp[[j]] <- as.data.frame(matrix(NA, nrow = sum(!r[, j]), ncol = m))
            dimnames(imp[[j]]) <- list(row.names(data)[r[, j] == FALSE], 1:m)
            y <- data[, j]
            ry <- r[, j]
            if (method[j] != "") {
                # for incomplete variables that are imputed
                for (i in 1:m) {
                  if (nmis[j] < nrow(data)) {
                    # begin 25jun2012
                    if (is.null(data.init)) {
                      imp[[j]][, i] <- mice.impute.sample(y, ry, ...)
                    } else {
                      imp[[j]][, i] <- data.init[!ry, j]
                    }
                    # end 25jun2012
                  } else imp[[j]][, i] <- rnorm(nrow(data))
                }
            }
        }
    }

    # OK. Iterate.
    from <- 1
    to <- from + maxit - 1
    q <- sampler(p, data, m, imp, r, visitSequence, c(from, to), printFlag, ...)

    ## restore the original NA's in the data
    for (j in p$visitSequence) p$data[(!r[, j]), j] <- NA

    ## delete data and imputations of automatic dummy variables
    imp <- q$imp[1:nvar]
    names(imp) <- varnames
    names(method) <- varnames
    names(form) <- varnames
    names(post) <- varnames
    names(visitSequence) <- varnames[visitSequence]
    if (!state$log)
        loggedEvents <- NULL
    if (state$log)
        row.names(loggedEvents) <- 1:nrow(loggedEvents)

    ## save, and return
    midsobj <- list(call = call, data = as.data.frame(p$data[, 1:nvar]), m = m, nmis = nmis, imp = imp, method = method, predictorMatrix = predictorMatrix,
        visitSequence = visitSequence, form = form, post = post, seed = seed, iteration = q$iteration, lastSeedValue = .Random.seed, chainMean = q$chainMean,
        chainVar = q$chainVar, loggedEvents = loggedEvents)
    if (diagnostics)
        midsobj <- c(midsobj, list(pad = p))
    oldClass(midsobj) <- "mids"
    return(midsobj)
}  # END OF MICE FUNCTION
