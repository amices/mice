#' Multivariate Imputation by Chained Equations (MICE)
#'
#' Generates Multivariate Imputations by Chained Equations (MICE)
#'
#' Generates multiple imputations for incomplete multivariate data by Gibbs
#' sampling. Missing data can occur anywhere in the data. The algorithm imputes
#' an incomplete column (the target column) by generating 'plausible' synthetic
#' values given other columns in the data. Each incomplete column must act as a
#' target column, and has its own specific set of predictors. The default set of
#' predictors for a given target consists of all other columns in the data. For
#' predictors that are incomplete themselves, the most recently generated
#' imputations are used to complete the predictors prior to imputation of the
#' target column.
#'
#' A separate univariate imputation model can be specified for each column. The
#' default imputation method depends on the measurement level of the target
#' column. In addition to these, several other methods are provided. You can
#' also write their own imputation functions, and call these from within the
#' algorithm.
#'
#' The data may contain categorical variables that are used in a regressions on
#' other variables. The algorithm creates dummy variables for the categories of
#' these variables, and imputes these from the corresponding categorical
#' variable.
#'
#' Built-in univariate imputation methods are:
#'
#' \tabular{lll}{
#' \code{pmm}               \tab any     \tab Predictive mean matching\cr
#' \code{midastouch}        \tab any     \tab Weighted predictive mean matching\cr
#' \code{sample}            \tab any     \tab Random sample from observed values\cr
#' \code{cart}              \tab any     \tab Classification and regression trees\cr
#' \code{rf}                \tab any     \tab Random forest imputations\cr
#' \code{mean}              \tab numeric \tab Unconditional mean imputation\cr
#' \code{norm}              \tab numeric \tab Bayesian linear regression\cr
#' \code{norm.nob}          \tab numeric \tab Linear regression ignoring model error\cr
#' \code{norm.boot}         \tab numeric \tab Linear regression using bootstrap\cr
#' \code{norm.predict}      \tab numeric \tab Linear regression, predicted values\cr
#' \code{lasso.norm}        \tab numeric \tab Lasso linear regression\cr
#' \code{lasso.select.norm} \tab numeric \tab Lasso select + linear regression\cr
#' \code{quadratic}         \tab numeric \tab Imputation of quadratic terms\cr
#' \code{ri}                \tab numeric \tab Random indicator for nonignorable data\cr
#' \code{logreg}            \tab binary  \tab Logistic regression\cr
#' \code{logreg.boot}       \tab binary  \tab Logistic regression with bootstrap\cr
#' \code{lasso.logreg}      \tab binary  \tab Lasso logistic regression\cr
#' \code{lasso.select.logreg}\tab binary  \tab Lasso select + logistic regression\cr
#' \code{polr}              \tab ordered \tab Proportional odds model\cr
#' \code{polyreg}           \tab unordered\tab Polytomous logistic regression\cr
#' \code{lda}               \tab unordered\tab Linear discriminant analysis\cr
#' \code{2l.norm}           \tab numeric  \tab Level-1 normal heteroscedastic\cr
#' \code{2l.lmer}           \tab numeric  \tab Level-1 normal homoscedastic, lmer\cr
#' \code{2l.pan}            \tab numeric  \tab Level-1 normal homoscedastic, pan\cr
#' \code{2l.bin}            \tab binary   \tab Level-1 logistic, glmer\cr
#' \code{2lonly.mean}       \tab numeric  \tab Level-2 class mean\cr
#' \code{2lonly.norm}       \tab numeric  \tab Level-2 class normal\cr
#' \code{2lonly.pmm}        \tab any      \tab Level-2 class predictive mean matching
#' }
#'
#' These corresponding functions are coded in the \code{mice} library under
#' names \code{mice.impute.method}, where \code{method} is a string with the
#' name of the univariate imputation method name, for example \code{norm}. The
#' \code{method} argument specifies the methods to be used.  For the \code{j}'th
#' column, \code{mice()} calls the first occurrence of
#' \code{paste('mice.impute.', method[j], sep = '')} in the search path.  The
#' mechanism allows uses to write customized imputation function,
#' \code{mice.impute.myfunc}. To call it for all columns specify
#' \code{method='myfunc'}.  To call it only for, say, column 2 specify
#' \code{method=c('norm','myfunc','logreg',\dots{})}.
#'
#' \emph{Skipping imputation:} The user may skip imputation of a column by
#' setting its entry to the empty method: \code{""}. For complete columns without
#' missing data \code{mice} will automatically set the empty method. Setting t
#' he empty method does not produce imputations for the column, so any missing
#' cells remain \code{NA}. If column A contains \code{NA}'s and is used as
#' predictor in the imputation model for column B, then \code{mice} produces no
#' imputations for the rows in B where A is missing. The imputed data
#' for B may thus contain \code{NA}'s. The remedy is to remove column A from
#' the imputation model for the other columns in the data. This can be done
#' by setting the entire column for variable A in the \code{predictorMatrix}
#' equal to zero.
#'
#' \emph{Passive imputation:} \code{mice()} supports a special built-in method,
#' called passive imputation. This method can be used to ensure that a data
#' transform always depends on the most recently generated imputations.  In some
#' cases, an imputation model may need transformed data in addition to the
#' original data (e.g. log, quadratic, recodes, interaction, sum scores, and so
#' on).
#'
#' Passive imputation maintains consistency among different transformations of
#' the same data. Passive imputation is invoked if \code{~} is specified as the
#' first character of the string that specifies the univariate method.
#' \code{mice()} interprets the entire string, including the \code{~} character,
#' as the formula argument in a call to \code{model.frame(formula,
#' data[!r[,j],])}. This provides a simple mechanism for specifying deterministic
#' dependencies among the columns. For example, suppose that the missing entries
#' in variables \code{data$height} and \code{data$weight} are imputed. The body
#' mass index (BMI) can be calculated within \code{mice} by specifying the
#' string \code{'~I(weight/height^2)'} as the univariate imputation method for
#' the target column \code{data$bmi}.  Note that the \code{~} mechanism works
#' only on those entries which have missing values in the target column. You
#' should make sure that the combined observed and imputed parts of the target
#' column make sense. An easy way to create consistency is by coding all entries
#' in the target as \code{NA}, but for large data sets, this could be
#' inefficient.  Note that you may also need to adapt the default
#' \code{predictorMatrix} to evade linear dependencies among the predictors that
#' could cause errors like \code{Error in solve.default()} or \code{Error:
#' system is exactly singular}. Though not strictly needed, it is often useful
#' to specify \code{visitSequence} such that the column that is imputed by the
#' \code{~} mechanism is visited each time after one of its predictors was
#' visited. In that way, deterministic relation between columns will always be
#' synchronized.
#'
#' A new argument \code{ls.meth} can be parsed to the lower level
#' \code{.norm.draw} to specify the method for generating the least squares
#' estimates and any subsequently derived estimates. Argument \code{ls.meth}
#' takes one of three inputs: \code{"qr"} for QR-decomposition, \code{"svd"} for
#' singular value decomposition and \code{"ridge"} for ridge regression.
#' \code{ls.meth} defaults to \code{ls.meth = "qr"}.
#'
#' \emph{Auxiliary predictors in formulas specification: }
#' For a given block, the \code{formulas} specification takes precedence over
#' the corresponding row in the \code{predictMatrix} argument. This
#' precedence is, however, restricted to the subset of variables
#' specified in the terms of the block formula. Any
#' variables not specified by \code{formulas} are imputed
#' according to the \code{predictMatrix} specification. Variables with
#' non-zero \code{type} values in the \code{predictMatrix} will
#' be added as main effects to the \code{formulas}, which will
#' act as supplementary covariates in the imputation model. It is possible
#' to turn off this behavior by specifying the
#' argument \code{auxiliary = FALSE}.
#'
#' @param data A data frame or a matrix containing the incomplete data.  Missing
#' values are coded as \code{NA}.
#' @param m Number of multiple imputations. The default is \code{m=5}.
#' @param method Can be either a single string, or a vector of strings with
#' length \code{length(blocks)}, specifying the imputation method to be
#' used for each column in data. If specified as a single string, the same
#' method will be used for all blocks. The default imputation method (when no
#' argument is specified) depends on the measurement level of the target column,
#' as regulated by the \code{defaultMethod} argument. Columns that need
#' not be imputed have the empty method \code{""}. See details.
#' @param predictorMatrix A numeric matrix of \code{length(blocks)} rows
#' and \code{ncol(data)} columns, containing 0/1 data specifying
#' the set of predictors to be used for each target column.
#' Each row corresponds to a variable block, i.e., a set of variables
#' to be imputed. A value of \code{1} means that the column
#' variable is used as a predictor for the target block (in the rows).
#' By default, the \code{predictorMatrix} is a square matrix of \code{ncol(data)}
#' rows and columns with all 1's, except for the diagonal.
#' Note: For two-level imputation models (which have \code{"2l"} in their names)
#' other codes (e.g, \code{2} or \code{-2}) are also allowed.
#' @param ignore A logical vector of \code{nrow(data)} elements indicating
#' which rows are ignored when creating the imputation model. The default
#' \code{NULL} includes all rows that have an observed value of the variable
#' to imputed. Rows with \code{ignore} set to \code{TRUE} do not influence the
#' parameters of the imputation model, but are still imputed. We may use the
#' \code{ignore} argument to split \code{data} into a training set (on which the
#' imputation model is built) and a test set (that does not influence the
#' imputation model estimates).
#' Note: Multivariate imputation methods, like \code{mice.impute.jomoImpute()}
#' or \code{mice.impute.panImpute()}, do not honour the \code{ignore} argument.
#' @param where A data frame or matrix with logicals of the same dimensions
#' as \code{data} indicating where in the data the imputations should be
#' created. The default, \code{where = is.na(data)}, specifies that the
#' missing data should be imputed. The \code{where} argument may be used to
#' overimpute observed data, or to skip imputations for selected missing values.
#' Note: Imputation methods that generate imptutations outside of
#' \code{mice}, like \code{mice.impute.panImpute()} may depend on a complete
#' predictor space. In that case, a custom \code{where} matrix can not be
#' specified.
#' @param blocks List of vectors with variable names per block. List elements
#' may be named to identify blocks. Variables within a block are
#' imputed by a multivariate imputation method
#' (see \code{method} argument). By default each variable is placed
#' into its own block, which is effectively
#' fully conditional specification (FCS) by univariate models
#' (variable-by-variable imputation). Only variables whose names appear in
#' \code{blocks} are imputed. The relevant columns in the \code{where}
#' matrix are set to \code{FALSE} of variables that are not block members.
#' A variable may appear in multiple blocks. In that case, it is
#' effectively re-imputed each time that it is visited.
#' @param visitSequence A vector of block names of arbitrary length, specifying the
#' sequence of blocks that are imputed during one iteration of the Gibbs
#' sampler. A block is a collection of variables. All variables that are
#' members of the same block are imputed
#' when the block is visited. A variable that is a member of multiple blocks
#' is re-imputed within the same iteration.
#' The default \code{visitSequence = "roman"} visits the blocks (left to right)
#' in the order in which they appear in \code{blocks}.
#' One may also use one of the following keywords: \code{"arabic"}
#' (right to left), \code{"monotone"} (ordered low to high proportion
#' of missing data) and \code{"revmonotone"} (reverse of monotone).
#' \emph{Special case}: If you specify both \code{visitSequence = "monotone"} and
#' \code{maxit = 1}, then the procedure will edit the \code{predictorMatrix}
#' to conform to the monotone pattern. Realize that convergence in one
#' iteration is only guaranteed if the missing data pattern is actually
#' monotone. The procedure does not check this.
#' @param formulas A named list of formula's, or expressions that
#' can be converted into formula's by \code{as.formula}. List elements
#' correspond to blocks. The block to which the list element applies is
#' identified by its name, so list names must correspond to block names.
#' The \code{formulas} argument is an alternative to the
#' \code{predictorMatrix} argument that allows for more flexibility in
#' specifying imputation models, e.g., for specifying interaction terms.
#' @param blots A named \code{list} of \code{alist}'s that can be used
#' to pass down arguments to lower level imputation function. The entries
#' of element \code{blots[[blockname]]} are passed down to the function
#' called for block \code{blockname}.
#' @param post A vector of strings with length \code{ncol(data)} specifying
#' expressions as strings. Each string is parsed and
#' executed within the \code{sampler()} function to post-process
#' imputed values during the iterations.
#' The default is a vector of empty strings, indicating no post-processing.
#' Multivariate (block) imputation methods ignore the \code{post} parameter.
#' @param defaultMethod A vector of length 4 containing the default
#' imputation methods for 1) numeric data, 2) factor data with 2 levels, 3)
#' factor data with > 2 unordered levels, and 4) factor data with > 2
#' ordered levels. By default, the method uses
#' \code{pmm}, predictive mean matching (numeric data) \code{logreg}, logistic
#' regression imputation (binary data, factor with 2 levels) \code{polyreg},
#' polytomous regression imputation for unordered categorical data (factor > 2
#' levels) \code{polr}, proportional odds model for (ordered, > 2 levels).
#' @param maxit A scalar giving the number of iterations. The default is 5.
#' @param printFlag If \code{TRUE}, \code{mice} will print history on console.
#' Use \code{print=FALSE} for silent computation.
#' @param seed An integer that is used as argument by the \code{set.seed()} for
#' offsetting the random number generator. Default is to leave the random number
#' generator alone.
#' @param data.init A data frame of the same size and type as \code{data},
#' without missing data, used to initialize imputations before the start of the
#' iterative process.  The default \code{NULL} implies that starting imputation
#' are created by a simple random draw from the data. Note that specification of
#' \code{data.init} will start all \code{m} Gibbs sampling streams from the same
#' imputation.
#' @param \dots Named arguments that are passed down to the univariate imputation
#' functions.
#'
#' @return Returns an S3 object of class \code{\link[=mids-class]{mids}}
#'        (multiply imputed data set)
#' @author Stef van Buuren \email{stef.vanbuuren@@tno.nl}, Karin
#' Groothuis-Oudshoorn \email{c.g.m.oudshoorn@@utwente.nl}, 2000-2010, with
#' contributions of Alexander Robitzsch, Gerko Vink, Shahab Jolani,
#' Roel de Jong, Jason Turner, Lisa Doove,
#' John Fox, Frank E. Harrell, and Peter Malewski.
#' @seealso \code{\link[=mids-class]{mids}}, \code{\link{with.mids}},
#' \code{\link{set.seed}}, \code{\link{complete}}
#' @references Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}:
#' Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#' Statistical Software}, \bold{45}(3), 1-67.
#' \doi{10.18637/jss.v045.i03}
#'
#' Van Buuren, S. (2018).
#' \href{https://stefvanbuuren.name/fimd/sec-FCS.html#sec:MICE}{\emph{Flexible Imputation of Missing Data. Second Edition.}}
#' Chapman & Hall/CRC. Boca Raton, FL.
#'
#' Van Buuren, S., Brand, J.P.L., Groothuis-Oudshoorn C.G.M., Rubin, D.B. (2006)
#' Fully conditional specification in multivariate imputation.  \emph{Journal of
#' Statistical Computation and Simulation}, \bold{76}, 12, 1049--1064.
#'
#' Van Buuren, S. (2007) Multiple imputation of discrete and continuous data by
#' fully conditional specification.  \emph{Statistical Methods in Medical
#' Research}, \bold{16}, 3, 219--242.
#'
#' Van Buuren, S., Boshuizen, H.C., Knook, D.L. (1999) Multiple imputation of
#' missing blood pressure covariates in survival analysis.  \emph{Statistics in
#' Medicine}, \bold{18}, 681--694.
#'
#' Brand, J.P.L. (1999) \emph{Development, implementation and evaluation of
#' multiple imputation strategies for the statistical analysis of incomplete
#' data sets.} Dissertation. Rotterdam: Erasmus University.
#' @keywords iteration
#' @examples
#' # do default multiple imputation on a numeric matrix
#' imp <- mice(nhanes)
#' imp
#'
#' # list the actual imputations for BMI
#' imp$imp$bmi
#'
#' # first completed data matrix
#' complete(imp)
#'
#' # imputation on mixed data with a different method per column
#' mice(nhanes2, meth = c("sample", "pmm", "logreg", "norm"))
#'
#' \dontrun{
#' # example where we fit the imputation model on the train data
#' # and apply the model to impute the test data
#' set.seed(123)
#' ignore <- sample(c(TRUE, FALSE), size = 25, replace = TRUE, prob = c(0.3, 0.7))
#'
#' # scenario 1: train and test in the same dataset
#' imp <- mice(nhanes2, m = 2, ignore = ignore, print = FALSE, seed = 22112)
#' imp.test1 <- filter(imp, ignore)
#' imp.test1$data
#' complete(imp.test1, 1)
#' complete(imp.test1, 2)
#'
#' # scenario 2: train and test in separate datasets
#' traindata <- nhanes2[!ignore, ]
#' testdata <- nhanes2[ignore, ]
#' imp.train <- mice(traindata, m = 2, print = FALSE, seed = 22112)
#' imp.test2 <- mice.mids(imp.train, newdata = testdata)
#' complete(imp.test2, 1)
#' complete(imp.test2, 2)
#' }
#' @export
mice <- function(data,
                 m = 5,
                 method = NULL,
                 predictorMatrix,
                 ignore = NULL,
                 where = NULL,
                 blocks,
                 visitSequence = NULL,
                 formulas,
                 blots = NULL,
                 post = NULL,
                 defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
                 maxit = 5,
                 printFlag = TRUE,
                 seed = NA,
                 data.init = NULL,
                 ...) {
  call <- match.call()
  check.deprecated(...)

  if (!is.na(seed)) set.seed(seed)

  # check form of data and m
  data <- check.dataform(data)
  m <- check.m(m)

  # determine input combination: predictorMatrix, blocks, formulas
  mp <- missing(predictorMatrix)
  mb <- missing(blocks)
  mf <- missing(formulas)

  # case A
  if (mp & mb & mf) {
    # blocks lead
    blocks <- make.blocks(colnames(data))
    predictorMatrix <- make.predictorMatrix(data, blocks)
    formulas <- make.formulas(data, blocks)
  }
  # case B
  if (!mp & mb & mf) {
    # predictorMatrix leads
    predictorMatrix <- check.predictorMatrix(predictorMatrix, data)
    blocks <- make.blocks(colnames(predictorMatrix), partition = "scatter")
    formulas <- make.formulas(data, blocks, predictorMatrix = predictorMatrix)
  }

  # case C
  if (mp & !mb & mf) {
    # blocks leads
    blocks <- check.blocks(blocks, data)
    predictorMatrix <- make.predictorMatrix(data, blocks)
    formulas <- make.formulas(data, blocks)
  }

  # case D
  if (mp & mb & !mf) {
    # formulas leads
    formulas <- check.formulas(formulas, data)
    blocks <- construct.blocks(formulas)
    predictorMatrix <- make.predictorMatrix(data, blocks)
  }

  # case E
  if (!mp & !mb & mf) {
    # predictor leads
    blocks <- check.blocks(blocks, data)
    z <- check.predictorMatrix(predictorMatrix, data, blocks)
    predictorMatrix <- z$predictorMatrix
    blocks <- z$blocks
    formulas <- make.formulas(data, blocks, predictorMatrix = predictorMatrix)
  }

  # case F
  if (!mp & mb & !mf) {
    # formulas lead
    formulas <- check.formulas(formulas, data)
    predictorMatrix <- check.predictorMatrix(predictorMatrix, data)
    blocks <- construct.blocks(formulas, predictorMatrix)
    predictorMatrix <- make.predictorMatrix(data, blocks, predictorMatrix)
  }

  # case G
  if (mp & !mb & !mf) {
    # blocks lead
    blocks <- check.blocks(blocks, data, calltype = "formula")
    formulas <- check.formulas(formulas, blocks)
    predictorMatrix <- make.predictorMatrix(data, blocks)
  }

  # case H
  if (!mp & !mb & !mf) {
    # blocks lead
    blocks <- check.blocks(blocks, data)
    formulas <- check.formulas(formulas, data)
    predictorMatrix <- check.predictorMatrix(predictorMatrix, data, blocks)
  }

  chk <- check.cluster(data, predictorMatrix)
  where <- check.where(where, data, blocks)

  # check visitSequence, edit predictorMatrix for monotone
  user.visitSequence <- visitSequence
  visitSequence <- check.visitSequence(visitSequence,
    data = data, where = where, blocks = blocks
  )
  predictorMatrix <- edit.predictorMatrix(
    predictorMatrix = predictorMatrix,
    visitSequence = visitSequence,
    user.visitSequence = user.visitSequence,
    maxit = maxit
  )
  method <- check.method(
    method = method, data = data, where = where,
    blocks = blocks, defaultMethod = defaultMethod
  )
  post <- check.post(post, data)
  blots <- check.blots(blots, data, blocks)
  ignore <- check.ignore(ignore, data)

  # data frame for storing the event log
  state <- list(it = 0, im = 0, dep = "", meth = "", log = FALSE)
  loggedEvents <- data.frame(it = 0, im = 0, dep = "", meth = "", out = "")

  # edit imputation setup
  setup <- list(
    method = method,
    predictorMatrix = predictorMatrix,
    visitSequence = visitSequence,
    post = post
  )
  setup <- edit.setup(data, setup, ...)
  method <- setup$method
  predictorMatrix <- setup$predictorMatrix
  visitSequence <- setup$visitSequence
  post <- setup$post

  # initialize imputations
  nmis <- apply(is.na(data), 2, sum)
  imp <- initialize.imp(
    data, m, ignore, where, blocks, visitSequence,
    method, nmis, data.init
  )

  # and iterate...
  from <- 1
  to <- from + maxit - 1
  q <- sampler(
    data, m, ignore, where, imp, blocks, method,
    visitSequence, predictorMatrix, formulas, blots,
    post, c(from, to), printFlag, ...
  )

  if (!state$log) loggedEvents <- NULL
  if (state$log) row.names(loggedEvents) <- seq_len(nrow(loggedEvents))

  ## save, and return
  midsobj <- list(
    data = data,
    imp = q$imp,
    m = m,
    where = where,
    blocks = blocks,
    call = call,
    nmis = nmis,
    method = method,
    predictorMatrix = predictorMatrix,
    visitSequence = visitSequence,
    formulas = formulas,
    post = post,
    blots = blots,
    ignore = ignore,
    seed = seed,
    iteration = q$iteration,
    lastSeedValue = get(".Random.seed",
      envir = globalenv(), mode = "integer",
      inherits = FALSE
    ),
    chainMean = q$chainMean,
    chainVar = q$chainVar,
    loggedEvents = loggedEvents,
    version = packageVersion("mice"),
    date = Sys.Date()
  )
  oldClass(midsobj) <- "mids"

  if (!is.null(midsobj$loggedEvents)) {
    warning("Number of logged events: ", nrow(midsobj$loggedEvents),
      call. = FALSE
    )
  }
  midsobj
}
