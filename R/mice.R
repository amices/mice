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
#' `pmm`               \tab any     \tab Predictive mean matching\cr
#' `midastouch`        \tab any     \tab Weighted predictive mean matching\cr
#' `sample`            \tab any     \tab Random sample from observed values\cr
#' `cart`              \tab any     \tab Classification and regression trees\cr
#' `rf`                \tab any     \tab Random forest imputations\cr
#' `mean`              \tab numeric \tab Unconditional mean imputation\cr
#' `norm`              \tab numeric \tab Bayesian linear regression\cr
#' `norm.nob`          \tab numeric \tab Linear regression ignoring model error\cr
#' `norm.boot`         \tab numeric \tab Linear regression using bootstrap\cr
#' `norm.predict`      \tab numeric \tab Linear regression, predicted values\cr
#' `lasso.norm`        \tab numeric \tab Lasso linear regression\cr
#' `lasso.select.norm` \tab numeric \tab Lasso select + linear regression\cr
#' `quadratic`         \tab numeric \tab Imputation of quadratic terms\cr
#' `ri`                \tab numeric \tab Random indicator for nonignorable data\cr
#' `logreg`            \tab binary  \tab Logistic regression\cr
#' `logreg.boot`       \tab binary  \tab Logistic regression with bootstrap\cr
#' `lasso.logreg`      \tab binary  \tab Lasso logistic regression\cr
#' `lasso.select.logreg`\tab binary  \tab Lasso select + logistic regression\cr
#' `polr`              \tab ordered \tab Proportional odds model\cr
#' `polyreg`           \tab unordered\tab Polytomous logistic regression\cr
#' `lda`               \tab unordered\tab Linear discriminant analysis\cr
#' `2l.norm`           \tab numeric  \tab Level-1 normal heteroscedastic\cr
#' `2l.lmer`           \tab numeric  \tab Level-1 normal homoscedastic, lmer\cr
#' `2l.pan`            \tab numeric  \tab Level-1 normal homoscedastic, pan\cr
#' `2l.bin`            \tab binary   \tab Level-1 logistic, glmer\cr
#' `2lonly.mean`       \tab numeric  \tab Level-2 class mean\cr
#' `2lonly.norm`       \tab numeric  \tab Level-2 class normal\cr
#' `2lonly.pmm`        \tab any      \tab Level-2 class predictive mean matching
#' }
#'
#' These corresponding functions are coded in the `mice` library under
#' names `mice.impute.method`, where `method` is a string with the
#' name of the univariate imputation method name, for example `norm`. The
#' `method` argument specifies the methods to be used.  For the `j`'th
#' column, `mice()` calls the first occurrence of
#' `paste('mice.impute.', method[j], sep = '')` in the search path.  The
#' mechanism allows uses to write customized imputation function,
#' `mice.impute.myfunc`. To call it for all columns specify
#' `method='myfunc'`.  To call it only for, say, column 2 specify
#' \code{method=c('norm','myfunc','logreg',\dots{})}.
#'
#' *Skipping imputation:* Imputation of variable (or variable block)
#' \eqn{j} can be skipped by setting the empty method, `method[j] = ""`.
#' On start-up, `mice()` will test whether variables within
#' block \eqn{j} need imputation. If not, `mice()` takes two actions:
#' It sets `method[j] <- ""` and it sets the rows of the `predictorMatrix` of
#' the variables within block \eqn{j} to zero.
#'
#' *BEWARE: Propagation of `NA`s*: Setting the empty method
#' for an incomplete variable is legal and prevent `mice()`  from generating
#' imputations for its missing cells. Sometimes this is wanted, but
#' it may have a surprising side effect to due missing value propagation.
#' For example, if column `"A"` contains `NA`'s and is a predictor in the
#' imputation model for column `"B"`, then setting `method["A"] = ""` will
#' propagate the missing data of `"A"` into `"B"` for the rows in `"B"`
#' where `"A"` is missing. The imputed data for `"B"` thus contain `NA`'s.
#' If this is not desired, apply one of the following two remedies:
#' 1) Remove column `"A"` as predictor from all imputation models, e.g.,
#' by setting `predictorMatrix[, "A"] <- 0`, and re-impute.
#' Or 2) Specify an imputation method for `"A"` and impute `"A"`. Optionally,
#' after convergence manually replace any imputations for `"A"` by `NA`
#' using `imp$imp$A[] <- NA`. In that case, `complete(imp, 1)` produces a
#' dataset that is complete, except for column `"A"`.
#'
#' *Passive imputation:* `mice()` supports a special built-in method,
#' called passive imputation. This method can be used to ensure that a data
#' transform always depends on the most recently generated imputations.  In some
#' cases, an imputation model may need transformed data in addition to the
#' original data (e.g. log, quadratic, recodes, interaction, sum scores, and so
#' on).
#'
#' Passive imputation maintains consistency among different transformations of
#' the same data. Passive imputation is invoked if `~` is specified as the
#' first character of the string that specifies the univariate method.
#' `mice()` interprets the entire string, including the `~` character,
#' as the formula argument in a call to `model.frame(formula,
#' data[!r[,j],])`. This provides a simple mechanism for specifying deterministic
#' dependencies among the columns. For example, suppose that the missing entries
#' in variables `data$height` and `data$weight` are imputed. The body
#' mass index (BMI) can be calculated within `mice` by specifying the
#' string `'~I(weight/height^2)'` as the univariate imputation method for
#' the target column `data$bmi`.  Note that the `~` mechanism works
#' only on those entries which have missing values in the target column. You
#' should make sure that the combined observed and imputed parts of the target
#' column make sense. An easy way to create consistency is by coding all entries
#' in the target as `NA`, but for large data sets, this could be
#' inefficient.  Note that you may also need to adapt the default
#' `predictorMatrix` to evade linear dependencies among the predictors that
#' could cause errors like `Error in solve.default()` or `Error:
#' system is exactly singular`. Though not strictly needed, it is often useful
#' to specify `visitSequence` such that the column that is imputed by the
#' `~` mechanism is visited each time after one of its predictors was
#' visited. In that way, deterministic relation between columns will always be
#' synchronized.
#'
#' #'A new argument `ls.meth` can be parsed to the lower level
#' `.norm.draw` to specify the method for generating the least squares
#' estimates and any subsequently derived estimates. Argument `ls.meth`
#' takes one of three inputs: `"qr"` for QR-decomposition, `"svd"` for
#' singular value decomposition and `"ridge"` for ridge regression.
#' `ls.meth` defaults to `ls.meth = "qr"`.
#'
#' *Auxiliary predictors in formulas specification: *
#' For a given block, the `formulas` specification takes precedence over
#' the corresponding row in the `predictMatrix` argument. This
#' precedence is, however, restricted to the subset of variables
#' specified in the terms of the block formula. Any
#' variables not specified by `formulas` are imputed
#' according to the `predictMatrix` specification. Variables with
#' non-zero `type` values in the `predictMatrix` will
#' be added as main effects to the `formulas`, which will
#' act as supplementary covariates in the imputation model. It is possible
#' to turn off this behavior by specifying the
#' argument `auxiliary = FALSE`.
#'
#' @param data      Data frame with \eqn{n} rows and \eqn{p} columns with
#'                  incomplete data.  Missing values are coded as `NA`.
#' @param m         Number of multiple imputations. The default is `m = 5`.
#'                  Setting `m = 1` produces a single imputation per cell
#'                  (not recommended in general).
#' @param method    Character vector of length \eqn{q} specifying imputation
#'                  methods for (groups of) variables. In the special case
#'                  `length(method) == 1`, the specified method applies to all
#'                  variables. When `method` is not specified, `mice()` will
#'                  select a method based on the variable type as regulated
#'                  by the `defaultMethod` argument. See details
#'                  on *skipping imputation*.
#' @param predictorMatrix
#'                  A square numeric matrix of \eqn{p} rows
#'                  and columns. Row- and column names are `colnames(data)`.
#'                  Each row corresponds to a variable to be imputed.
#'                  A value of `1` means that the column variable is a
#'                  predictor for the row variable, while a `0` means that
#'                  the column variable is not a predictor. The default
#'                  `predictorMatrix` is `1` everywhere, except for a zero
#'                  diagonal. For variables that need no be imputed,
#'                  `mice()` automatically sets the corresponding rows in the
#'                  `predictorMatrix` to zero. See details
#'                  on *skipping imputation*.
#'                  Two-level imputation models (which have `"2l"` in their
#'                  names) other codes than `0` and `1`, e.g, `2` or `-2`,
#'                  are also used.
#' @param ignore    A logical vector of \eqn{n} elements indicating
#'                  which rows are ignored for estimating the parameters of
#'                  the imputation model.
#'                  Rows with `ignore` set to `TRUE` do not influence the
#'                  parameters of the imputation model.
#'                  The `ignore` argument allows splitting `data` into a
#'                  training set (on which we fit the imputation model)
#'                  and a test set (that does not influence the imputation
#'                  model parameter estimates).
#'                  The default `NULL` corresponds to all `FALSE`, thus
#'                  including all rows into the imputation models.
#'                  Note: Multivariate imputation methods,
#'                  like `mice.impute.jomoImpute()` or
#'                  `mice.impute.panImpute()`, do not honour the `ignore`
#'                  argument.
#' @param where     A data frame or matrix of logicals with \eqn{n} rows
#'                  and \eqn{p} columns, indicating the cells of `data` for
#'                  which imputations are generated.
#'                  The default `where = is.na(data)` specifies that all
#'                  missing data are imputed.
#'                  The `where` argument can overimpute cells
#'                  with observed data, or skip imputation of specific missing
#'                  cells. Be aware that the latter option could propagate
#'                  missing values to other variables. See details.
#'                  Note: Methods that generate multivariate imputations
#'                  (e.g. `mice.impute.panImpute()`) do not honour the
#'                  `where` argument.
#' @param blocks List of vectors with variable names per block. List elements
#' may be named to identify blocks. Variables within a block are
#' imputed by a multivariate imputation method
#' (see `method` argument). By default each variable is placed
#' into its own block, which is effectively
#' fully conditional specification (FCS) by univariate models
#' (variable-by-variable imputation). Only variables whose names appear in
#' `blocks` are imputed. The relevant columns in the `where`
#' matrix are set to `FALSE` of variables that are not block members.
#' A variable may appear in multiple blocks. In that case, it is
#' effectively re-imputed each time that it is visited.
#' @param visitSequence A vector of block names of arbitrary length, specifying the
#' sequence of blocks that are imputed during one iteration of the Gibbs
#' sampler. A block is a collection of variables. All variables that are
#' members of the same block are imputed
#' when the block is visited. A variable that is a member of multiple blocks
#' is re-imputed within the same iteration.
#' The default `visitSequence = "roman"` visits the blocks (left to right)
#' in the order in which they appear in `blocks`.
#' One may also use one of the following keywords: `"arabic"`
#' (right to left), `"monotone"` (ordered low to high proportion
#' of missing data) and `"revmonotone"` (reverse of monotone).
#' *Special case*: If you specify both `visitSequence = "monotone"` and
#' `maxit = 1`, then the procedure will edit the `predictorMatrix`
#' to conform to the monotone pattern. Realize that convergence in one
#' iteration is only guaranteed if the missing data pattern is actually
#' monotone. The procedure does not check this.
#' @param formulas A named list of formula's, or expressions that
#' can be converted into formula's by `as.formula`. List elements
#' correspond to blocks. The block to which the list element applies is
#' identified by its name, so list names must correspond to block names.
#' The `formulas` argument is an alternative to the
#' `predictorMatrix` argument that allows for more flexibility in
#' specifying imputation models, e.g., for specifying interaction terms.
#' @param blots A named `list` of `alist`'s that can be used
#' to pass down arguments to lower level imputation function. The entries
#' of element `blots[[blockname]]` are passed down to the function
#' called for block `blockname`.
#' @param post A vector of strings with length `ncol(data)` specifying
#' expressions as strings. Each string is parsed and
#' executed within the `sampler()` function to post-process
#' imputed values during the iterations.
#' The default is a vector of empty strings, indicating no post-processing.
#' Multivariate (block) imputation methods ignore the `post` parameter.
#' @param defaultMethod A vector of length 4 containing the default
#' imputation methods for 1) numeric data, 2) factor data with 2 levels, 3)
#' factor data with > 2 unordered levels, and 4) factor data with > 2
#' ordered levels. By default, the method uses
#' `pmm`, predictive mean matching (numeric data) `logreg`, logistic
#' regression imputation (binary data, factor with 2 levels) `polyreg`,
#' polytomous regression imputation for unordered categorical data (factor > 2
#' levels) `polr`, proportional odds model for (ordered, > 2 levels).
#' @param maxit A scalar giving the number of iterations. The default is 5.
#' @param printFlag If `TRUE`, `mice` will print history on console.
#' Use `print=FALSE` for silent computation.
#' @param seed An integer that is used as argument by the `set.seed()` for
#' offsetting the random number generator. Default is to leave the random number
#' generator alone.
#' @param data.init A data frame of the same size and type as `data`,
#' without missing data, used to initialize imputations before the start of the
#' iterative process.  The default `NULL` implies that starting imputation
#' are created by a simple random draw from the data. Note that specification of
#' `data.init` will start all `m` Gibbs sampling streams from the same
#' imputation.
#' @param \dots Named arguments that are passed down to the univariate imputation
#' functions.
#' @param nest experimental variable grouping input
#'
#' @return Returns an S3 object of class [`mids()`][mids-class]
#'        (multiply imputed data set)
#' @author Stef van Buuren \email{stef.vanbuuren@@tno.nl}, Karin
#' Groothuis-Oudshoorn \email{c.g.m.oudshoorn@@utwente.nl}, 2000-2010, with
#' contributions of Alexander Robitzsch, Gerko Vink, Shahab Jolani,
#' Roel de Jong, Jason Turner, Lisa Doove,
#' John Fox, Frank E. Harrell, and Peter Malewski.
#' @seealso [`mids()`][mids-class], [with.mids()],
#' [set.seed()], [complete()]
#' @references Van Buuren, S., Groothuis-Oudshoorn, K. (2011). `mice`:
#' Multivariate Imputation by Chained Equations in `R`. *Journal of
#' Statistical Software*, **45**(3), 1-67.
#' \doi{10.18637/jss.v045.i03}
#'
#' Van Buuren, S. (2018).
#' [*Flexible Imputation of Missing Data. Second Edition.*](https://stefvanbuuren.name/fimd/sec-FCS.html#sec:MICE)
#' Chapman & Hall/CRC. Boca Raton, FL.
#'
#' Van Buuren, S., Brand, J.P.L., Groothuis-Oudshoorn C.G.M., Rubin, D.B. (2006)
#' Fully conditional specification in multivariate imputation.  *Journal of
#' Statistical Computation and Simulation*, **76**, 12, 1049--1064.
#'
#' Van Buuren, S. (2007) Multiple imputation of discrete and continuous data by
#' fully conditional specification.  *Statistical Methods in Medical
#' Research*, **16**, 3, 219--242.
#'
#' Van Buuren, S., Boshuizen, H.C., Knook, D.L. (1999) Multiple imputation of
#' missing blood pressure covariates in survival analysis.  *Statistics in
#' Medicine*, **18**, 681--694.
#'
#' Brand, J.P.L. (1999) *Development, implementation and evaluation of
#' multiple imputation strategies for the statistical analysis of incomplete
#' data sets.* Dissertation. Rotterdam: Erasmus University.
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
                 nest = NULL,
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

  # add support nest
  if (!is.null(nest)) {
    blocks <- n2b(nest, silent = FALSE)
  }

  # determine input combination: predictorMatrix, blocks, formulas
  mp <- missing(predictorMatrix)
  mb <- missing(blocks)
  mf <- missing(formulas)

  # store unedited user predictorMatrix
  user.predictorMatrix <- NULL
  if (!mp) {
    user.predictorMatrix <- predictorMatrix
  }
  user.blocks <- NULL
  if (!mb) {
    user.blocks <- blocks
  }

  # case A
  if (mp & mb & mf) {
    # formulas leads
    formulas <- make.formulas(data)
    predictorMatrix <- f2p(formulas, data)
    blocks <- construct.blocks(formulas)
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
    predictorMatrix <- f2p(formulas, data, blocks)
  }

  # case E
  if (!mp & !mb & mf) {
    # predictor leads (use for multivariate imputation)
    predictorMatrix <- check.predictorMatrix(predictorMatrix, data)
    blocks <- check.blocks(blocks, data, calltype = "pred")
    formulas <- make.formulas(data, blocks, predictorMatrix = predictorMatrix)
  }

  # case F
  if (!mp & mb & !mf) {
    # it is better to forbid this case
    # formulas lead
    formulas <- check.formulas(formulas, data)
    predictorMatrix <- check.predictorMatrix(predictorMatrix, data)
    blocks <- construct.blocks(formulas, predictorMatrix)
    predictorMatrix <- make.predictorMatrix(data, blocks, predictorMatrix)
  }

  # case G
  if (mp & !mb & !mf) {
    # it is better to forbid this case
    # blocks lead
    blocks <- check.blocks(blocks, data)
    formulas <- check.formulas(formulas, blocks)
    predictorMatrix <- make.predictorMatrix(data, blocks)
  }

  # case H
  if (!mp & !mb & !mf) {
    # it is better to forbid this case
    # blocks lead
    blocks <- check.blocks(blocks, data)
    formulas <- check.formulas(formulas, data)
    predictorMatrix <- check.predictorMatrix(predictorMatrix, data, blocks)
  }

  chk <- check.cluster(data, predictorMatrix)
  where <- check.where(where, data, blocks)

  # check visitSequence,
  user.visitSequence <- visitSequence
  visitSequence <- check.visitSequence(visitSequence,
                                       data = data, where = where, blocks = blocks
  )

  # derive method vector
  method <- check.method(
    method = method, data = data, where = where,
    blocks = blocks, defaultMethod = defaultMethod,
    user.predictorMatrix = user.predictorMatrix,
    user.blocks = user.blocks
  )

  # edit predictorMatrix for monotone, set zero rows for empty methods
  predictorMatrix <- edit.predictorMatrix(
    predictorMatrix = predictorMatrix,
    method = method,
    blocks = blocks,
    where = where,
    visitSequence = visitSequence,
    user.visitSequence = user.visitSequence,
    maxit = maxit
  )

  # update formulas to ~ 1 if method = ""
  for (b in names(method)) {
    if (hasName(formulas, b) && method[[b]] == "") {
      formulas[[b]] <- as.formula(paste(b, "~ 1"))
    }
  }

  # evasion of NA propagation by inactivating unimputed incomplete predictors
  # issue #583
  # 1) find unimputed incomplete predictors
  # 2) set predictorMatrix entries to zero
  # 3) update formulas

  # step 1: uip = unimputed incomplete predictors
  # nomissings <- colnames(data)[!apply(is.na(data), 2, sum)]
  # uip <- setdiff(colnames(data), unlist(blocks))

  # step 2: update predictorMatrix
  # setrowzero <- intersect(nomissings, uip)
  # setcolzero <- setdiff(uip, nomissings)
  # predictorMatrix[, setcolzero] <- 0
  # predictorMatrix[setrowzero, ] <- 0

  # step 3: update formulas
  # formulas <- lapply(formulas, remove.rhs.variables, vars = uip)

  # other checks
  post <- check.post(post, data)
  blots <- check.blots(blots, data, blocks)
  ignore <- check.ignore(ignore, data)

  # data frame for storing the event log
  state <- list(it = 0, im = 0, dep = "", meth = "", log = FALSE)
  loggedEvents <- data.frame(it = 0, im = 0, dep = "", meth = "", out = "")

  # edit imputation setup
  setup <- list(
    method = method,
    formulas = formulas,
    blots = blots,
    predictorMatrix = predictorMatrix,
    visitSequence = visitSequence,
    post = post
  )
  setup <- edit.setup(data, setup, ...)
  method <- setup$method
  formulas <- setup$formulas
  blots <- setup$blots
  predictorMatrix <- setup$predictorMatrix
  visitSequence <- setup$visitSequence
  post <- setup$post

  # update model
  #  formulas <- p2f(predictorMatrix, blocks)
  #  roles <- p2c(predictorMatrix)
  #  blots <- paste.roles(blots, roles)

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

  stopifnot(validate.mids(midsobj))

  if (!is.null(midsobj$loggedEvents)) {
    warning("Number of logged events: ", nrow(midsobj$loggedEvents),
            call. = FALSE
    )
  }
  midsobj
}
