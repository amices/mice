#' @name mice
#' @title Multivariate Imputation by Chained Equations (MICE)
#'
#' @description
#' The `mice()` function generates multiple imputations for incomplete
#' multivariate data. In constast to single imputation, each missing
#' value is replaced by two or more plausible values. The variation between
#' these plausible value represents the inherent uncertainty of the
#' value that is missing.
#'
#' The `mice()` function imputes
#' an incomplete column (the target column) by generating 'plausible' synthetic
#' values given other columns in the data. Each incomplete column can act as a
#' target column, with its own specific set of predictors. The default set of
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
#' | Method                | Type      | Description                              |
#' |-----------------------|-----------|------------------------------------------|
#' | `pmm`                | any       | Predictive mean matching                 |
#' | `midastouch`         | any       | Weighted predictive mean matching        |
#' | `sample`             | any       | Random sample from observed values       |
#' | `cart`               | any       | Classification and regression trees      |
#' | `rf`                 | any       | Random forest imputations                |
#' | `mean`               | numeric   | Unconditional mean imputation            |
#' | `norm`               | numeric   | Bayesian linear regression               |
#' | `norm.nob`           | numeric   | Linear regression ignoring model error   |
#' | `norm.boot`          | numeric   | Linear regression using bootstrap        |
#' | `norm.predict`       | numeric   | Linear regression, predicted values      |
#' | `lasso.norm`         | numeric   | Lasso linear regression                  |
#' | `lasso.select.norm`  | numeric   | Lasso select + linear regression         |
#' | `quadratic`          | numeric   | Imputation of quadratic terms            |
#' | `ri`                 | numeric   | Random indicator for nonignorable data   |
#' | `logreg`             | binary    | Logistic regression                      |
#' | `logreg.boot`        | binary    | Logistic regression with bootstrap       |
#' | `lasso.logreg`       | binary    | Lasso logistic regression                |
#' | `lasso.select.logreg`| binary    | Lasso select + logistic regression       |
#' | `polr`               | ordered   | Proportional odds model                  |
#' | `polyreg`            | unordered | Polytomous logistic regression           |
#' | `lda`                | unordered | Linear discriminant analysis             |
#' | `2l.norm`            | numeric   | Level-1 normal heteroscedastic           |
#' | `2l.lmer`            | numeric   | Level-1 normal homoscedastic, lmer       |
#' | `2l.pan`             | numeric   | Level-1 normal homoscedastic, pan        |
#' | `2l.bin`             | binary    | Level-1 logistic, glmer                  |
#' | `2lonly.mean`        | numeric   | Level-2 class mean                       |
#' | `2lonly.norm`        | numeric   | Level-2 class normal                     |
#' | `2lonly.pmm`         | any       | Level-2 class predictive mean matching   |
#'
#' These corresponding functions are coded in the `mice` library under
#' names `mice.impute.method`, where `method` is a string with the
#' name of the univariate imputation method name, for example `norm`. The
#' `method` argument specifies the methods to be used. For the `j`th
#' column, `mice()` calls the first occurrence of
#' `paste('mice.impute.', method[j], sep = '')` in the search path. The
#' mechanism allows uses to write customized imputation function,
#' `mice.impute.myfunc`. To call it for all columns specify
#' `method='myfunc'`. To call it only for, say, column 2 specify
#' `method=c('norm','myfunc','logreg',...)`.
#'
#' *Skipping imputation*: The user may skip imputation of a column by
#' setting its entry to the empty method: `""`. For complete columns without
#' missing data `mice` will automatically set the empty method. Setting the
#' empty method does not produce imputations for the column, so any missing
#' cells remain `NA`. If column A contains `NA`s and is used as
#' predictor in the imputation model for column B, then `mice` produces no
#' imputations for the rows in B where A is missing. The imputed data
#' for B may thus contain `NA`s. The remedy is to remove column A from
#' the imputation model for the other columns in the data. This can be done
#' by setting the entire column for variable A in the `predictorMatrix`
#' equal to zero.
#'
#' *Passive imputation*: `mice()` supports a special built-in method,
#' called passive imputation. This method can be used to ensure that a data
#' transform always depends on the most recently generated imputations. In some
#' cases, an imputation model may need transformed data in addition to the
#' original data (e.g., log, quadratic, recodes, interaction, sum scores, and so
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
#' string `~I(weight/height^2)` as the univariate imputation method for
#' the target column `data$bmi`. Note that the `~` mechanism works
#' only on those entries which have missing values in the target column. You
#' should make sure that the combined observed and imputed parts of the target
#' column make sense. An easy way to create consistency is by coding all entries
#' in the target as `NA`, but for large data sets, this could be
#' inefficient. Note that you may also need to adapt the default
#' `predictorMatrix` to evade linear dependencies among the predictors that
#' could cause errors like `Error in solve.default()` or `Error:
#' system is exactly singular`. Though not strictly needed, it is often useful
#' to specify `visitSequence` such that the column that is imputed by the
#' `~` mechanism is visited each time after one of its predictors was
#' visited. In that way, deterministic relation between columns will always be
#' synchronized.
#'
#' A new argument `ls.meth` can be parsed to the lower level
#' `.norm.draw` to specify the method for generating the least squares
#' estimates and any subsequently derived estimates. Argument `ls.meth`
#' takes one of three inputs: `"qr"` for QR-decomposition, `"svd"` for
#' singular value decomposition, and `"ridge"` for ridge regression.
#' `ls.meth` defaults to `ls.meth = "qr"`.
#'
#' *Auxiliary predictors in formulas specification*:
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
#' @param data A `data.frame` or a `matrix` containing the incomplete data. Missing
#'   values are coded as `NA`.
#' @param m Number of multiple imputations. The default is `m = 5`.
#' @param method Can be either a single string or a vector of strings with
#'   length equal to `length(blocks)`, specifying the imputation method to be
#'   used for each column in `data`. If specified as a single string, the same
#'   method will be used for all blocks. The default imputation method (when no
#'   argument is specified) depends on the measurement level of the target column,
#'   as regulated by the `defaultMethod` argument. Columns that do not need to be
#'   imputed should have the empty method `""`. See Details.
#' @param predictorMatrix A numeric matrix with `length(blocks)` rows
#'   and `ncol(data)` columns, containing 0/1 data specifying
#'   the set of predictors to be used for each target column.
#'   Each row corresponds to a variable block, i.e., a set of variables
#'   to be imputed. A value of `1` means that the column variable is used as a
#'   predictor for the target block (in the rows).
#'
#'   By default, the `predictorMatrix` is a square matrix of `ncol(data)`
#'   rows and columns with all `1`s, except for the diagonal.
#'
#'   **Note**: For two-level imputation models (which have `"2l"` in their names),
#'   other codes (e.g., `2` or `-2`) are also allowed.
#' @param ignore A logical vector of `nrow(data)` elements indicating
#'   which rows are ignored when creating the imputation model. The default
#'   `NULL` includes all rows that have an observed value of the variable
#'   to be imputed. Rows with `ignore` set to `TRUE` do not influence the
#'   parameters of the imputation model but are still imputed. This argument
#'   can be used to split `data` into a training set (on which the
#'   imputation model is built) and a test set (that does not influence the
#'   imputation model estimates).
#'
#'   **Note**: Multivariate imputation methods, like `mice.impute.jomoImpute()`
#'   or `mice.impute.panImpute()`, do not honor the `ignore` argument.
#' @param where A `data.frame` or `matrix` with logical values of the same dimensions
#'   as `data`, indicating where in the data the imputations should be
#'   created. The default, `where = is.na(data)`, specifies that the
#'   missing data should be imputed. The `where` argument may be used to
#'   overimpute observed data or to skip imputations for selected missing values.
#'
#'   **Note**: Imputation methods that generate imputations outside of
#'   `mice`, like `mice.impute.panImpute()`, may depend on a complete
#'   predictor space. In that case, a custom `where` matrix cannot be
#'   specified.
#' @param blocks A list of vectors with variable names per block. List elements
#'   may be named to identify blocks. Variables within a block are
#'   imputed by a multivariate imputation method
#'   (see the `method` argument). By default, each variable is placed
#'   into its own block, which is effectively fully conditional specification
#'   (FCS) by univariate models (variable-by-variable imputation).
#'
#'   Only variables whose names appear in `blocks` are imputed. The relevant
#'   columns in the `where` matrix are set to `FALSE` for variables that
#'   are not block members. A variable may appear in multiple blocks. In that
#'   case, it is effectively re-imputed each time that it is visited.
#' @param visitSequence A vector of block names of arbitrary length, specifying the
#'   sequence of blocks that are imputed during one iteration of the Gibbs
#'   sampler. A block is a collection of variables. All variables that are
#'   members of the same block are imputed when the block is visited. A variable
#'   that is a member of multiple blocks is re-imputed within the same iteration.
#'
#'   The default, `visitSequence = "roman"`, visits the blocks (left to right)
#'   in the order in which they appear in `blocks`.
#'
#'   You can also use one of the following keywords: `"arabic"`
#'   (right to left), `"monotone"` (ordered low to high proportion
#'   of missing data), and `"revmonotone"` (reverse of monotone).
#'
#'   **Special case**: If you specify both `visitSequence = "monotone"` and
#'   `maxit = 1`, the procedure will edit the `predictorMatrix`
#'   to conform to the monotone pattern. Convergence in one
#'   iteration is only guaranteed if the missing data pattern is actually
#'   monotone. The procedure does not check this.
#' @param formulas A named list of formulas, or expressions that
#'   can be converted into formulas by `as.formula`. List elements
#'   correspond to blocks. The block to which the list element applies is
#'   identified by its name, so list names must correspond to block names.
#'
#'   The `formulas` argument is an alternative to the
#'   `predictorMatrix` argument that allows for more flexibility in
#'   specifying imputation models, e.g., for specifying interaction terms.
#' @param modeltype A character vector of `length(blocks)` elements
#'   that indicates how the imputation model is specified. Entries can be
#'   one of two values: `"pred"` or `"formula"`. If
#'   `modeltype = "pred"`, the predictors of the imputation
#'   model for the block are specified by the corresponding row of the
#'   `predictorMatrix`. If  `modeltype = "formula"`, the
#'   imputation model is specified by the relevant entry in
#'   `formulas`. The default depends on the presence of the
#'   `formulas` argument. If `formulas` is present, then
#'   `mice()` sets
#'   `modeltype = "formula"` for any block
#'   for which a formula is specified. Otherwise, `modeltype = "pred"`.
#' @param blots A named `list` of `alist`s that can be used
#'   to pass down arguments to lower-level imputation functions. The entries
#'   of element `blots[[blockname]]` are passed down to the function
#'   called for block `blockname`.
#' @param post A vector of strings with length `ncol(data)` specifying
#'   expressions as strings. Each string is parsed and
#'   executed within the `sampler()` function to post-process
#'   imputed values during the iterations.
#'
#'   The default is a vector of empty strings, indicating no post-processing.
#'   Multivariate (block) imputation methods ignore the `post` parameter.
#' @param defaultMethod A vector of length 4 containing the default
#'   imputation methods for:
#'
#'   1. numeric data,
#'   2. factor data with 2 levels,
#'   3. factor data with > 2 unordered levels, and
#'   4. factor data with > 2 ordered levels.
#'
#'   By default, the methods are:
#'   - `pmm` for numeric data
#'   - `logreg` for binary (2-level) factor data
#'   - `polyreg` for unordered categorical data (factor > 2 levels)
#'   - `polr` for ordered categorical data (> 2 levels).
#' @param maxit A scalar giving the number of iterations. The default is 5.
#' @param printFlag If `TRUE`, `mice` will print history on the console.
#'   Use `print = FALSE` for silent computation.
#' @param seed An integer that is used as an argument to `set.seed()` for
#'   offsetting the random number generator. Default is to leave the random number
#'   generator unchanged.
#' @param data.init A `data.frame` of the same size and type as `data`,
#'   without missing data, used to initialize imputations before the start of the
#'   iterative process. The default `NULL` implies that starting imputations
#'   are created by a simple random draw from the data. Note that specification of
#'   `data.init` will start all `m` Gibbs sampling streams from the same
#'   imputation.
#' @param ... Named arguments that are passed down to the univariate imputation
#'   functions.
#'
#' #' @return Returns an S3 object of class [`mids`](mids-class)
#'   (multiply imputed data set).
#'
#' @author
#' Stef van Buuren \email{stef.vanbuuren@@tno.nl},
#' Karin Groothuis-Oudshoorn \email{c.g.m.oudshoorn@@utwente.nl}, 2000–2010,
#' with contributions from:
#' Alexander Robitzsch, Gerko Vink, Shahab Jolani, Roel de Jong, Jason Turner,
#' Lisa Doove, John Fox, Frank E. Harrell, and Peter Malewski.
#'
#' @seealso
#' - [`mids()`]
#' - [`with.mids()`]
#' - [`set.seed()`]
#' - [`complete()`]
#'
#' @references
#' Van Buuren, S., Groothuis-Oudshoorn, K. (2011).
#' *mice: Multivariate Imputation by Chained Equations in R*.
#' *Journal of Statistical Software*, **45**(3), 1–67.
#' [doi:10.18637/jss.v045.i03](https://doi.org/10.18637/jss.v045.i03)
#'
#' Van Buuren, S. (2018).
#' *Flexible Imputation of Missing Data. Second Edition.*
#' Chapman & Hall/CRC. Boca Raton, FL.
#'
#' Van Buuren, S., Brand, J.P.L., Groothuis-Oudshoorn, C.G.M., Rubin, D.B. (2006).
#' Fully conditional specification in multivariate imputation.
#' *Journal of Statistical Computation and Simulation*, **76**(12), 1049–1064.
#'
#' Van Buuren, S. (2007).
#' Multiple imputation of discrete and continuous data by fully conditional specification.
#' *Statistical Methods in Medical Research*, **16**(3), 219–242.
#'
#' Van Buuren, S., Boshuizen, H.C., Knook, D.L. (1999).
#' Multiple imputation of missing blood pressure covariates in survival analysis.
#' *Statistics in Medicine*, **18**, 681–694.
#'
#' Brand, J.P.L. (1999).
#' *Development, implementation and evaluation of multiple imputation strategies for the statistical analysis of incomplete data sets.*
#' Dissertation. Rotterdam: Erasmus University.
#'
#' @keywords iteration
#'
#' @examples
#' # Perform default multiple imputation on a numeric matrix
#' imp <- mice(nhanes)
#' imp
#'
#' # List the actual imputations for BMI
#' imp$imp$bmi
#'
#' # First completed data matrix
#' complete(imp)
#'
#' # Imputation on mixed data with a different method per column
#' mice(nhanes2, meth = c("sample", "pmm", "logreg", "norm"))
#'
#' \dontrun{
#' # Example: Fit the imputation model on the train data
#' # and apply the model to impute the test data
#' set.seed(123)
#' ignore <- sample(c(TRUE, FALSE), size = 25, replace = TRUE, prob = c(0.3, 0.7))
#'
#' # Scenario 1: Train and test in the same dataset
#' imp <- mice(nhanes2, m = 2, ignore = ignore, print = FALSE, seed = 22112)
#' imp.test1 <- filter(imp, ignore)
#' imp.test1$data
#' complete(imp.test1, 1)
#' complete(imp.test1, 2)
#'
#' # Scenario 2: Train and test in separate datasets
#' traindata <- nhanes2[!ignore, ]
#' testdata <- nhanes2[ignore, ]
#' imp.train <- mice(traindata, m = 2, print = FALSE, seed = 22112)
#' imp.test2 <- mice.mids(imp.train, newdata = testdata)
#' complete(imp.test2, 1)
#' complete(imp.test2, 2)
#' }
#'
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
                 modeltype = NULL,
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
    modeltype <- make.modeltype(modeltype, predictorMatrix, formulas, "pred")
  }
  # case B
  if (!mp & mb & mf) {
    # predictorMatrix leads
    predictorMatrix <- check.predictorMatrix(predictorMatrix, data)
    blocks <- make.blocks(colnames(predictorMatrix), partition = "scatter")
    formulas <- make.formulas(data, blocks, predictorMatrix = predictorMatrix)
    modeltype <- make.modeltype(modeltype, predictorMatrix, formulas, "pred")
  }

  # case C
  if (mp & !mb & mf) {
    # blocks leads
    blocks <- check.blocks(blocks, data)
    predictorMatrix <- make.predictorMatrix(data, blocks)
    formulas <- make.formulas(data, blocks)
    modeltype <- make.modeltype(modeltype, predictorMatrix, formulas, "pred")
  }

  # case D
  if (mp & mb & !mf) {
    # formulas leads
    formulas <- check.formulas(formulas, data)
    blocks <- construct.blocks(formulas)
    predictorMatrix <- make.predictorMatrix(data, blocks)
    modeltype <- make.modeltype(modeltype, predictorMatrix, formulas, "formula")
  }

  # case E
  if (!mp & !mb & mf) {
    # predictor leads
    blocks <- check.blocks(blocks, data)
    z <- check.predictorMatrix(predictorMatrix, data, blocks)
    predictorMatrix <- z$predictorMatrix
    blocks <- z$blocks
    formulas <- make.formulas(data, blocks, predictorMatrix = predictorMatrix)
    modeltype <- make.modeltype(modeltype, predictorMatrix, formulas, "pred")
  }

  # case F
  if (!mp & mb & !mf) {
    # formulas lead
    formulas <- check.formulas(formulas, data)
    predictorMatrix <- check.predictorMatrix(predictorMatrix, data)
    blocks <- construct.blocks(formulas, predictorMatrix)
    predictorMatrix <- make.predictorMatrix(data, blocks, predictorMatrix)
    modeltype <- make.modeltype(modeltype, predictorMatrix, formulas, "formula")
  }

  # case G
  if (mp & !mb & !mf) {
    # blocks lead
    blocks <- check.blocks(blocks, data)
    formulas <- check.formulas(formulas, blocks)
    predictorMatrix <- make.predictorMatrix(data, blocks)
    modeltype <- make.modeltype(modeltype, predictorMatrix, formulas, "formula")
  }

  # case H
  if (!mp & !mb & !mf) {
    # blocks lead
    blocks <- check.blocks(blocks, data)
    formulas <- check.formulas(formulas, data)
    predictorMatrix <- check.predictorMatrix(predictorMatrix, data, blocks)
    modeltype <- make.modeltype(modeltype, predictorMatrix, formulas, "formula")
  }

  chk <- check.cluster(data, predictorMatrix)
  where <- check.where(where, data, blocks)

  # check visitSequence, edit predictorMatrix for monotone
  user.visitSequence <- visitSequence
  visitSequence <- check.visitSequence(visitSequence,
    data = data, where = where, blocks = blocks
  )
  predictorMatrix <- mice.edit.predictorMatrix(
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
  setup <- mice.edit.setup(data, setup, ...)
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
    visitSequence, predictorMatrix, formulas,
    modeltype, blots,
    post, c(from, to), printFlag, ...
  )

  if (!state$log) loggedEvents <- NULL
  if (state$log) row.names(loggedEvents) <- seq_len(nrow(loggedEvents))

  ## save, and return
  midsobj <- mids(
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
    modeltype = modeltype,
    post = post,
    blots = blots,
    ignore = ignore,
    seed = seed,
    iteration = q$iteration,
    lastSeedValue = get(".Random.seed",
      envir = globalenv(), mode = "integer",
      inherits = FALSE),
    chainMean = q$chainMean,
    chainVar = q$chainVar,
    loggedEvents = loggedEvents)

  if (!is.null(midsobj$loggedEvents)) {
    warning("Number of logged events: ", nrow(midsobj$loggedEvents),
      call. = FALSE
    )
  }
  return(midsobj)
}
