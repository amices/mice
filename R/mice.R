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
#' `mnar.norm`         \tab numeric \tab NARFCS under user-specified MNAR\cr
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
#' Built-in multivariate imputation methods are:
#'
#' \tabular{lll}{
#' `mpmm`               \tab any     \tab Multivariate PMM\cr
#' `jomoImpute`         \tab any     \tab `jomo::jomo()` through `mitml::jomoImpute()`\cr
#' `panImpute`          \tab numeric \tab `pan::pan()` through `mitml::panImpute()`
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
#'                  A square numeric matrix of maximal \eqn{p} rows and
#'                  maximal \eqn{p} columns. Row- and column names are
#'                  `colnames(data)`.
#'                  Each row corresponds to a variable to be imputed.
#'                  A value of `1` means that the column variable is a
#'                  predictor for the row variable, while a `0` means that
#'                  the column variable is not a predictor. The default
#'                  `predictorMatrix` is `1` everywhere, except for a zero
#'                  diagonal. Row- and column-names are optional for the
#'                  maximum \eqn{p} by \eqn{p} size. The user may specify a
#'                  smaller `predictorMatrix`, but column and row names are
#'                  then mandatory and should match be part of `colnames(data)`.
#'                  For variables that are not imputed, `mice()` automatically
#'                  sets the corresponding rows in the `predictorMatrix` to
#'                  zero. See details on *skipping imputation*.
#'                  Two-level imputation models (which have `"2l"` in their
#'                  names) support other codes than `0` and `1`, e.g, `2`
#'                  or `-2` that assign special roles to some variables.
#' @param ignore    A logical vector of \eqn{n} elements indicating
#'                  which rows are ignored for estimating the parameters of
#'                  the imputation model.
#'                  Rows with `ignore` set to `TRUE` do not influence the
#'                  parameters of the imputation model.
#'                  The `ignore` argument allows splitting `data` into a
#'                  training set (on which `mice()` fits the imputation model)
#'                  and a test set (that does not influence the imputation
#'                  model parameter estimates).
#'                  The default `NULL` corresponds to all `FALSE`, thus
#'                  including all rows into the imputation models.
#'                  Note: Not all imputation methods may support the `ignore`
#'                  argument (e.g., `mice.impute.jomoImpute()` or
#'                  `mice.impute.panImpute()`).
#' @param where     A data frame or matrix of logicals with \eqn{n} rows
#'                  and \eqn{p} columns, indicating the cells of `data` for
#'                  which imputations are generated.
#'                  The default `where = is.na(data)` specifies that all
#'                  missing data are imputed.
#'                  The `where` argument can overimpute cells
#'                  with observed data, or skip imputation of specific missing
#'                  cells. Be aware that the latter option could propagate
#'                  missing values to other variables. See details.
#'                  Note: Not all imputation methods may support the `where`
#'                  argument (e.g., `mice.impute.jomoImpute()` or
#'                  `mice.impute.panImpute()`).
#' @param blocks    List of \eqn{q} character vectors that identifies the
#'                  variable names per block. The name of list elements
#'                  identify blocks. `mice()` will provide default names
#'                  (`"b1"`, `"b2"`, ...) for blocks containing multiple
#'                  variables. Variables within a block are imputed as a
#'                  block, e.g. by a multivariate imputation method, or
#'                  by an iterated version of the same univariate imputation
#'                  method. By default each variable is allocated to a
#'                  separate block, which is effectively fully conditional
#'                  specification (FCS) by univariate models
#'                  (variable-by-variable imputation).
#'                  All data variables are assigned to a block.
#'                  A variable can belong to only one block, so there are
#'                  at most \eqn{p} blocks.
#'                  See the `parcel` argument for an easier alternative to
#'                  the `blocks` argument.
#' @param visitSequence
#'                  A vector of block names of arbitrary length, specifying
#'                  the sequence of blocks in which blocks are imputed.
#'                  The `visitSequence` defines one iteration through the
#'                  data. A given block may be visited multiple times
#'                  within one iteration.
#'                  Variables that are members of the same block
#'                  are imputed togeteher when the block is visited.
#'                  The default `visitSequence = "roman"` visits the blocks
#'                  (left to right) in the order in which they appear
#'                  in `blocks`. One may also use one of the following
#'                  keywords: `"arabic"` (right to left), `"monotone"`
#'                  (ordered low to high proportion of missing data) and
#'                  `"revmonotone"` (reverse of monotone).
#'                  *Special case*: If you specify both
#'                  `visitSequence = "monotone"` and `maxit = 1`, then the
#'                  procedure will edit the `predictorMatrix` to conform to
#'                  the monotone pattern, so convergence is then immediate.
#'                  Realize that convergence in one iteration is only
#'                  guaranteed if the missing data pattern is actually
#'                  monotone. `mice()` does not check for monotonicity.
#' @param formulas  A named list with \eqn{q} component, each containing
#'                  one formula. The left hand side (LHS) specifies the
#'                  variables to be imputed, and the right hand side (RHS)
#'                  specifies the predictors used for imputation. For example,
#'                  model `y1 + y2 ~ x1 + x2` imputes `y1` and `y2` using `x1`
#'                  and `x2` as predictors. Imputation by a multivariate
#'                  imputation model imputes `y1` and `y2` simultaneously
#'                  by a joint model, whereas `mice()` can also impute
#'                  `y1` and `y2` by a repeated univariate model as
#'                  `y1 ~ y2 + x1 + x2` and `y2 ~ y1 + x1 + x2`.
#'                  The `formulas` argument is an alternative to the
#'                  combination of the `predictorMatrix` and
#'                  `blocks` arguments. It is more compact and allows for
#'                  more flexibility in specifying imputation models,
#'                  e.g., for adding
#'                  interaction terms (`y1 + y2 ~ x1 * x2` ),
#'                  logical variables (`y1 + y2 ~ x1 + (x2 > 20)`),
#'                  three-level categories (`y1 + y2 ~ x1 + cut(age, 3)`),
#'                  polytomous terms (`y1 + y2 ~ x1 + poly(age, 3)`,
#'                  smoothing terms (`y1 + y2 ~ x1 + bs(age)`),
#'                  sum scores (`y1 + y2 ~ I(x1 + x2)`) or
#'                  quotients (`y1 + y2 ~ I(x1 / x2)`)
#'                  on the fly.
#'                  Optionally, the user can name formulas. If not named,
#'                  `mice()` will name formulas with multiple variables
#'                  as `F1`, `F2`, and so on. Formulas with one
#'                  dependent (e.g. `ses ~ x1 + x2`) will be named
#'                  after the dependent variable `"ses"`.
#' @param dots     A named `list` with maximally \eqn{q} `alist` used to
#'                  pass down optional arguments to lower level imputation
#'                  functions.
#'                  The entries of element `dots[[h]]` are passed down to
#'                  the method called on block `h` or formula `h`.
#'                  For example, `dots = list(age = alist(donor = 20))`
#'                  specifies that imputation of `age` should draw from
#'                  imputations using 20 (instead of the default five) nearest
#'                  neighbours.
#' @param post      A vector of length \eqn{p}, each specifying an expression
#'                  as a string. The string is parsed and executed within
#'                  the `sampler()` function to post-process imputed
#'                  values during the iterations. The default is a vector
#'                  of empty strings, indicating no post-processing.
#'                  Multivariate imputation methods ignore the `post`
#'                  parameter.
#' @param defaultMethod
#'                  A vector of length 4 containing the default imputation
#'                  methods for
#'                  1) numeric data (`"pmm"`)
#'                  2) factor data with 2 levels, (`"logreg"`)
#'                  3) factor data with > 2 unordered levels, (`"polyreg"`) and
#'                  4) factor data with > 2 ordered levels (`"polr"`).
#'                  The `defaultMethod` can be used to alter to default mapping
#'                  of variable type to imputation method.
#' @param maxit     A scalar giving the number of iterations. The default is 5.
#'                  In general, the user should study the convergence of the
#'                  algorithm, e.g., by `plot(imp)`.
#' @param printFlag If `printFlag = TRUE` (default) then `mice()` will
#'                  print iteration history on the console. This is useful for
#'                  checking how far the algorithm is. Use `print = FALSE`
#'                  for silent computation, simulations, and to suppress
#'                  iteration output on the console.
#' @param seed      An integer that is used as argument by the `set.seed()`
#'                  for offsetting the random number generator. Default is
#'                  to leave the random number generator alone. Use `seed` to
#'                  be reproduce a given imputation.
#' @param data.init A data frame of the same size and type as `data`, but
#'                  without missing data, used to initialize imputations
#'                  before the start of the iterative process.
#'                  The default `data.init = NULL` generates starting
#'                  imputations by a simple random draw from marginal
#'                  of the observed data.
#'                  Note that specification of `data.init` will start all
#'                  `m` Gibbs sampling streams from the same imputation.
#' @param \dots     Named arguments that are passed down to the univariate
#'                  imputation functions. Use `dots` for a more fine-grained
#'                  alternative.
#' @param parcel      A character vector with \eqn{p} elements identifying the
#'                  variable group (or block) to which each variable is
#'                  allocated.
#' @param blots     Deprecated. Replaced by `dots`.
#' @param autoremove Logical. Should unimputed incomplete predictors be removed
#'                  to prevent NA propagation?
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
                 predictorMatrix,
                 parcel = NULL,
                 formulas,
                 method = NULL,
                 defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
                 dots = NULL,
                 visitSequence = NULL,
                 maxit = 5,
                 seed = NA,
                 data.init = NULL,
                 where = NULL,
                 ignore = NULL,
                 post = NULL,
                 printFlag = TRUE,
                 autoremove = TRUE,
                 blocks,
                 blots = NULL,
                 ...) {
  call <- match.call()

  # legacy handling
  check.deprecated(...)
  if (!missing(blots)) {
    warning("argument 'blots' is deprecated; please use 'dots' instead.",
            call. = FALSE)
    dots <- blots
  }

  # data frame for storing the event log
  state <- list(it = 0, im = 0, dep = "", meth = "", log = FALSE)
  loggedEvents <- data.frame(it = 0, im = 0, dep = "", meth = "", out = "")

  if (!is.na(seed)) set.seed(seed)

  # check form of data and m
  data <- check.dataform(data)
  m <- check.m(m)

  # add support parcel
  if (!is.null(parcel)) {
    blocks <- n2b(parcel, silent = FALSE)
  }

  # determine input combination: predictorMatrix, blocks, formulas
  mp <- missing(predictorMatrix)
  mb <- missing(blocks)
  mf <- missing(formulas)

  # case A
  if (mp & mb & mf) {
    # formulas leads
    formulas <- make.formulas(data)
    attr(formulas, "ynames") <- colnames(data)
    predictorMatrix <- f2p(formulas, data)
    blocks <- construct.blocks(formulas)
  }
  # case B
  if (!mp & mb & mf) {
    # predictorMatrix leads
    predictorMatrix <- check.predictorMatrix(predictorMatrix, data,
                                             autoremove = autoremove)
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
    formulas <- check.formulas(formulas, data, autoremove = autoremove)
    blocks <- construct.blocks(formulas)
    predictorMatrix <- f2p(formulas, data, blocks)
  }

  # case E
  if (!mp & !mb & mf) {
    # predictor leads (use for multivariate imputation)
    predictorMatrix <- check.predictorMatrix(predictorMatrix, data,
                                             autoremove = autoremove)
    blocks <- check.blocks(blocks, data, calltype = "pred")
    formulas <- make.formulas(data, blocks, predictorMatrix = predictorMatrix)
  }

  # case F
  if (!mp & mb & !mf) {
    stop("cannot process mix of 'predictorMatrix' and 'formulas' arguments",
        call. = FALSE)
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
    stop("cannot process mix of 'parcel', 'blocks' or 'formulas' arguments",
         call. = FALSE)
    blocks <- check.blocks(blocks, data)
    formulas <- check.formulas(formulas, blocks)
    predictorMatrix <- make.predictorMatrix(data, blocks)
  }

  # case H
  if (!mp & !mb & !mf) {
    # it is better to forbid this case
    # blocks lead
    stop("cannot process mix of 'predictorMatrix' and 'formulas' arguments",
         call. = FALSE)
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

  # collect the ynames (variables to impute) from the model and clean
  ynames <- collect.ynames(predictorMatrix, blocks, formulas)
  attr(predictorMatrix, "ynames") <- NULL
  attr(blocks, "ynames") <- NULL
  attr(formulas, "ynames") <- NULL

  # derive method vector
  method <- check.method(
    method = method, data = data, where = where,
    blocks = blocks, defaultMethod = defaultMethod,
    ynames)

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
  dots <- check.dots(dots, data, blocks)
  ignore <- check.ignore(ignore, data)

  # edit imputation setup
  setup <- list(
    method = method,
    formulas = formulas,
    dots = dots,
    predictorMatrix = predictorMatrix,
    visitSequence = visitSequence,
    post = post
  )
  setup <- edit.setup(data, setup, ...)
  method <- setup$method
  formulas <- setup$formulas
  dots <- setup$dots
  predictorMatrix <- setup$predictorMatrix
  visitSequence <- setup$visitSequence
  post <- setup$post

  # update parcel
  parcel <- b2n(blocks)
  parcel <- reorder.parcel(parcel, data)

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
    visitSequence, predictorMatrix, formulas, dots,
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
    parcel = parcel,
    blocks = blocks,
    call = call,
    nmis = nmis,
    method = method,
    predictorMatrix = predictorMatrix,
    visitSequence = visitSequence,
    formulas = formulas,
    post = post,
    dots = dots,
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
