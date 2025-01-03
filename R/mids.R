#' Multiply imputed data set (`mids`)
#'
#' The `mids` object is an S3 class that represents a multiply imputed data set.
#' The `mids()` function is the S3 constructor.
#'
#' The following functions produce a `mids` object:
#' `mids()`, [mice()], [mice.mids()], [cbind()], [rbind()], [ibind()], [as.mids()] and [filter()].
#'
#' The S3 class `mids` has the following methods:
#' [bwplot()], [complete()], [densityplot()], `plot()`, `print()`,
#' [stripplot()], `summary()`, [with()], and [xyplot()].
#'
#' @inheritParams mice
#' @param imp Calculated field.
#' @param call Calculated field.
#' @param nmis Calculated field.
#' @param iteration Calculated field.
#' @param lastSeedValue Calculated field.
#' @param chainMean Calculated field.
#' @param chainVar Calculated field.
#' @param loggedEvents Calculated field.
#' @param version Calculated field.
#' @param date Calculated field.
#' @return `mids()` returns a `mids` object.
#'
#' @section Structure:
#' Objects of class `"mids"` are lists with the following elements:
#'
#' - `data`: Original (incomplete) data set.
#' - `imp`: A list of `ncol(data)` components with the generated multiple imputations.
#'   Each list component is a `data.frame` (`nmis[j]` by `m`) of imputed values for variable `j`.
#'   A `NULL` component is used for variables for which no imputations are generated.
#' - `m`: Number of imputations.
#' - `where`: The `where` argument of the [mice()] function.
#' - `blocks`: The `blocks` argument of the [mice()] function.
#' - `call`: Call that created the object.
#' - `nmis`: A named vector with counts of missing values per variable.
#' - `method`: A vector of strings of `length(blocks)` specifying the imputation method per block.
#' - `predictorMatrix`: A numerical matrix containing integers specifying the predictor set.
#' - `visitSequence`: A vector of variable and block names that specifies how variables and blocks
#'   are visited in one iteration through the data.
#' - `formulas`: A named list of formulas, or expressions that can be converted into formulas
#'   by `as.formula()`. List elements correspond to blocks. The block to which the list element applies
#'   is identified by its name, so list names must correspond to block names.
#' - `post`: A vector of strings of length `length(blocks)` with commands for post-processing.
#' - `blots`: "Block dots". The `blots` argument to the [mice()] function.
#' - `ignore`: A logical vector of length `nrow(data)` indicating the rows in `data` used to build
#'   the imputation model. (New in `mice 3.12.0`.)
#' - `seed`: The seed value of the solution.
#' - `iteration`: Last Gibbs sampling iteration number.
#' - `lastSeedValue`: Random number generator state.
#' - `chainMean`: An array of dimensions `ncol` by `maxit` by `m` containing the mean of the
#'   generated multiple imputations. The array can be used for monitoring convergence.
#'   Note that observed data are not present in this mean.
#' - `chainVar`: An array with similar structure as `chainMean`, containing the variance of
#'   the imputed values.
#' - `loggedEvents`: A `data.frame` with five columns containing warnings, corrective actions,
#'   and other inside info.
#' - `version`: Version number of the `mice` package that created the object.
#' - `date`: Date at which the object was created.
#'
#' @section LoggedEvents:
#' The `loggedEvents` entry is a matrix with five columns containing a record of automatic removal actions.
#' It is `NULL` if no action was made. At initialization, the program removes constant variables and variables
#' that cause collinearity. During iteration, the program does the following actions:
#'
#' - One or more variables that are linearly dependent are removed (for categorical data, a 'variable'
#'   corresponds to a dummy variable).
#' - Proportional odds regression imputation that does not converge is replaced by `polyreg`.
#'
#' Explanation of elements in `loggedEvents`:
#'
#' - `it`: Iteration number at which the record was added.
#' - `im`: Imputation number.
#' - `dep`: Name of the dependent variable.
#' - `meth`: Imputation method used.
#' - `out`: A (possibly long) character vector with the names of the altered or removed predictors.
#'
#' @section Methods:
#' The `mids` class of objects has methods for the following generic functions:
#' `print()`, `summary()`, `plot()`.
#'
#' @author Stef van Buuren, Karin Groothuis-Oudshoorn
#' @seealso [mice()], [mira()], [mipo()], [lattice::xyplot()]
#' @references
#' - van Buuren, S., & Groothuis-Oudshoorn, K. (2011). `mice`: Multivariate Imputation by Chained Equations in `R`.
#'   *Journal of Statistical Software*, **45**(3), 1–67. \doi{10.18637/jss.v045.i03}.
#'
#' @name mids
#' @aliases mids mids-class
#' @keywords classes
#' @examples
#' data <- data.frame(a = c(1, NA, 3), b = c(NA, 2, 3))
#' q <- list(
#'   a = structure(
#'     list(`1` = 3, `2` = 3, `3` = 3, `4` = 3, `5` = 3),
#'          row.names = "2", class = "data.frame"),
#'   b = structure(
#'     list(`1` = 3, `2` = 3, `3` = 2, `4` = 2, `5` = 3),
#'          row.names = "1", class = "data.frame"))
#'
#' imp <- mids(
#'   data = data,
#'   imp = q,
#'   m = 5,
#'   where = is.na(data),
#'   blocks = list(a = "a", b = "b"),
#'   nmis = colSums(is.na(data)),
#'   method = c(a = "mean", b = "norm"),
#'   predictorMatrix = matrix(1, nrow = 2, ncol = 2, dimnames = list(c("a", "b"), c("a", "b"))),
#'   visitSequence = c("a", "b"),
#'   formulas = list(a = a ~ b, b = b ~ a),
#'   post = NULL,
#'   blots = NULL,
#'   ignore = logical(nrow(data)),
#'   seed = 123,
#'   iteration = 1,
#'   chainMean = list(a = c(1, 2, 3), b = c(3, 2, 1)),
#'   chainVar = list(a = c(1.1, 1.2, 1.3), b = c(0.9, 1.0, 1.1)),
#'   loggedEvents = NULL)
#'
#' print(imp)
#' @export
mids <- function(
    data = data.frame(),
    imp = list(),
    m = integer(),
    where = matrix,
    blocks = list(),
    call = match.call(),
    nmis = integer(),
    method = character(),
    predictorMatrix = matrix(),
    visitSequence = character(),
    formulas = list(),
    modeltype = character(),
    post = character(),
    blots = list(),
    ignore = logical(),
    seed = integer(),
    iteration = integer(),
    lastSeedValue = tryCatch(
      get(".Random.seed", envir = globalenv(), mode = "integer", inherits = FALSE),
      error = function(e) NULL
    ),
    chainMean = list(),
    chainVar = list(),
    loggedEvents = data.frame(),
    version = packageVersion("mice"),
    date = Sys.Date()) {
  obj <- list(
    data = data,
    imp = imp,
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
    iteration = iteration,
    lastSeedValue = lastSeedValue,
    chainMean = chainMean,
    chainVar = chainVar,
    loggedEvents = loggedEvents,
    version = packageVersion("mice"),
    date = Sys.Date()
  )
  class(obj) <- "mids"
  return(obj)
}

#' Plot the trace lines of the MICE algorithm
#'
#' @section Plot:
#' The `plot()` method plots the trace lines of the MICE algorithm.
#' The `plot` method for a `mids` object displays the mean imputed
#' value per imputation and the mean standard deviation of the imputed
#' values against the iteration number for each of the *m* replications.
#' By default, the function creates a plot for each incomplete
#' variable. On convergence, the streams should intermingle and be free
#' of any trend.
#'
#' @param x An object of class `mids`.
#' @param y A formula that specifies which variables, streams, and iterations are plotted.
#'   If omitted, all streams, variables, and iterations are plotted.
#' @param theme The trellis theme to apply to the graphs. The default is `mice.theme()`.
#' @param layout A vector of length 2 specifying the number of columns and rows in the plot.
#'   The default is `c(2, 3)`.
#' @param type Parameter `type` of [lattice::panel.xyplot()].
#' @param col Parameter `col` of [lattice::panel.xyplot()].
#' @param lty Parameter `lty` of [lattice::panel.xyplot()].
#' @param ... Other arguments.
#' @return `plot()` returns a [lattice::xyplot()] object.
#' @method plot mids
#' @rdname mids
#' @examples
#' imp <- mice(nhanes, print = FALSE)
#' plot(imp, bmi + chl ~ .it | .ms, layout = c(2, 1))
#' @export
plot.mids <- function(x, y = NULL, theme = mice.theme(), layout = c(2, 3),
                      type = "l", col = 1:10, lty = 1, ...) {
  strip.combined <- function(which.given, which.panel, factor.levels, ...) {
    if (which.given == 1) {
      lattice::panel.rect(0, 0, 1, 1,
                          col = theme$strip.background$col, border = 1
      )
      lattice::panel.text(
        x = 0, y = 0.5, pos = 4,
        lab = factor.levels[which.panel[which.given]]
      )
    }
    if (which.given == 2) {
      lattice::panel.text(
        x = 1, y = 0.5, pos = 2,
        lab = factor.levels[which.panel[which.given]]
      )
    }
  }

  call <- match.call()
  if (!is.mids(x)) {
    stop("argument 'x' must be a 'mids' object", call. = FALSE)
  }
  if (is.null(x$chainMean)) {
    stop("no convergence diagnostics found", call. = FALSE)
  }

  mn <- x$chainMean
  sm <- sqrt(x$chainVar)

  # select subset of nonmissing entries
  obs <- apply(!(is.nan(mn) | is.na(mn)), 1, all)
  varlist <- names(obs)[obs]

  ## create formula if not given in y
  if (missing(y)) {
    formula <- as.formula(paste0(
      paste0(varlist, collapse = "+"),
      "~.it|.ms"
    ))
  } else {
    formula <- NULL
    if (is.null(y)) {
      formula <- as.formula(paste0(
        paste0(varlist, collapse = "+"),
        "~.it|.ms"
      ))
    }
    if (is.character(y)) {
      formula <- if (length(y) == 1) {
        as.formula(paste0(y, "~.it|.ms"))
      } else {
        as.formula(paste0(paste0(y, collapse = "+"), "~.it|.ms"))
      }
    }
    if (is.integer(y) || is.logical(y)) {
      vars <- varlist[y]
      formula <- if (length(vars) == 1) {
        as.formula(paste0(vars, "~.it|.ms"))
      } else {
        as.formula(paste0(paste0(vars, collapse = "+"), "~.it|.ms"))
      }
    }
    if (is.null(formula)) {
      formula <- as.formula(y)
    }
  }

  m <- x$m
  it <- x$iteration
  mn <- matrix(aperm(mn[varlist, , , drop = FALSE], c(2, 3, 1)), nrow = m * it)
  sm <- matrix(aperm(sm[varlist, , , drop = FALSE], c(2, 3, 1)), nrow = m * it)

  adm <- expand.grid(seq_len(it), seq_len(m), c("mean", "sd"))
  data <- cbind(adm, rbind(mn, sm))
  colnames(data) <- c(".it", ".m", ".ms", varlist)
  ## Dummy to trick R CMD check
  .m <- NULL
  rm(.m)

  tp <- lattice::xyplot(
    x = formula, data = data, groups = .m,
    type = type, lty = lty, col = col, layout = layout,
    scales = list(
      y = list(relation = "free"),
      x = list(alternating = FALSE)
    ),
    as.table = TRUE,
    xlab = "Iteration",
    ylab = "",
    strip = strip.combined,
    par.strip.text = list(lines = 0.5),
    ...
  )
  update(tp, par.settings = theme)
}

#' @rdname mids
#' @return `print()` returns the input object invisibly.
#' @method print mids
#' @export
print.mids <- function(x, ...) {
  cat("Class: mids\n")
  cat("Number of multiple imputations: ", x$m, "\n")
  cat("Imputation methods:\n")
  print(x$method, ...)
  cat("PredictorMatrix:\n")
  print(head(x$predictorMatrix), ...)
  if (!is.null(x$loggedEvents)) {
    cat("Number of logged events: ", nrow(x$loggedEvents), "\n")
    print(head(x$loggedEvents), ...)
  }
  invisible(x)
}

#' @rdname mids
#' @param object Object of class `mids`.
#' @return `summary()` returns the input object invisibly.
#' @method summary mids
#' @export
summary.mids <- function(object, ...) {
  print(object, ...)
  invisible(object)
}
