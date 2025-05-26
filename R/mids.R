#' Multiply imputed data set (\code{mids})
#'
#' The \code{mids} object is an S3 class that represents a multiply imputed
#' data set. The \code{mids()} function is the S3 constructor.
#' The following functions produce a \code{mids} object:
#' \code{mids()}, \code{\link{mice}()}, \code{\link{mice.mids}()},
#' \code{\link{cbind}()}, \code{\link{rbind}()}, \code{\link{ibind}()},
#' \code{\link{as.mids}()} and \code{\link{filter}()}.
#'
#' The S3 class \code{mids} has the following methods:
#' \code{\link{bwplot}()}, \code{\link{complete}()},
#' \code{\link{densityplot}()}, \code{plot()},
#' \code{print()}, \code{\link{stripplot}()}, \code{summary()},
#' \code{\link{with}()} and \code{\link{xyplot}()}.
#'
#' @inheritParams mice
#' @param imp Calculated field
#' @param call Calculated field
#' @param nmis Calculated field
#' @param iteration Calculated field
#' @param lastSeedValue Calculated field
#' @param chainMean Calculated field
#' @param chainVar Calculated field
#' @param loggedEvents Calculated field
#' @param version Calculated field
#' @param date Calculated field
#' @return
#' \code{mids()} returns a \code{mids} object.
#'
#' @section Structure:
#' Objects of class \code{"mids"} are lists with the following elements:
#'  \describe{
#'    \item{\code{data}:}{Original (incomplete) data set.}
#'    \item{\code{imp}:}{A list of \code{ncol(data)} components with
#'    the generated multiple imputations. Each list component is a
#'    \code{data.frame} (\code{nmis[j]} by \code{m}) of imputed values
#'    for variable \code{j}. A \code{NULL} component is used for
#'    variables for which not imputations are generated.}
#'    \item{\code{m}:}{Number of imputations.}
#'    \item{\code{where}:}{The \code{where} argument of the
#'    \code{mice()} function.}
#'    \item{\code{blocks}:}{The \code{blocks} argument of the
#'    \code{mice()} function.}
#'    \item{\code{call}:}{Call that created the object.}
#'    \item{\code{nmis}:}{An Named vector with counts of missing values per variable}
#'    \item{\code{method}:}{A vector of strings of \code{length(blocks}
#'    specifying the imputation method per block.}
#'    \item{\code{predictorMatrix}:}{A numerical matrix of containing
#'    integers specifying the predictor set.}
#'    \item{\code{visitSequence}:}{A vector of variable and block names that
#'     specifies how variables and blocks are visited in one iteration throuh
#'     the data.}
#'    \item{\code{formulas}:}{A named list of formula's, or expressions that
#'    can be converted into formula's by \code{as.formula}. List elements
#'    correspond to blocks. The block to which the list element applies is
#'    identified by its name, so list names must correspond to block names.}
#'    \item{\code{post}:}{A vector of strings of length \code{length(blocks)}
#'    with commands for post-processing.}
#'    \item{\code{blots}:}{"Block dots". The \code{blots} argument to the \code{mice()}
#'    function.}
#'    \item{\code{ignore}:}{A logical vector of length \code{nrow(data)} indicating
#'    the rows in \code{data} used to build the imputation model. (new in \code{mice 3.12.0})}
#'    \item{\code{seed}:}{The seed value of the solution.}
#'    \item{\code{iteration}:}{Last Gibbs sampling iteration number.}
#'    \item{\code{lastSeedValue}:}{Random number generator state.}
#'    \item{\code{chainMean}:}{An array of dimensions \code{ncol} by
#'    \code{maxit} by \code{m} elements containing the mean of
#'    the generated multiple imputations.
#'    The array can be used for monitoring convergence.
#'    Note that observed data are not present in this mean.}
#'    \item{\code{chainVar}:}{An array with similar structure as
#'    \code{chainMean}, containing the variance of the imputed values.}
#'    \item{\code{loggedEvents}:}{A \code{data.frame} with five columns
#'    containing warnings, corrective actions, and other inside info.}
#'    \item{\code{version}:}{Version number of \code{mice} package that
#'    created the object.}
#'    \item{\code{date}:}{Date at which the object was created.}
#' }
#'
#' @section LoggedEvents:
#' The \code{loggedEvents} entry is a matrix with five columns containing a
#' record of automatic removal actions. It is \code{NULL} is no action was
#' made.  At initialization the program removes constant variables, and
#' removes variables to cause collinearity.
#' During iteration, the program does the following actions:
#' \itemize{
#'     \item One or more variables that are linearly dependent are removed
#' (for categorical data, a 'variable' corresponds to a dummy variable)
#'     \item  Proportional odds regression imputation that does not converge
#' and is replaced by \code{polyreg}.
#' }
#'
#' Explanation of elements in \code{loggedEvents}:
#' \describe{
#' \item{\code{it}}{iteration number at which the record was added,}
#' \item{\code{im}}{imputation number,}
#' \item{\code{dep}}{name of the dependent variable,}
#' \item{\code{meth}}{imputation method used,}
#' \item{\code{out}}{a (possibly long) character vector with the
#' names of the altered or removed predictors.}
#' }
#' @section Methods:
#' The \code{mids} class of objects has methods for the following
#' generic functions: \code{print}, \code{summary}, \code{plot}.
#' @author Stef van Buuren, Karin Groothuis-Oudshoorn
#' @seealso \code{\link{mice}}, \code{\link{mira}},
#' \code{\link{mipo}}, \code{\link[lattice]{xyplot}}
#' @references van Buuren S and Groothuis-Oudshoorn K (2011). \code{mice}:
#' Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#' Statistical Software}, \bold{45}(3), 1-67.
#' \doi{10.18637/jss.v045.i03}
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
    calltype = character(),
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
    calltype = calltype,
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
#' The \code{plot()} metho plots the trace lines of the MICE algorithm.
#' The \code{plot} method for a \code{mids} object plots the mean imputed
#' value per imputation and the mean standard deviation of the imputed
#' values against the iteration number for each of the $m$ replications.
#' By default, the function creates a plot for each incomplete
#' variable. On convergence, the streams should intermingle and be free
#' of any trend.
#'
#' @param x      An object of class \code{mids}
#' @param y      A formula that specifies which variables, stream and iterations are plotted.
#'               If omitted, all streams, variables and iterations are plotted.
#' @param theme  The trellis theme to applied to the graphs. The default is \code{mice.theme()}.
#' @param layout A vector of length 2 given the number of columns and rows in the plot.
#'               The default is \code{c(2, 3)}.
#' @param type   Parameter \code{type} of \code{\link[lattice]{panel.xyplot}}.
#' @param col    Parameter \code{col} of \code{\link[lattice]{panel.xyplot}}.
#' @param lty    Parameter \code{lty} of \code{\link[lattice]{panel.xyplot}}.
#' @param \dots  Others arguments
#' @return \code{plot()} returns a \code{\link[lattice]{xyplot}} object.
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
#' @return \code{print()} returns the input object invisibly.
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
#' @param object Object of class \code{mids}
#' @return \code{summary()} returns the input object invisibly.
#' @method summary mids
#' @export
summary.mids <- function(object, ...) {
  print(object, ...)
  invisible(object)
}
