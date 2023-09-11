#' Scatterplot of observed and imputed data
#'
#' Plotting methods for imputed data using \pkg{lattice}.
#' `xyplot()` produces a conditional scatterplots. The function
#' automatically separates the observed (blue) and imputed (red) data. The
#' function extends the usual features of \pkg{lattice}.
#'
#' The argument `na.groups` may be used to specify (combinations of)
#' missingness in any of the variables. The argument `groups` can be used
#' to specify groups based on the variable values themselves. Only one of both
#' may be active at the same time. When both are specified, `na.groups`
#' takes precedence over `groups`.
#'
#' Use the `subset` and `na.groups` together to plots parts of the
#' data. For example, select the first imputed data set by by
#' `subset=.imp==1`.
#'
#' Graphical parameters like `col`, `pch` and `cex` can be
#' specified in the arguments list to alter the plotting symbols. If
#' `length(col)==2`, the color specification to define the observed and
#' missing groups. `col[1]` is the color of the 'observed' data,
#' `col[2]` is the color of the missing or imputed data. A convenient color
#' choice is `col=mdc(1:2)`, a transparent blue color for the observed
#' data, and a transparent red color for the imputed data. A good choice is
#' `col=mdc(1:2), pch=20, cex=1.5`. These choices can be set for the
#' duration of the session by running `mice.theme()`.
#'
#' @aliases xyplot
#' @param x A `mids` object, typically created by `mice()` or
#' `mice.mids()`.
#' @param data Formula that selects the data to be plotted.  This argument
#' follows the \pkg{lattice} rules for *formulas*, describing the primary
#' variables (used for the per-panel display) and the optional conditioning
#' variables (which define the subsets plotted in different panels) to be used
#' in the plot.
#'
#' The formula is evaluated on the complete data set in the `long` form.
#' Legal variable names for the formula include `names(x$data)` plus the
#' two administrative factors `.imp` and `.id`.
#'
#' **Extended formula interface:** The primary variable terms (both the LHS
#' `y` and RHS `x`) may consist of multiple terms separated by a
#' \sQuote{+} sign, e.g., `y1 + y2 ~ x | a * b`.  This formula would be
#' taken to mean that the user wants to plot both `y1 ~ x | a * b` and
#' `y2 ~ x | a * b`, but with the `y1 ~ x` and `y2 ~ x` in
#' *separate panels*. This behavior differs from standard \pkg{lattice}.
#' *Only combine terms of the same type*, i.e. only factors or only
#' numerical variables. Mixing numerical and categorical data occasionally
#' produces odds labeling of vertical axis.
#'
#' @param na.groups An expression evaluating to a logical vector indicating
#' which two groups are distinguished (e.g. using different colors) in the
#' display. The environment in which this expression is evaluated in the
#' response indicator `is.na(x$data)`.
#'
#' The default `na.group = NULL` contrasts the observed and missing data
#' in the LHS `y` variable of the display, i.e. groups created by
#' `is.na(y)`. The expression `y` creates the groups according to
#' `is.na(y)`. The expression `y1 & y2` creates groups by
#' `is.na(y1) & is.na(y2)`, and `y1 | y2` creates groups as
#' `is.na(y1) | is.na(y2)`, and so on.
#' @param groups This is the usual `groups` arguments in \pkg{lattice}. It
#' differs from `na.groups` because it evaluates in the completed data
#' `data.frame(complete(x, "long", inc=TRUE))` (as usual), whereas
#' `na.groups` evaluates in the response indicator. See
#' [xyplot()] for more details. When both `na.groups` and
#' `groups` are specified, `na.groups` takes precedence, and
#' `groups` is ignored.
#' @param theme A named list containing the graphical parameters. The default
#' function `mice.theme` produces a short list of default colors, line
#' width, and so on. The extensive list may be obtained from
#' `trellis.par.get()`. Global graphical parameters like `col` or
#' `cex` in high-level calls are still honored, so first experiment with
#' the global parameters. Many setting consists of a pair. For example,
#' `mice.theme` defines two symbol colors. The first is for the observed
#' data, the second for the imputed data. The theme settings only exist during
#' the call, and do not affect the trellis graphical parameters.
#' @param as.table See [lattice::xyplot()].
#' @param outer See [lattice::xyplot()].
#' @param allow.multiple See [lattice::xyplot()].
#' @param drop.unused.levels See [lattice::xyplot()].
#' @param subscripts See [lattice::xyplot()].
#' @param subset See [lattice::xyplot()].
#' @param \dots Further arguments, usually not directly processed by the
#' high-level functions documented here, but instead passed on to other
#' functions.
#' @return The high-level functions documented here, as well as other high-level
#' Lattice functions, return an object of class `"trellis"`.  The
#' [`update()`][lattice::update.trellis] method can be used to
#' subsequently update components of the object, and the
#' [`print()`][lattice::print.trellis] method (usually called by default)
#' will plot it on an appropriate plotting device.
#' @note The first two arguments (`x` and `data`) are reversed
#' compared to the standard Trellis syntax implemented in \pkg{lattice}. This
#' reversal was necessary in order to benefit from automatic method dispatch.
#'
#' In \pkg{mice} the argument `x` is always a `mids` object, whereas
#' in \pkg{lattice} the argument `x` is always a formula.
#'
#' In \pkg{mice} the argument `data` is always a formula object, whereas in
#' \pkg{lattice} the argument `data` is usually a data frame.
#'
#' All other arguments have identical interpretation.
#'
#' @author Stef van Buuren
#' @seealso [mice()], [stripplot()], [densityplot()],
#' [bwplot()], [lattice()] for an overview of the
#' package, as well as [lattice::xyplot()],
#' [lattice::panel.xyplot()],
#' [lattice::print.trellis()],
#' [`trellis.par.set()`][lattice::trellis.par.get]
#' @references Sarkar, Deepayan (2008) *Lattice: Multivariate Data
#' Visualization with R*, Springer.
#'
#' van Buuren S and Groothuis-Oudshoorn K (2011). `mice`: Multivariate
#' Imputation by Chained Equations in `R`. *Journal of Statistical
#' Software*, **45**(3), 1-67. \doi{10.18637/jss.v045.i03}
#' @keywords hplot
#' @examples
#' imp <- mice(boys, maxit = 1)
#'
#' # xyplot: scatterplot by imputation number
#' # observe the erroneous outlying imputed values
#' # (caused by imputing hgt from bmi)
#' xyplot(imp, hgt ~ age | .imp, pch = c(1, 20), cex = c(1, 1.5))
#'
#' # same, but label with missingness of wgt (four cases)
#' xyplot(imp, hgt ~ age | .imp, na.group = wgt, pch = c(1, 20), cex = c(1, 1.5))
#' @export
xyplot.mids <- function(x,
                        data,
                        na.groups = NULL,
                        groups = NULL,
                        as.table = TRUE,
                        theme = mice.theme(),
                        allow.multiple = TRUE,
                        outer = TRUE,
                        drop.unused.levels = lattice::lattice.getOption("drop.unused.levels"),
                        ...,
                        subscripts = TRUE,
                        subset = TRUE) {
  call <- match.call()
  if (!is.mids(x)) stop("Argument 'x' must be a 'mids' object")
  if (missing(data)) stop("Missing formula")
  formula <- data

  ## unpack data and response indicator
  cd <- data.frame(complete(x, "long", include = TRUE))
  r <- as.data.frame(is.na(x$data))

  ## evaluate na.group in response indicator
  nagp <- eval(expr = substitute(na.groups), envir = r, enclos = parent.frame())
  if (is.expression(nagp)) nagp <- eval(expr = nagp, envir = r, enclos = parent.frame())

  ## evaluate groups in imputed data
  ngp <- eval(expr = substitute(groups), envir = cd, enclos = parent.frame())
  if (is.expression(ngp)) ngp <- eval(expr = ngp, envir = cd, enclos = parent.frame())
  groups <- ngp

  ## evaluate subset in imputed data
  ss <- eval(expr = substitute(subset), envir = cd, enclos = parent.frame())
  if (is.expression(ss)) ss <- eval(expr = ss, envir = cd, enclos = parent.frame())
  subset <- ss

  ## evaluate further arguments before parsing
  dots <- list(...)
  args <- list(
    allow.multiple = allow.multiple,
    outer = outer,
    drop.unused.levels = drop.unused.levels,
    subscripts = subscripts,
    as.table = as.table
  )

  ## determine the y-variables
  form <- lattice::latticeParseFormula(
    model = formula, data = cd, subset = subset,
    groups = groups, multiple = allow.multiple,
    outer = outer, subscripts = TRUE,
    drop = drop.unused.levels
  )
  ynames <- unlist(lapply(strsplit(form$left.name, " \\+ "), rm.whitespace))

  ## calculate selection vector gp
  nona <- is.null(call$na.groups)
  if (!is.null(call$groups) && nona) {
    gp <- call$groups
  } else {
    if (nona) {
      na.df <- r[, ynames, drop = FALSE]
      gp <- unlist(lapply(na.df, rep.int, x$m + 1))
    } else {
      gp <- rep.int(nagp, length(ynames) * (x$m + 1))
    }
  }

  ## change axis defaults of extended formula interface
  if (is.null(call$ylab)) {
    args$ylab <- ""
    if (length(ynames) == 1) args$ylab <- ynames
  }
  if (is.null(call$scales)) {
    args$scales <- list()
    if (length(ynames) > 1) {
      args$scales <- list(
        x = list(relation = "free"),
        y = list(relation = "free")
      )
    }
  }

  ## ready
  args <- c(
    x = formula, data = list(cd),
    groups = list(gp),
    args, dots, subset = call$subset
  )

  ## go
  tp <- do.call("xyplot", args)
  update(tp, par.settings = theme)
}
