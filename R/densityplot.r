#'Density plot of observed and imputed data
#'
#'Plotting methods for imputed data using \pkg{lattice}. \code{densityplot} 
#'produces plots of the densities. The function
#'automatically separates the observed and imputed data. The
#'functions extend the usual features of \pkg{lattice}.
#'
#'The argument \code{na.groups} may be used to specify (combinations of)
#'missingness in any of the variables. The argument \code{groups} can be used
#'to specify groups based on the variable values themselves. Only one of both
#'may be active at the same time. When both are specified, \code{na.groups}
#'takes precedence over \code{groups}.
#'
#'Use the \code{subset} and \code{na.groups} together to plots parts of the
#'data. For example, select the first imputed data set by by
#'\code{subset=.imp==1}.
#'
#'Graphical paramaters like \code{col}, \code{pch} and \code{cex} can be
#'specified in the arguments list to alter the plotting symbols. If
#'\code{length(col)==2}, the color specification to define the observed and
#'missing groups. \code{col[1]} is the color of the 'observed' data,
#'\code{col[2]} is the color of the missing or imputed data. A convenient color
#'choice is \code{col=mdc(1:2)}, a transparent blue color for the observed
#'data, and a transparent red color for the imputed data. A good choice is
#'\code{col=mdc(1:2), pch=20, cex=1.5}. These choices can be set for the
#'duration of the session by running \code{mice.theme()}.
#'
#'@aliases densityplot
#'@param x A \code{mids} object, typically created by \code{mice()} or
#'\code{mice.mids()}.
#'@param data Formula that selects the data to be plotted.  This argument
#'follows the \pkg{lattice} rules for \emph{formulas}, describing the primary
#'variables (used for the per-panel display) and the optional conditioning
#'variables (which define the subsets plotted in different panels) to be used
#'in the plot.
#'
#'The formula is evaluated on the complete data set in the \code{long} form.
#'Legal variable names for the formula include \code{names(x$data)} plus the
#'two administrative factors \code{.imp} and \code{.id}.
#'
#'\bold{Extended formula interface:} The primary variable terms (both the LHS
#'\code{y} and RHS \code{x}) may consist of multiple terms separated by a
#'\sQuote{+} sign, e.g., \code{y1 + y2 ~ x | a * b}.  This formula would be
#'taken to mean that the user wants to plot both \code{y1 ~ x | a * b} and
#'\code{y2 ~ x | a * b}, but with the \code{y1 ~ x} and \code{y2 ~ x} in
#'\emph{separate panels}. This behavior differs from standard \pkg{lattice}.
#'\emph{Only combine terms of the same type}, i.e. only factors or only
#'numerical variables. Mixing numerical and categorical data occasionally
#'produces odds labeling of vertical axis.
#'
#'The function \code{densityplot} does not use the \code{y} terms in the
#'formula. Density plots for \code{x1} and \code{x2} are requested as \code{~
#'x1 + x2}.
#'@param na.groups An expression evaluating to a logical vector indicating
#'which two groups are distinguished (e.g. using different colors) in the
#'display. The environment in which this expression is evaluated in the
#'response indicator \code{is.na(x$data)}.
#'
#'The default \code{na.group = NULL} constrasts the observed and missing data
#'in the LHS \code{y} variable of the display, i.e. groups created by
#'\code{is.na(y)}. The expression \code{y} creates the groups according to
#'\code{is.na(y)}. The expression \code{y1 & y2} creates groups by
#'\code{is.na(y1) & is.na(y2)}, and \code{y1 | y2} creates groups as
#'\code{is.na(y1) | is.na(y2)}, and so on.
#'@param groups This is the usual \code{groups} arguments in \pkg{lattice}. It
#'differs from \code{na.groups} because it evaluates in the completed data
#'\code{data.frame(complete(x, "long", inc=TRUE))} (as usual), whereas
#'\code{na.groups} evaluates in the response indicator. See
#'\code{\link{xyplot}} for more details. When both \code{na.groups} and
#'\code{groups} are specified, \code{na.groups} takes precedence, and
#'\code{groups} is ignored.
#'@param plot.points A logical used in \code{densityplot} that signals whether
#'the points should be plotted.
#'@param theme A named list containing the graphical parameters. The default
#'function \code{mice.theme} produces a short list of default colors, line
#'width, and so on. The extensive list may be obtained from
#'\code{trellis.par.get()}. Global graphical parameters like \code{col} or
#'\code{cex} in high-level calls are still honored, so first experiment with
#'the global parameters. Many setting consists of a pair. For example,
#'\code{mice.theme} defines two symbol colors. The first is for the observed
#'data, the second for the imputed data. The theme settings only exist during
#'the call, and do not affect the trellis graphical parameters.
#'@param mayreplicate A logical indicating whether color, line widths, and so
#'on, may be replicated. The graphical functions attempt to choose
#'"intelligent" graphical parameters. For example, the same color can be
#'replicated for different element, e.g. use all reds for the imputed data.
#'Replication may be switched off by setting the flag to \code{FALSE}, in order
#'to allow the user to gain full control.
#'@param thicker Used in \code{densityplot}. Multiplication factor of the line
#'width of the observed density. \code{thicker=1} uses the same thickness for
#'the observed and imputed data.
#'@param as.table See \code{\link[lattice:xyplot]{xyplot}}.
#'@param panel See \code{\link{xyplot}}.
#'@param default.prepanel See \code{\link[lattice:xyplot]{xyplot}}.
#'@param outer See \code{\link[lattice:xyplot]{xyplot}}.
#'@param allow.multiple See \code{\link[lattice:xyplot]{xyplot}}.
#'@param drop.unused.levels See \code{\link[lattice:xyplot]{xyplot}}.
#'@param subscripts See \code{\link[lattice:xyplot]{xyplot}}.
#'@param subset See \code{\link[lattice:xyplot]{xyplot}}.
#'@param \dots Further arguments, usually not directly processed by the
#'high-level functions documented here, but instead passed on to other
#'functions.
#'@return The high-level functions documented here, as well as other high-level
#'Lattice functions, return an object of class \code{"trellis"}.  The
#'\code{\link[lattice:update.trellis]{update}} method can be used to
#'subsequently update components of the object, and the
#'\code{\link[lattice:print.trellis]{print}} method (usually called by default)
#'will plot it on an appropriate plotting device.
#'@note The first two arguments (\code{x} and \code{data}) are reversed
#'compared to the standard Trellis syntax implemented in \pkg{lattice}. This
#'reversal was necessary in order to benefit from automatic method dispatch.
#'
#'In \pkg{mice} the argument \code{x} is always a \code{mids} object, whereas
#'in \pkg{lattice} the argument \code{x} is always a formula.
#'
#'In \pkg{mice} the argument \code{data} is always a formula object, whereas in
#'\pkg{lattice} the argument \code{data} is usually a data frame.
#'
#'All other arguments have identical interpretation.
#'
#'\code{densityplot} errs on empty groups, which occurs if all observations in
#'the subgroup contain \code{NA}. The relevant error message is: \code{Error in
#'density.default: ... need at least 2 points to select a bandwidth
#'automatically}. There is yet no workaround for this problem. Use the more
#'robust \code{bwplot} or \code{stripplot} as a replacement.
#'@author Stef van Buuren
#'@seealso \code{\link{mice}}, \code{\link{xyplot}}, \code{\link{stripplot}},
#'\code{\link{bwplot}}, \code{\link{Lattice}} for an overview of the
#'package, as well as \code{\link[lattice:densityplot]{densityplot}},
#'\code{\link[lattice:panel.densityplot]{panel.densityplot}},
#'\code{\link[lattice:print.trellis]{print.trellis}},
#'\code{\link[lattice:trellis.par.set]{trellis.par.set}}
#'@references Sarkar, Deepayan (2008) \emph{Lattice: Multivariate Data
#'Visualization with R}, Springer.  \url{http://lmdvr.r-forge.r-project.org/}
#'
#'van Buuren S and Groothuis-Oudshoorn K (2011). \code{mice}: Multivariate
#'Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical
#'Software}, \bold{45}(3), 1-67. \url{http://www.jstatsoft.org/v45/i03/}
#'@keywords hplot
#'@examples
#'
#'
#'imp <- mice(boys, maxit=1)
#'
#'### density plot of head circumference per imputation
#'### blue is observed, red is imputed
#'densityplot(imp, ~hc|.imp)
#'
#'### All combined in one panel.
#'densityplot(imp, ~hc)
#'
#'
#'@method densityplot mids
#'@S3method densityplot mids
#'@export
densityplot.mids <- function(x,
             data,
             na.groups = NULL,
             groups = NULL,
             as.table = TRUE,
             plot.points = FALSE,
             theme = mice.theme(),
             mayreplicate = TRUE,
             thicker = 2.5,
             allow.multiple = TRUE,
             outer = TRUE,
             drop.unused.levels = lattice.getOption("drop.unused.levels"),
             panel = lattice.getOption("panel.densityplot"),
             default.prepanel = lattice.getOption("prepanel.default.densityplot"),
             ...,
             subscripts = TRUE,
             subset = TRUE)
{
  call <- match.call()
  if (!is.mids(x)) stop("Argument 'x' must be a 'mids' object")

  ## unpack data and response indicator
  cd <- data.frame(complete(x, "long", include=TRUE))
  r <- as.data.frame(is.na(x$data))

  ## evaluate na.group in response indicator 
  nagp <- eval(expr=substitute(na.groups), envir=r, enclos=parent.frame())
  if (is.expression(nagp)) nagp <- eval(expr=nagp, envir=r, enclos=parent.frame())

  ## evaluate groups in imputed data
  ngp <- eval(expr=substitute(groups), envir=cd, enclos=parent.frame())
  if (is.expression(ngp)) ngp <- eval(expr=ngp, envir=cd, enclos=parent.frame())
  groups <- ngp
  
  ## evaluate subset in imputed data
  ss <- eval(expr=substitute(subset), envir=cd, enclos=parent.frame())
  if (is.expression(ss)) ss <- eval(expr=ss, envir=cd, enclos=parent.frame())
  subset <- ss

  ## evaluate further arguments before parsing
  dots <- list(...)
  args <- list(panel = panel,
               default.prepanel = default.prepanel,
               allow.multiple = allow.multiple,
               outer = outer,
               drop.unused.levels = drop.unused.levels,
               subscripts = subscripts,
               as.table = as.table,
               plot.points = plot.points)

  ## create formula if not given (in call$data !)
  vnames <- names(cd)[-(1:2)]
  allfactors <- unlist(lapply(cd,is.factor))[-(1:2)]
  if (missing(data)) {
    vnames <- vnames[!allfactors & x$nmis>2 & x$nmis < nrow(x$data)-1]
    formula <- as.formula(paste("~",paste(vnames,collapse="+",sep=""),sep=""))
  } else formula <- data
  
  ## determine the y-variables
  form  <- latticeParseFormula(model=formula, data=cd, subset = subset,
                               groups = groups, multiple = allow.multiple,
                               outer = outer, subscripts = TRUE,
                               drop = drop.unused.levels)
  xnames <- unlist(lapply(strsplit(form$right.name," \\+ "), rm.whitespace))  ## Jul2011

  ## calculate selection vector gp
  nona <- is.null(call$na.groups)
  if (!is.null(call$groups) & nona) gp <- call$groups
  else {
    if (nona) {
      ## na.df <- r[, xnames, drop=FALSE]
      ## imp0 <- rep(cd$.imp==0, x$m+1)
      ## ss <- rep(subset, x$m+1)
      ## gp <- unlist(lapply(na.df, rep, x$m+1))
      ## gp[imp0] <- !gp[imp0]
      ## call$subset <- ss & gp
      for (i in 1:length(xnames)) {
        xvar <- xnames[i]
        select <- cd$.imp!=0 & !r[,xvar]
        cd[select, xvar] <- NA
      }
      gp <- rep(cd$.imp, length(xnames))
    } else {
      for (i in 1:length(xnames)) {
        xvar <- xnames[i]
        select <- cd$.imp!=0 & !nagp
        cd[select, xvar] <- NA
      }
      gp <- rep(cd$.imp, length(xnames))
    }
  }

  ## replicate color 2 if group=.imp is part of xnames
  mustreplicate <- !(!is.null(call$groups) & nona) & mayreplicate
  if (mustreplicate) {
    theme$superpose.line$col <- rep(theme$superpose.line$col[1:2], c(1,x$m))
    theme$superpose.line$lwd <- rep(c(theme$superpose.line$lwd[1]*thicker, theme$superpose.line$lwd[1]),c(1,x$m))
    theme$superpose.symbol$col <- rep(theme$superpose.symbol$col[1:2], c(1,x$m))
    theme$superpose.symbol$pch <- c(NA,49:(49+x$m-1))
  }

  ## change axis defaults of extended formula interface  
  if (is.null(call$xlab)) {
    args$xlab <- ""
    if (length(xnames)==1) args$xlab <- xnames
  }
  if (is.null(call$scales)) {
      args$scales <- list()
      if (length(xnames)>1)
        args$scales <- list(x=list(relation="free"), y=list(relation="free"))
    }

  ## ready
  args <- c(x=formula, data=list(cd),
            groups=list(gp),
            args, dots, subset=call$subset)  

  ## go
  tp <- do.call("densityplot", args)
  tp <- update(tp, par.settings = theme)
  return(tp)
}
