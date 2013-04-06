\name{stripplot}
\title{Box-and-whisker plot, stripplot, density plot and scatterplot for imputed data}
\alias{bwplot}
\alias{stripplot}
\alias{densityplot}
\alias{xyplot}
\alias{bwplot.mids}
\alias{stripplot.mids}
\alias{densityplot.mids}
\alias{xyplot.mids}

\usage{

\method{bwplot}{mids}(
    x,
    data,
    na.groups = NULL,
    groups = NULL,
    as.table = TRUE,
    theme = mice.theme(),
    mayreplicate = TRUE,
    allow.multiple = TRUE,
    outer = TRUE,
    drop.unused.levels = lattice.getOption("drop.unused.levels"),
    ...,
    subscripts = TRUE,
    subset = TRUE)

\method{stripplot}{mids}(  
    x,
    data,
    na.groups = NULL,
    groups = NULL,
    as.table = TRUE,
    theme = mice.theme(),
    allow.multiple = TRUE,
    outer = TRUE,
    drop.unused.levels = lattice.getOption("drop.unused.levels"),
    panel = lattice.getOption("panel.stripplot"),
    default.prepanel = lattice.getOption("prepanel.default.stripplot"),
    jitter.data = TRUE,
    horizontal = FALSE,
    ...,
    subscripts = TRUE,
    subset = TRUE)

\method{densityplot}{mids} (
    x,
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
    default.prepanel =
    lattice.getOption("prepanel.default.densityplot"),
   ...,
    subscripts = TRUE,
    subset = TRUE)

\method{xyplot}{mids} (
    x,
    data,
    na.groups = NULL,
    groups = NULL,
    as.table = TRUE,
    theme = mice.theme(),
    allow.multiple = TRUE,
    outer = TRUE,
    drop.unused.levels = lattice.getOption("drop.unused.levels"),
    ...,
    subscripts = TRUE,
    subset = TRUE)

}

\description{ Plotting methods for imputed data using \pkg{lattice}.
  \code{bwplot} produces box-and-whisker plots, \code{stripplot}
  produces one-dimensional scatterplots, \code{densityplot} produces
  plots of the densities, and \code{xyplot} produces a conditional
  scatterplots. Each function automatically separates the observed and
  imputed data in a natural way. The functions extend the usual features
  of \pkg{lattice}.}

\arguments{
  \item{x}{
      A \code{mids} object, typically created by \code{mice()} or \code{mice.mids()}. 
  }

\item{data}{ 
    Formula that selects the data to be plotted.
    This argument follows the \pkg{lattice} rules for
    \emph{formulas}, describing the primary variables (used for the per-panel
    display) and the optional conditioning variables (which define the
    subsets plotted in different panels) to be used in the plot.

    The formula is evaluated on the complete data set in the
    \code{long} form. Legal variable names for the formula include 
   \code{names(x$data)} plus the two administrative factors \code{.imp}
    and \code{.id}.

    \bold{Extended formula interface:} The primary variable terms (both the LHS
    \code{y} and RHS \code{x}) may consist of multiple terms separated
    by a \sQuote{+} sign, e.g., \code{y1 + y2 ~ x | a * b}.  This
    formula would be taken to mean that the user wants to plot both
    \code{y1 ~ x | a * b} and \code{y2 ~ x | a * b}, but with the
    \code{y1 ~ x} and \code{y2 ~ x} in \emph{separate panels}. This behavior
    differs from standard \pkg{lattice}. \emph{Only combine terms of the
    same type}, i.e. only factors or only numerical variables. Mixing
    numerical and categorical data occasionally produces odds labeling of vertical axis.

    For convience, in \code{stripplot()} and \code{bwplot} the formula
    \code{y~.imp} may be abbreviated as \code{y}. This applies only to
    a single \code{y}, and does not (yet) work for \code{y1+y2~.imp}.

    The function \code{densityplot} does not use the \code{y} terms in
    the formula. Density plots for \code{x1} and \code{x2} are
    requested as \code{~ x1 + x2}. 
}

\item{na.groups}{ An expression evaluating to a logical vector
    indicating which two groups are distinguished (e.g. using
    different colors) in the display. The environment in which this
    expression is evaluated in the response indicator \code{is.na(x$data)}.

    The default \code{na.group =
      NULL} constrasts the observed and missing data in the LHS
    \code{y} variable of the display, i.e. groups created by
    \code{is.na(y)}. The expression \code{y} creates the groups according to
    \code{is.na(y)}. The expression \code{y1 & y2} creates groups by
    \code{is.na(y1) & is.na(y2)}, and \code{y1 | y2} 
     creates groups as \code{is.na(y1) | is.na(y2)}, and so on.
}

\item{groups}{ This is the usual \code{groups} arguments in
    \pkg{lattice}. It differs from \code{na.groups} because it
    evaluates in the completed data \code{data.frame(complete(x, "long",
      inc=TRUE))} (as usual), whereas \code{na.groups} evaluates in
    the response indicator. See \code{\link{xyplot}} for
    more details. When both \code{na.groups} and \code{groups} are
    specified, \code{na.groups} takes precedence, and \code{groups} is
    ignored. 
}

\item{plot.points}{A logical used in \code{densityplot} that signals
    whether the points should be plotted.}

\item{theme}{A named list containing the graphical parameters. The
    default function \code{mice.theme} produces a short list of default
    colors, line width, and so on. The extensive list may be obtained from
    \code{trellis.par.get()}. Global graphical parameters like
    \code{col} or \code{cex} in high-level calls are still honored, so
    first experiment with the global parameters. Many setting consists
    of a pair. For example, \code{mice.theme} defines two symbol
    colors. The first is for the observed data, the second for the
    imputed data. The theme settings only exist during the call, and do not
    affect the trellis graphical parameters.}

\item{mayreplicate}{A logical indicating whether color, line widths,
    and so on, may be replicated. The graphical functions attempt to
    choose "intelligent" graphical parameters. For example, the same
    color can be replicated for different element, e.g. use all reds
    for the imputed data. Replication may be switched off by setting the flag
    to \code{FALSE}, in order to allow the user to gain full control.}

\item{thicker}{Used in \code{densityplot}. Multiplication factor of
    the line width of the observed density. \code{thicker=1} uses the
    same thickness for the observed and imputed data.}

\item{jitter.data}{See \code{\link[lattice:panel.xyplot]{panel.xyplot}}.}

\item{horizontal}{See \code{\link[lattice:xyplot]{xyplot}}.}

\item{as.table}{See \code{\link[lattice:xyplot]{xyplot}}.}

\item{panel}{See \code{\link{xyplot}}.}

\item{default.prepanel}{See \code{\link[lattice:xyplot]{xyplot}}.}

\item{outer}{See \code{\link[lattice:xyplot]{xyplot}}.}

\item{allow.multiple}{See \code{\link[lattice:xyplot]{xyplot}}.}

\item{drop.unused.levels}{See \code{\link[lattice:xyplot]{xyplot}}.}

\item{subscripts}{See \code{\link[lattice:xyplot]{xyplot}}.}

\item{subset}{See \code{\link[lattice:xyplot]{xyplot}}.}



\item{\dots}{
    Further arguments, usually not directly processed by the high-level
    functions documented here, but instead passed on to other
    functions.}
}


\value{
  The high-level functions documented here, as well as other high-level
  Lattice functions, return an object of class \code{"trellis"}.  The
  \code{\link[lattice:update.trellis]{update}} method can be used to
  subsequently update components of the object, and the
  \code{\link[lattice:print.trellis]{print}} method (usually called by
  default) will plot it on an appropriate plotting device.
}

\details{
The argument \code{na.groups} may be used to specify (combinations
of) missingness in any of the variables. The argument \code{groups}
can be used to specify groups based on the variable values
themselves. Only one of both may be active at the same time. When both
are specified, \code{na.groups} takes precedence over
\code{groups}. 

Use the \code{subset} and \code{na.groups} together to
plots parts of the data. For example, select the first imputed data
set by by \code{subset=.imp==1}.

Graphical paramaters like \code{col}, \code{pch} and \code{cex} can be
specified in the arguments list to alter the plotting symbols. If
\code{length(col)==2}, the color specification to define the observed
and missing groups. \code{col[1]} is the color of the 'observed' data,
\code{col[2]} is the color of the missing or imputed data. A
convenient color choice is \code{col=mdc(1:2)}, a transparent blue
color for the observed data, and a transparent red color for the
imputed data. A good choice is \code{col=mdc(1:2), pch=20,
  cex=1.5}. These choices can be set for the duration of the session
by running \code{mice.theme()}.

}

\note{
  The first two arguments (\code{x} and \code{data}) are reversed
  compared to the standard Trellis syntax implemented in
  \pkg{lattice}. This reversal was necessary in order to benefit from
  automatic method dispatch.

  In \pkg{mice} the argument \code{x} is always a \code{mids} object,
  whereas in \pkg{lattice} the argument \code{x} is always a formula.

  In \pkg{mice} the argument \code{data} is always a formula object,
  whereas in \pkg{lattice} the argument \code{data} is usually a data
  frame.

  All other arguments have identical interpretation.

  \code{densityplot} errs on empty groups, which occurs if all observations
  in the subgroup contain \code{NA}. The relevant error message is: \code{Error in density.default:
    ... need at least 2 points to select a bandwidth
    automatically}. There is yet no workaround for this problem. Use
  the more robust \code{bwplot} or \code{stripplot} as a replacement.
}

\references{
  Sarkar, Deepayan (2008) \emph{Lattice: Multivariate Data Visualization
    with R}, Springer.  \url{http://lmdvr.r-forge.r-project.org/}

van Buuren S and Groothuis-Oudshoorn K (2011).
\code{mice}: Multivariate Imputation by Chained Equations in \code{R}.
\emph{Journal of Statistical Software}, \bold{45}(3), 1-67.
\url{http://www.jstatsoft.org/v45/i03/}

}

\seealso{
  \code{\link{mice}},
  \code{\link{Lattice}} for an overview of the package, as well as
  \code{\link[lattice:xyplot]{xyplot}},
  \code{\link[lattice:densityplot]{densityplot}},
  \code{\link[lattice:panel.bwplot]{panel.bwplot}},
  \code{\link[lattice:panel.stripplot]{panel.stripplot}},
  \code{\link[lattice:panel.densityplot]{panel.densityplot}},
  \code{\link[lattice:panel.xyplot]{panel.xyplot}},
  \code{\link[lattice:print.trellis]{print.trellis}},
  \code{\link[lattice:trellis.par.set]{trellis.par.set}}
}

\author{Stef van Buuren}

\examples{

imp <- mice(boys, maxit=1)

### box-and-whisker plot per imputation of all numerical variables
bwplot(imp)

### tv (testicular volume), conditional on region
bwplot(imp, tv~.imp|reg)

### same data, organized in a different way 
bwplot(imp, tv~reg|.imp, theme=list())

### stripplot, all numerical variables
\donttest{stripplot(imp)}

### same, but with improved display
stripplot(imp, col=c("grey",mdc(2)),pch=c(1,20))

### distribution per imputation of height, weight and bmi
### labeled by their own missingness
stripplot(imp, hgt+wgt+bmi~.imp, cex=c(2,4), pch=c(1,20),jitter=FALSE,
layout=c(3,1))

### same, but labeled with the missingness of wgt (just four cases)
\donttest{stripplot(imp, hgt+wgt+bmi~.imp, na=wgt, cex=c(2,4), pch=c(1,20),jitter=FALSE,
layout=c(3,1))}

### distribution of age and height, labeled by missingness in height
### most height values are missing for those around 
### the age of two years
### some additional missings occur in region WEST
stripplot(imp, age+hgt~.imp|reg, hgt, col=c(hcl(0,0,40,0.2), mdc(2)),pch=c(1,20))

### heavily jitted relation between two categorical variables
### labeled by missingness of gen
### aggregated over all imputed data sets
stripplot(imp, gen~phb, factor=2, cex=c(8,1), hor=TRUE)

### circle fun
\donttest{stripplot(imp, gen~.imp, factor=2, cex=c(8,6), hor=FALSE, na=wgt,outer=TRUE,scales="free",pch=c(1,19))}

### density plot of head circumference per imputation
### blue is observed, red is imputed
densityplot(imp, ~hc|.imp)

### All combined in one panel.
densityplot(imp, ~hc)

### The more powerful density plot of all 
### numerical variables with at least
### two missing values. 
\donttest{densityplot(imp)}


### xyplot: scatterplot by imputation number
### observe the erroneous outlying imputed values
### (caused by imputing hgt from bmi)
xyplot(imp, hgt~age|.imp, pch=c(1,20),cex=c(1,1.5))

### same, but label with missingness of wgt (four cases)
\donttest{xyplot(imp, hgt~age|.imp, na.group=wgt, pch=c(1,20),cex=c(1,1.5))}

}
\keyword{hplot}
