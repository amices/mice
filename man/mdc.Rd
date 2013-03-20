\name{mdc}
\alias{mdc}
\alias{mice.theme}

\title{Graphical parameter for missing data plots.}

\description{
\code{mdc} returns colors used to distinguish observed, missing and
combined data in plotting. \code{mice.theme} return a partial list of
named objects that can be used as a theme in \code{stripplot},
\code{bwplot}, \code{densityplot} and \code{xyplot}. 
}

\usage{
mdc(r="observed", s="symbol", transparent=TRUE,
   cso = hcl(240,100,40,0.7),
   csi = hcl(0,100,40,0.7),
   csc = "gray50",
   clo = hcl(240,100,40,0.8),
   cli = hcl(0,100,40,0.8), 
   clc = "gray50")

mice.theme(transparent=TRUE, alpha.fill=0.3)

}
\arguments{
  \item{r}{
     A numerical or character vector. The numbers 1-6 request colors
     as follows: 1=\code{cso}, 2=\code{csi}, 3=\code{csc},
     4=\code{clo}, 5=\code{cli} and 6=\code{clc}. Alternatively,
     \code{r} may contain the strings "\code{observed}",
     "\code{missing}", or "\code{both}", or abbreviations thereof.}
     \item{s}{A character vector containing the strings "\code{symbol}" or
    "\code{line}", or abbreviations thereof.}   
    \item{transparent}{A logical indicating whether alpha-transparancy is
    	allowed. The default is \code{TRUE}.}
    \item{alpha.fill}{A numerical values between 0 and 1 that indicates the default 
        alpha value for fills.}
\item{cso}{The symbol color for the observed data. The default is a
    transparent blue.}
\item{csi}{The symbol color for the missing or imputed data. The default is a
    transparent red.}
\item{csc}{The symbol color for the combined observed and imputed data. The default is a
    grey color.}
\item{clo}{The line color for the observed data. The default is a
    slightly darker transparent blue.}
\item{cli}{The line color for the missing or imputed data. The default is a
    slightly darker transparent red.}
\item{clc}{The line color for the combined observed and imputed data. The default is a
    grey color.}
}
\details{
This function eases consistent use of colors in plots. The default  
follows the Abayomi convention, which uses blue for observed data, red
for missing or imputed data, and black for combined data. 
}
\value{
\code{mdc} returns a vector containing color definitions. The length of the output vector is
calculate from the length of \code{r} and \code{s}. Elements of the
input vectors are
repeated if needed. \code{mice.theme} return a named list that can be
used as a theme in the functions in \pkg{lattice}. By default, 
the \code{mice.theme()} function sets \code{transparent <- TRUE} if the current device
\code{.Device} supports semi-transparent colors.
}
\author{
Stef van Buuren, sept 2012.
}

\references{
  Sarkar, Deepayan (2008) \emph{Lattice: Multivariate Data Visualization
    with R}, Springer.  \url{http://lmdvr.r-forge.r-project.org/}
}

\seealso{
    \code{\link{hcl}}, 
    \code{\link{rgb}},
    \code{\link[mice:xyplot]{xyplot.mids}},
    \code{\link[lattice:xyplot]{xyplot}},
    \code{\link[lattice:trellis.par.set]{trellis.par.set}}
}
\keyword{hplot}

\examples{
# all six colors
mdc(1:6)

# lines color for observed and missing data
mdc(c("obs","mis"), "lin")

}
