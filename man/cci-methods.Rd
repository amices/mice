\docType{methods}
\name{cci}
\alias{cci}
\alias{cci,data.frame-method}
\alias{cci,matrix-method}
\alias{cci,mids-method}
\title{Complete case indicator}
\arguments{
  \item{x}{An \code{R} object. Currently supported are
  methods for the following classes: \code{mids},
  \code{data.frame} and \code{matrix}. In addition,
  \code{x} can be a vector of any kind.}
}
\value{
  Logical vector indicating the complete cases, . If
  \code{x} is a \code{data.frame} or \code{matrix} the
  length is \code{nrow(x)}. In other cases, the length is
  \code{length(x)}.
}
\description{
  This array is useful for extracting the subset of
  complete cases. The function \code{cci(x)} is equivelant
  to \code{complete.cases(x)}. Missing values in \code{x}
  are coded as \code{NA}. The companion function for
  selecting the incomplete cases is \code{ici()}.
}
\examples{
cci(nhanes) # indicator for 13 complete cases
 f <- cci(nhanes[,c("bmi","hyp")]) # complete data for bmi and hyp
 nhanes[f,] # obtain all data from those with complete bmi and hyp
}
\author{
  Stef van Buuren, 2010.
}
\seealso{
  \code{\link{complete.cases}}, \code{\link{ici}}
  \code{\link{cc}}, \code{\link{ic}}, \code{\link{ccn}},
  \code{\link{icn}}
}
\keyword{univar}

