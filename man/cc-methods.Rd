\docType{methods}
\name{cc}
\alias{cc}
\alias{cc,data.frame-method}
\alias{cc,matrix-method}
\alias{cc,mids-method}
\title{Complete cases}
\usage{
cc(x, drop = TRUE)
}
\arguments{
  \item{x}{An \code{R} object. Currently supported are
  methods for the following classes: \code{mids},
  \code{mira}, \code{mipo}, \code{data.frame} and
  \code{matrix}. In addition, \code{x} can be a vector of
  any kind.}

  \item{drop}{A logical flag for matrices and arrays. If
  \code{drop=TRUE} the result is coerced to the lowest
  possible dimension.}
}
\value{
A \code{vector}, \code{matrix} of \code{data.frame}
containing the data of the complete cases.
}
\description{
Extracting complete cases is also known as 'listwise
deletion' or 'complete case analyses'. \code{cc(x)} is
equivalent to \code{na.omit(x)}. Missing values in \code{x}
are coded as \code{NA}. The companion function for
selecting the incomplete cases is \code{ic()}.
}
\examples{
cc(nhanes)   # get the 13 complete cases
cc(nhanes[,2,drop=FALSE], drop=FALSE) # extract complete bmi as column
}
\author{
Stef van Buuren, 2010.
}
\seealso{
\code{\link{na.omit}}, \code{\link{ic}}, \code{\link{cci}},
\code{\link{ici}}, \code{\link{ccn}}, \code{\link{icn}}
}
\keyword{univar}

