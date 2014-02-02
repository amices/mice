\docType{methods}
\name{icn}
\alias{icn}
\alias{icn,data.frame-method}
\alias{icn,matrix-method}
\alias{icn,mids-method}
\title{Incomplete cases n}
\usage{
icn(x)
}
\arguments{
  \item{x}{An \code{R} object. Currently supported are
  methods for the following classes: \code{mids},
  \code{data.frame} and \code{matrix}. In addition,
  \code{x} can be a vector of any kind.}
}
\value{
An integer with the number of elements in \code{x} with
incomplete data.
}
\description{
Calculates the number of incomplete cases. The companion
function for calculating the number of complete cases is
\code{ccn()}.
}
\examples{
icn(nhanes) # the remaining 12 rows
 icn(nhanes[,c("bmi","hyp")]) # number of cases with incomplete bmi and hyp
}
\author{
Stef van Buuren, 2010.
}
\seealso{
\code{\link{ccn}}, \code{\link{cc}}, \code{\link{ic}},
\code{\link{cci}}, \code{\link{ici}}
}
\keyword{univar}

