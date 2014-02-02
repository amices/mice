\docType{methods}
\name{ici}
\alias{ici}
\alias{ici,data.frame-method}
\alias{ici,matrix-method}
\alias{ici,mids-method}
\title{Incomplete case indicator}
\usage{
ici(x)
}
\arguments{
  \item{x}{An \code{R} object. Currently supported are
  methods for the following classes: \code{mids},
  \code{data.frame} and \code{matrix}. In addition,
  \code{x} can be a vector of any kind.}
}
\value{
Logical vector indicating the incomplete cases, . If
\code{x} is a \code{data.frame} or \code{matrix} the length
is \code{nrow(x)}. In other cases, the length is
\code{length(x)}.
}
\description{
This array is useful for extracting the subset of
incomplete cases. Missing values in \code{x} are coded as
\code{NA}. The companion function for selecting the
complete cases is \code{cci()}.
}
\examples{
ici(nhanes) # indicator for 12 rows with incomplete cases
}
\author{
Stef van Buuren, 2010.
}
\seealso{
\code{\link{cci}} \code{\link{cc}}, \code{\link{ic}},
\code{\link{ccn}}, \code{\link{icn}}
}
\keyword{univar}

