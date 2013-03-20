\name{ccn}
\alias{ccn}
\alias{icn}
\alias{ccn,data.frame-method}
\alias{ccn,matrix-method}
\alias{ccn,mids-method}
\alias{icn,data.frame-method}
\alias{icn,matrix-method}
\alias{icn,mids-method}
\title{Number of (in)complete cases}
\usage{
\S4method{ccn}{data.frame}(x)
\S4method{ccn}{matrix}(x)
\S4method{ccn}{mids}(x)
\S4method{icn}{data.frame}(x)
\S4method{icn}{matrix}(x)
\S4method{icn}{mids}(x)
}
\arguments{
\item{x}{An \code{R} object. Currently supported are methods for the
    following classes: \code{mids}, \code{data.frame} and
    \code{matrix}. In addition, \code{x} can be a vector of any kind.}
}

\description{Calculates the number of (in)complete cases}

\value{ An integer with the number of elements in \code{x} with
  (in)complete data.}

\author{Stef van Buuren, 2010.}

\seealso{\code{\link{cc}}, \code{\link{ic}},
  code{\link{cci}}, code{link{ici}}}

\examples{
  ccn(nhanes) # 13 complete cases 
  icn(nhanes) # the remaining 12 rows 
  icn(nhanes[,c("bmi","hyp")]) # number of cases with incomplete bmi and hyp
}

\keyword{univar} 
