\name{cci}
\alias{cci}
\alias{ici}
\alias{cci,data.frame-method}
\alias{cci,matrix-method}
\alias{cci,mids-method}
\alias{ici,data.frame-method}
\alias{ici,matrix-method}
\alias{ici,mids-method}
\title{Extracts (in)complete case indicator}
\usage{
\S4method{cci}{data.frame}(x)
\S4method{cci}{matrix}(x)
\S4method{cci}{mids}(x)
\S4method{ici}{data.frame}(x)
\S4method{ici}{matrix}(x)
\S4method{ici}{mids}(x)
}
\arguments{
\item{x}{An \code{R} object. Currently supported are methods for the
    following classes: \code{mids}, \code{data.frame} and
    \code{matrix}. In addition, \code{x} can be a vector of any kind.}
}

\description{Extracts (in)complete case indicator}

\value{ A logical vector indicating the complete and the incomplete
  cases, with a length of \code{nrow(x)} if \code{x} is a data.frame
  or matrix, and with length \code{length(x)} in other cases.}

\details{ This array is useful for extracting subsets of the complete
  and incomplete data. Missing values in \code{x} are coded
  as NA.}

\author{Stef van Buuren, 2010.}

\seealso{\code{\link{na.omit}}, \code{\link{cc}}, \code{\link{ic}},
  code{\link{ccn}}, code{link{icn}}}

\examples{
  cci(nhanes) # indicator for 13 complete cases 
  ici(nhanes) # indicator for 12 rows with incomplete cases 
  f <- cci(nhanes[,c("bmi","hyp")]) # complete data for bmi and hyp
  nhanes[f,] # obtain all data from those with complete bmi and hyp
}

\keyword{univar} 
