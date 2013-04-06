\name{cc}
\alias{cc}
\alias{ic}
\alias{cc,data.frame-method}
\alias{cc,matrix-method}
\alias{cc,mids-method}
\alias{ic,data.frame-method}
\alias{ic,matrix-method}
\alias{ic,mids-method}
\title{Extracts complete and incomplete cases}
\usage{
\S4method{cc}{data.frame}(x, drop = TRUE)
\S4method{cc}{matrix}(x, drop = TRUE)
\S4method{cc}{mids}(x, drop = TRUE)
\S4method{ic}{data.frame}(x, drop = TRUE)
\S4method{ic}{matrix}(x, drop = TRUE)
\S4method{ic}{mids}(x, drop = TRUE)
}
\arguments{
\item{x}{An \code{R} object. Currently supported are methods for
    the following classes: \code{mids}, \code{mira},
    \code{mipo}, \code{data.frame} and \code{matrix}. In
    addition, \code{x} can be a vector of any kind.} 
\item{drop}{A logical flag for matrices and arrays.  If
    \code{drop=TRUE} the result is coerced to the lowest possible
    dimension.}}

\description{Extracts complete and incomplete cases}

\value{ A vector, matrix of data.frame containing the data of the
  complete cases (\code{cc}) or the incomplete cases (\code{ic}).  }

\details{ This is the same as listwise deletion. \code{cc()} is
  equivalent to \code{na.omit()}. Missing values in \code{x} are coded
  as NA.}

\author{Stef van Buuren, 2010.}

\seealso{\code{\link{na.omit}}, \code{\link{cci}}, \code{\link{ici}},
  code{\link{ccn}}, code{link{icn}}}

\examples{

cc(nhanes)   # get the 13 complete cases
ic(nhanes)   # get the 12 rows with incomplete cases 
ic(nhanes[1:10,])  # incomplete cases within the first ten rows
ic(nhanes[,2:3])  # restrict extraction to variables bmi and hyp
cc(nhanes[,2,drop=FALSE], drop=FALSE) # extract complete bmi as column
}

\keyword{univar} 
