\name{as.mira}
\alias{as.mira}
\title{Create a \code{mira} object from repeated analyses}
\usage{
  as.mira(fitlist)
}
\arguments{
  \item{fitlist}{A list containing $m$ fitted analysis
  objects}
}
\value{
  An S3 object of class \code{mira}.
}
\description{
  The \code{as.mira()} function takes the results of
  repeated complete-data analysis stored as a list, and
  turns it into a \code{mira} object that can be pooled.
  Pooling requires that \code{coef()} and \code{vcov()}
  methods are available for fitted object.
}
\author{
  Stef van Buuren, 2011
}
\seealso{
  \code{\link[=mira-class]{mira}}
}

