\name{nhanes}
\alias{nhanes}
\docType{data}
\title{NHANES example - all variables numerical}
\description{
A small data set with non-monotone missing values.
}
\usage{data(nhanes)}
\format{
  A data frame with 25 observations on the following 4 variables.
  \describe{
    \item{\code{age}}{Age group (1=20-39, 2=40-59, 3=60+)}
    \item{\code{bmi}}{Body mass index (kg/m**2)}
    \item{\code{hyp}}{Hypertensive (1=no,2=yes)}
    \item{\code{chl}}{Total serum cholesterol (mg/dL)}
  }
}
\details{
A small data set with all numerical variables. The data set 
\code{nhanes2} is the same data set, but with \code{age} and 
\code{hyp} treated as factors.
}
\source{
Schafer, J.L. (1997). 
\emph{Analysis of Incomplete Multivariate Data.} 
London: Chapman & Hall. Table 6.14.
}

\seealso{\code{\link{nhanes2}}}


\examples{

imp <- mice(nhanes)     # create 5 imputed data sets
complete(imp)           # print the first imputed data set
}
\keyword{datasets}
