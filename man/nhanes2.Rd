\name{nhanes2}
\alias{nhanes2}
\docType{data}
\title{NHANES example - mixed numerical and discrete variables}
\description{
A small data set with non-monotone missing values.
}
\usage{data(nhanes2)}
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
A small data set with missing data and mixed numerical and 
discrete variables. The data set 
\code{nhanes} is the same data set, but with all data 
treated as numerical.
}
\source{
Schafer, J.L. (1997). 
\emph{Analysis of Incomplete Multivariate Data.} 
London: Chapman & Hall. Table 6.14.
}

\seealso{\code{\link{nhanes}}}


\examples{

imp <- mice(nhanes2)     # create 5 imputed data sets
complete(imp)           # print the first imputed data set
}
\keyword{datasets}
