\docType{data}
\name{nhanes2}
\alias{nhanes2}
\title{NHANES example - mixed numerical and discrete variables}
\format{A data frame with 25 observations on the following 4 variables.
\describe{
\item{age}{Age group (1=20-39, 2=40-59, 3=60+)}
\item{bmi}{Body mass index (kg/m**2)}
\item{hyp}{Hypertensive (1=no,2=yes)}
\item{chl}{Total serum cholesterol (mg/dL)} }}
\source{
  Schafer, J.L. (1997).  \emph{Analysis of Incomplete
  Multivariate Data.} London: Chapman & Hall. Table 6.14.
}
\description{
  A small data set with non-monotone missing values.
}
\details{
  A small data set with missing data and mixed numerical
  and discrete variables. The data set \code{nhanes} is the
  same data set, but with all data treated as numerical.
}
\examples{
imp <- mice(nhanes2)     # create 5 imputed data sets
complete(imp)           # print the first imputed data set
}
\seealso{
  \code{\link{nhanes}}
}
\keyword{datasets}

