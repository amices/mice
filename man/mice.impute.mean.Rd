\name{mice.impute.mean}
\alias{mice.impute.mean}
\title{Imputation by the Mean}

\description{
  Imputes the arithmetic mean of the observed data
}
\usage{
    mice.impute.mean(y, ry, x=NULL, ...)
}
\arguments{
  \item{y}{Incomplete data vector of length \code{n}}
  \item{ry}{Vector of missing data pattern (\code{FALSE}=missing, \code{TRUE}=observed)}
  \item{x}{Matrix (\code{n} x \code{p}) of complete covariates.}
  \item{...}{Other named arguments.}

}

\value{
   A vector of length \code{nmis} with imputations.
}

\section{Warning}{
  Imputing the mean of a variable is almost never appropriate.
  See Little and Rubin (1987). 
}
\references{
Van Buuren, S., Groothuis-Oudshoorn, K. (2011).
\code{mice}: Multivariate Imputation by Chained Equations in \code{R}.
\emph{Journal of Statistical Software}, \bold{45}(3), 1-67.
\url{http://www.jstatsoft.org/v45/i03/}
  
  Little, R.J.A. and Rubin, D.B. (1987). Statistical Analysis with Missing Data. 
  New York: John Wiley and Sons.
}
\seealso{
  \code{\link{mice}}, \code{\link{mean}}
}
\author{Stef van Buuren, Karin Groothuis-Oudshoorn, 2000} 
\keyword{datagen} 
