\name{pool.r.squared}
\alias{pool.r.squared}
\title{Pooling: R squared}

\description{
  Pools R^2 of m repeated complete data models.
}
\usage{
pool.r.squared(object,adjusted=FALSE)
}

\arguments{
  \item{object}{An object of class 'mira', produced by \code{lm.mids} 
  or \code{with.mids} with \code{lm} as modelling function.}
  \item{adjusted}{A logical value. If adjusted=TRUE then the adjusted R^2 is calculated.
  The default value is FALSE.}
}
\value{ Returns a 1x4 table with elements:
\item{est}{The pooled R^2 estimate}
\item{lo95}{The 95 \% lower bound of the pooled R^2.}
\item{hi95}{The 95 \% upper bound of the pooled R^2.}
\item{fmi}{The fraction of missing information due to nonresponse.}
}


\details{
  The function pools the coefficients of determination R^2 or the adjusted coefficients
  of determination (R^2_a) obtained with the \code{lm} modelling function. For pooling it uses
  the Fisher \emph{z}-transformation.
}

\references{
  Harel, O (2009). The estimation of R^2 and adjusted R^2 in incomplete data sets using multiple 
  imputation, Journal of Applied Statistics (to appear).

  Rubin, D.B. (1987). Multiple Imputation for Nonresponse in Surveys.
  New York: John Wiley and Sons.

van Buuren S and Groothuis-Oudshoorn K (2011).
\code{mice}: Multivariate Imputation by Chained Equations in \code{R}.
\emph{Journal of Statistical Software}, \bold{45}(3), 1-67.
\url{http://www.jstatsoft.org/v45/i03/}

  }
\seealso{
  \code{\link{pool}},\code{\link{pool.scalar}}
}
\examples{

imp<-mice(nhanes)

fit<-lm.mids(chl~age+hyp+bmi,imp)
pool.r.squared(fit)
pool.r.squared(fit,adjusted=TRUE) 

#fit<-lm.mids(chl~age+hyp+bmi,imp)
#
#> pool.r.squared(fit)
#          est     lo 95     hi 95       fmi
#R^2 0.5108041 0.1479687 0.7791927 0.3024413
#
#> pool.r.squared(fit,adjusted=TRUE) 
#          est      lo 95    hi 95       fmi
#adj R^2 0.4398066 0.08251427 0.743172 0.3404165
# 

}

\author{Karin Groothuis-Oudshoorn and Stef van Buuren, 2009}

\keyword{htest}
