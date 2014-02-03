\name{pool.scalar}
\alias{pool.scalar}
\title{Multiple imputation pooling: univariate version}
\usage{
pool.scalar(Q, U)
}
\arguments{
  \item{Q}{A vector of univariate estimates of m repeated
  complete data analyses.}

  \item{U}{A vector containing the corresponding m
  variances of the univariate estimates.}
}
\value{
Returns a list with components. Component \code{m} is the
number of imputations. Component \code{qhat} contains the
\code{m} univariate estimates of repeated complete data
analyses. Component \code{u} contains the corresponding
\code{m} variances of the univariate estimates. Component
\code{qbar} is the pooled univariate estimate, formula
(3.1.2) Rubin (1987). Component \code{ubar} is the mean of
the variances (i.e. the pooled within-imputation variance),
formula (3.1.3) Rubin (1987). Component \code{b} is the
between-imputation variance, formula (3.1.4) Rubin (1987).
Component \code{t} is the total variance of the pooled
estimated, formula (3.1.5) Rubin (1987). Component \code{r}
is the relative increase in variance due to nonresponse,
formula (3.1.7) Rubin (1987). Component \code{df} is the
degrees of freedom for t reference distribution, formula
(3.1.6) Rubin (1987). Component \code{f} is the fraction
missing information due to nonresponse, formula (3.1.10)
Rubin (1987).
}
\description{
Pools univariate estimates of m repeated complete data
analysis
}
\details{
The function averages the univariate estimates of the
complete data model, computes the total variance over the
repeated analyses, and computes the relative increase in
variance due to nonresponse and the fraction of missing
information.
}
\examples{
imp <- mice(nhanes)
m <- imp$m
Q <- rep(NA,m)
U <- rep(NA,m)
for (i in 1:m) {
   Q[i] <- mean(complete(imp,i)$bmi)
   U[i] <- var(complete(imp,i)$bmi)/(nrow(nhanes))
}
pool.scalar(Q,U)

#pool.scalar(Q,U)
#$m
#[1] 5
#
#$qhat
#[1] 26.764 26.748 27.024 27.340 26.436
#
#$u
#[1] 17.85490 19.11677 20.61440 21.05750 15.16990
#
#$qbar
#[1] 26.8624
#
#$ubar
#[1] 18.76269
#
#$b
#[1] 0.1147008
#
#t
#[1] 18.90033
#
#$r
#[1] 0.007335885
#
#$df
#[1] 75422.96
#
#$f
#[1] 0.007308785
#
}
\author{
Karin Groothuis-Oudshoorn and Stef van Buuren, 2009
}
\references{
Rubin, D.B. (1987). Multiple Imputation for Nonresponse in
Surveys.  New York: John Wiley and Sons.
}
\seealso{
\code{\link{pool}}
}
\keyword{htest}

