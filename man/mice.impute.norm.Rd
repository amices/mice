\name{mice.impute.norm}
\alias{mice.impute.norm}
\alias{norm}
\title{Imputation by Bayesian linear regression}
\usage{
  mice.impute.norm(y, ry, x, ...)
}
\arguments{
  \item{y}{Incomplete data vector of length \code{n}}

  \item{ry}{Vector of missing data pattern
  (\code{FALSE}=missing, \code{TRUE}=observed)}

  \item{x}{Matrix (\code{n} x \code{p}) of complete
  covariates.}

  \item{...}{Other named arguments.}
}
\value{
  A vector of length \code{nmis} with imputations.
}
\description{
  Imputes univariate missing data using Bayesian linear
  regression analysis
}
\details{
  Draws values of \code{beta} and \code{sigma} for Bayesian
  linear regression imputation of \code{y} given \code{x}
  according to Rubin p. 167.
}
\note{
  Using \code{mice.impute.norm} for all columns is similar
  to Schafer's NORM method (Schafer, 1997).
}
\author{
  Stef van Buuren, Karin Groothuis-Oudshoorn, 2000
}
\references{
  Van Buuren, S., Groothuis-Oudshoorn, K. (2011).
  \code{mice}: Multivariate Imputation by Chained Equations
  in \code{R}. \emph{Journal of Statistical Software},
  \bold{45}(3), 1-67.
  \url{http://www.jstatsoft.org/v45/i03/}

  Brand, J.P.L. (1999) \emph{Development, implementation
  and evaluation of multiple imputation strategies for the
  statistical analysis of incomplete data sets.}
  Dissertation. Rotterdam: Erasmus University.

  Schafer, J.L. (1997). Analysis of incomplete multivariate
  data. London: Chapman & Hall.
}
\keyword{datagen}

