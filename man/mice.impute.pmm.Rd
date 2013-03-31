\name{mice.impute.pmm}
\alias{mice.impute.pmm}
\alias{pmm}
\title{Imputation by predictive mean matching}
\usage{
  mice.impute.pmm(y, ry, x, ...)
}
\arguments{
  \item{y}{Numeric vector with incomplete data}

  \item{ry}{Response pattern of \code{y}
  (\code{TRUE}=observed, \code{FALSE}=missing)}

  \item{x}{Design matrix with \code{length(y)} rows and
  \code{p} columns containing complete covariates.}

  \item{...}{Other named arguments.}
}
\value{
  Numeric vector of length \code{sum(!ry)} with imputations
}
\description{
  Imputes univariate missing data using predictive mean
  matching
}
\details{
  Imputation of \code{y} by predictive mean matching, based
  on Rubin (1987, p. 168, formulas a and b).  The procedure
  is as follows: \enumerate{ \item Estimate beta and sigma
  by linear regression \item Draw beta* and sigma* from the
  proper posterior \item Compute predicted values for
  \code{yobs}beta and \code{ymis}beta* \item For each
  \code{ymis}, find the observation with closest predicted
  value, and take its observed value in \code{y} as the
  imputation.  \item If there is more than one candidate,
  make a random draw among them.  Note: The matching is
  done on predicted \code{y}, NOT on observed \code{y}. }
}
\note{
  \code{mice.impute.pmm2()} was used in \code{mice 2.13}
  and after a faster alternative to
  \code{mice.impute.pmm()}. Starting with \code{mice 2.14},
  \code{mice.impute.pmm()} has been replaced by
  \code{mice.impute.pmm2()}. The \code{mice.impute.pmm2()}
  function will be depricated in future versions of
  \pkg{mice}.
}
\author{
  Stef van Buuren, Karin Groothuis-Oudshoorn, 2000, 2012
}
\references{
  Little, R.J.A. (1988), Missing data adjustments in large
  surveys (with discussion), Journal of Business Economics
  and Statistics, 6, 287--301.

  Rubin, D.B. (1987). Multiple imputation for nonresponse
  in surveys. New York: Wiley.

  Van Buuren, S., Brand, J.P.L., Groothuis-Oudshoorn
  C.G.M., Rubin, D.B. (2006) Fully conditional
  specification in multivariate imputation.  \emph{Journal
  of Statistical Computation and Simulation}, \bold{76},
  12, 1049--1064.

  Van Buuren, S., Groothuis-Oudshoorn, K. (2011).
  \code{mice}: Multivariate Imputation by Chained Equations
  in \code{R}. \emph{Journal of Statistical Software},
  \bold{45}(3), 1-67.
  \url{http://www.jstatsoft.org/v45/i03/}
}
\keyword{datagen}

