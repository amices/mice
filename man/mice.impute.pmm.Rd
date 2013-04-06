\name{mice.impute.pmm}
\alias{mice.impute.pmm}
\alias{mice.impute.pmm2}
\alias{pmm}
\alias{pmm2}
\title{Imputation by Predictive Mean Matching}
\usage{
mice.impute.pmm(y, ry, x, ...)
mice.impute.pmm2(y, ry, x, ...)
}
\description{Imputes univariate missing data using predictive mean matching}
\arguments{
  \item{y}{ Numeric vector with incomplete data}
  \item{ry}{Response pattern of \code{y} (\code{TRUE}=observed, \code{FALSE}=missing)}
  \item{x}{ Design matrix with \code{length(y)} rows and \code{p} columns containing 
  complete covariates.}
  \item{...}{Other named arguments.}
}

\value{\item{imp}{Numeric vector of length \code{sum(!ry)} with imputations}
}

\details{
  Imputation of \code{y} by predictive mean matching, based on Rubin (1987, p. 168, formulas a and b).
  The procedure is as follows:
  \enumerate{
    \item Estimate beta and sigma by linear regression
    \item Draw beta* and sigma* from the proper posterior
    \item Compute predicted values for \code{yobs}beta and \code{ymis}beta*
    \item For each \code{ymis}, find the observation with closest predicted
  value, and take its observed value in \code{y} as the imputation.
    \item If there is more than one candidate, make a random draw among them.
  Note: The matching is done on predicted \code{y}, NOT on observed \code{y}.
}
    \code{mice.impute.pmm2()} is about five times faster than \code{mice.impute.pmm()}, and was added to \code{mice 2.13}. If 
    \code{pmm2()} holds up after testing, expect it to replace the default function 
    \code{pmm()} in a future version of \code{mice}. 
}
\references{
Little, R.J.A. (1988), Missing data adjustments in large surveys (with discussion), 
Journal of Business Economics and Statistics, 6, 287--301.

Rubin, D.B. (1987). Multiple imputation for nonresponse in surveys. New York: Wiley.

Van Buuren, S., Brand, J.P.L., Groothuis-Oudshoorn C.G.M., Rubin, D.B. (2006) 
Fully conditional specification in multivariate imputation. 
\emph{Journal of Statistical Computation and Simulation}, \bold{76}, 12, 1049--1064. 
\url{http://www.stefvanbuuren.nl/publications/FCS in multivariate imputation - JSCS 2006.pdf}

Van Buuren, S., Groothuis-Oudshoorn, K. (2011).
\code{mice}: Multivariate Imputation by Chained Equations in \code{R}.
\emph{Journal of Statistical Software}, \bold{45}(3), 1-67.
\url{http://www.jstatsoft.org/v45/i03/}

}

\author{Stef van Buuren, Karin Groothuis-Oudshoorn, 2000, 2012} 
\keyword{datagen} 
