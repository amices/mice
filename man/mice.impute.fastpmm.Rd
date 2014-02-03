\name{mice.impute.fastpmm}
\alias{fastpmm}
\alias{mice.impute.fastpmm}
\title{Imputation by fast predictive mean matching}
\usage{
mice.impute.fastpmm(y, ry, x, donors = 5, type = 1, ridge = 1e-05, ...)
}
\arguments{
  \item{y}{Numeric vector with incomplete data}

  \item{ry}{Response pattern of \code{y}
  (\code{TRUE}=observed, \code{FALSE}=missing)}

  \item{x}{Design matrix with \code{length(y)} rows and
  \code{p} columns containing complete covariates.}

  \item{donors}{Size of the set of potential donors from
  which a random draw is made. The default is \code{donors
  = 5}.}

  \item{type}{Type of matching distance. The default choice
  \code{type = 1} calculates the distance between the
  predicted value of \code{yobs} and the drawn values of
  \code{ymis}. Other choices are \code{type = 0} (distance
  between predicted values) and \code{type = 2} (distance
  between drawn values).}

  \item{ridge}{The ridge penalty applied in
  \code{.norm.draw()} to prevent problems with
  multicollinearity. The default is \code{ridge = 1e-05},
  which means that 0.01 percent of the diagonal is added to
  the cross-product. Larger ridges may result in more
  biased estimates. For highly noisy data (e.g. many junk
  variables), set \code{ridge = 1e06} or lower to reduce
  bias.}

  \item{...}{Other named arguments.}
}
\value{
Numeric vector of length \code{sum(!ry)} with imputations
}
\description{
Imputes univariate missing data using fast predictive mean
matching
}
\details{
Imputation of \code{y} by predictive mean matching, based
on Rubin (1987, p. 168, formulas a and b).  The procedure
is as follows: \enumerate{ \item Estimate beta and sigma by
linear regression \item Draw beta* and sigma* from the
proper posterior \item Compute predicted values for
\code{yobs}beta and \code{ymis}beta* \item For each
\code{ymis}, find the observation with closest predicted
value, and take its observed value in \code{y} as the
imputation.  \item If there is more than one candidate,
make a random draw among them.  Note: The matching is done
on predicted \code{y}, NOT on observed \code{y}. }
}
\note{
\code{mice.impute.fastpmm()} was introduced on \code{mice
2.19} and after a faster alternative to
\code{mice.impute.pmm()}. This is an experimental feature.
}
\author{
Stef van Buuren, Karin Groothuis-Oudshoorn, 2000, 2012
}
\references{
Little, R.J.A. (1988), Missing data adjustments in large
surveys (with discussion), Journal of Business Economics
and Statistics, 6, 287--301.

Rubin, D.B. (1987). Multiple imputation for nonresponse in
surveys. New York: Wiley.
}
\keyword{datagen}

