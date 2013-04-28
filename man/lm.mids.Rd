\name{lm.mids}
\alias{lm.mids}
\title{Linear regression for \code{mids} object}
\usage{
  lm.mids(formula, data, ...)
}
\arguments{
  \item{formula}{a formula object, with the response on the
  left of a ~ operator, and the terms, separated by +
  operators, on the right. See the documentation of
  \code{\link{lm}} and \code{\link{formula}} for details.}

  \item{data}{An object of type 'mids', which stands for
  'multiply imputed data set', typically created by a call
  to function \code{mice()}.}

  \item{\dots}{Additional parameters passed to
  \code{\link{lm}}}
}
\value{
  An objects of class \code{mira}, which stands for
  'multiply imputed repeated analysis'.  This object
  contains \code{data$m} distinct \code{lm.objects}, plus
  some descriptive information.
}
\description{
  Applies \code{lm()} to multiply imputed data set
}
\details{
  This function is included for backward compatibility with
  V1.0. The function is superseeded by
  \code{\link{with.mids}}.
}
\examples{
imp <- mice(nhanes)
fit <- lm.mids(bmi~hyp+chl, data = imp)
fit
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
}
\seealso{
  \code{\link{lm}}, \code{\link[=mids-class]{mids}},
  \code{\link[=mira-class]{mira}}
}
\keyword{multivariate}

