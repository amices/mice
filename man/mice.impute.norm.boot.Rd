\name{mice.impute.norm.boot}
\alias{mice.impute.norm.boot}
\alias{norm.boot}
\title{Imputation by Linear Regression, Bootstrap Method}
\usage{
mice.impute.norm.boot(y, ry, x, ridge=0.00001, ...)
}
\description{Imputes univariate missing data using linear regression with boostrap 
}
\arguments{
  \item{y}{Incomplete data vector of length \code{n}}
  \item{ry}{Vector of missing data pattern (\code{FALSE}=missing, \code{TRUE}=observed)}
  \item{x}{Matrix (\code{n} x \code{p}) of complete covariates.}
  \item{ridge}{Ridge parameter}
  \item{...}{Other named arguments.}

}
\value{A vector of length \code{nmis} with imputations.
}
\details{
    Draws a bootstrap sample from \code{x[ry,]} and \code{y[ry]}, calculates regression 
weights and imputes with normal residuals. The \code{ridge} parameter adds 
a penalty term \code{ridge*diag(xtx)} to the variance-covariance matrix \code{xtx}.
}
\references{
Van Buuren, S., Groothuis-Oudshoorn, K. (2011).
\code{mice}: Multivariate Imputation by Chained Equations in \code{R}.
\emph{Journal of Statistical Software}, \bold{45}(3), 1-67.
\url{http://www.jstatsoft.org/v45/i03/}
}

\author{Stef van Buuren, 2011} 
\keyword{datagen} 
