\name{mice.impute.polyreg}
\alias{mice.impute.polyreg}
\title{Imputation by polytomous regression - unordered}
\usage{
mice.impute.polyreg(y, ry, x, nnet.maxit = 100, nnet.trace = FALSE,
  nnet.maxNWts = 1500, ...)
}
\arguments{
  \item{y}{Incomplete data vector of length \code{n}}

  \item{ry}{Vector of missing data pattern
  (\code{FALSE}=missing, \code{TRUE}=observed)}

  \item{x}{Matrix (\code{n} x \code{p}) of complete
  covariates.}

  \item{nnet.maxit}{Tuning parameter for \code{nnet()}.}

  \item{nnet.trace}{Tuning parameter for \code{nnet()}.}

  \item{nnet.maxNWts}{Tuning parameter for \code{nnet()}.}

  \item{...}{Other named arguments.}
}
\value{
A vector of length \code{nmis} with imputations.
}
\description{
Imputes missing data in a categorical variable using
polytomous regression
}
\details{
By default, unordered factors with more than two levels are
imputed by \code{mice.impute.polyreg()}. 'The function
\code{mice.impute.polyreg()} imputes categorical response
variables by the Bayesian polytomous regression model. See
J.P.L. Brand (1999), Chapter 4, Appendix B.

The method consists of the following steps: \enumerate{
\item Fit categorical response as a multinomial model \item
Compute predicted categories \item Add appropriate noise to
predictions.  }

The algorithm of \code{mice.impute.polyreg} uses the
function \code{multinom()} from the \code{nnet} package.

In order to avoid bias due to perfect prediction, the
algorithm augment the data according to the method of
White, Daniel and Royston (2010).
}
\author{
Stef van Buuren, Karin Groohuis-Oudshoorn, 2000-2010
}
\references{
Van Buuren, S., Groothuis-Oudshoorn, K. (2011).
\code{mice}: Multivariate Imputation by Chained Equations
in \code{R}. \emph{Journal of Statistical Software},
\bold{45}(3), 1-67. \url{http://www.jstatsoft.org/v45/i03/}

Brand, J.P.L. (1999) \emph{Development, implementation and
evaluation of multiple imputation strategies for the
statistical analysis of incomplete data sets.}
Dissertation. Rotterdam: Erasmus University.

White, I.R., Daniel, R. Royston, P. (2010). Avoiding bias
due to perfect prediction in multiple imputation of
incomplete categorical variables. \emph{Computational
Statistics and Data Analysis}, 54, 2267-2275.

Venables, W.N. & Ripley, B.D. (2002). \emph{Modern applied
statistics with S-Plus (4th ed)}. Springer, Berlin.
}
\seealso{
\code{\link{mice}}, \code{\link[nnet]{multinom}},
\code{\link[MASS]{polr}}
}
\keyword{datagen}

