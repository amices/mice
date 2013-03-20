\name{mice.impute.logreg}
\alias{mice.impute.logreg}
\alias{mice.impute.logreg.boot}
\alias{logreg}
\alias{logreg.boot}
\title{Multiple Imputation by Logistic Regression}

\description{
  Imputes univariate missing data using logistic regression.
}
\usage{
    mice.impute.logreg(y, ry, x, ...)
    mice.impute.logreg.boot(y, ry, x, ...)
}
\arguments{
  
  \item{y}{Incomplete data vector of length \code{n}}
  \item{ry}{Vector of missing data pattern of length \code{n} (\code{FALSE}=missing,
    \code{TRUE}=observed) }
  \item{x}{Matrix (\code{n} x \code{p}) of complete covariates.}
  \item{...}{Other named arguments.}
}

\value{
  \item{imp}{A vector of length \code{nmis} with imputations (0 or 1).}
}

\details{
  Imputation for binary response variables by the Bayesian 
  logistic regression model (Rubin 1987, p. 169-170) or bootstrap 
  logistic regression model. 
  The Bayesian method consists of the following steps:
  \enumerate{
    \item Fit a logit, and find (bhat, V(bhat))
    \item Draw BETA from N(bhat, V(bhat))
    \item Compute predicted scores for m.d., i.e. logit-1(X BETA)
    \item Compare the score to a random (0,1) deviate, and impute.}
  The method relies on the standard \code{glm.fit} function. Warnings from \code{glm.fit} are suppressed.
  The bootstrap method draws a bootstrap sample from \code{y[ry]} and \code{x[ry,]}. 
  Perfect prediction is handled by the data augmentation method.
}
\references{
Van Buuren, S., Groothuis-Oudshoorn, K. (2011).
\code{mice}: Multivariate Imputation by Chained Equations in \code{R}.
\emph{Journal of Statistical Software}, \bold{45}(3), 1-67.
\url{http://www.jstatsoft.org/v45/i03/}

  Brand, J.P.L. (1999). Development, Implementation and Evaluation of Multiple Imputation Strategies for 
  the Statistical Analysis of Incomplete Data Sets. Ph.D. Thesis, 
  TNO Prevention and Health/Erasmus University Rotterdam. ISBN 90-74479-08-1. 
  
  Venables, W.N. & Ripley, B.D. (1997). Modern applied statistics with S-Plus (2nd ed). Springer, Berlin.

  White, I., Daniel, R. and Royston, P (2010). Avoiding bias due to perfect prediction in multiple imputation of incomplete categorical variables. 
  Computational Statistics and Data Analysis, 54:22672275.
}
\seealso{\code{\link{mice}}, \code{\link{glm}}, \code{\link{glm.fit}}}

\author{Stef van Buuren, Karin Groothuis-Oudshoorn, 2000, 2011} 

\keyword{datagen}
