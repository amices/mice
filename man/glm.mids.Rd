\name{glm.mids}
\alias{glm.mids}
\title{Generalized linear model for \code{mids} object}
\usage{
glm.mids(formula, family = gaussian, data, ...)
}
\arguments{
  \item{formula}{a formula expression as for other
  regression models, of the form response ~ predictors. See
  the documentation of \code{\link{lm}} and
  \code{\link{formula}} for details.}

  \item{family}{The family of the glm model}

  \item{data}{An object of type \code{mids}, which stands
  for 'multiply imputed data set', typically created by
  function \code{mice()}.}

  \item{\dots}{Additional parameters passed to
  \code{\link{glm}}.}
}
\value{
An objects of class \code{mira}, which stands for 'multiply
imputed repeated analysis'.  This object contains
\code{data$m} distinct \code{glm.objects}, plus some
descriptive information.
}
\description{
Applies \code{glm()} to a multiply imputed data set
}
\details{
This function is included for backward compatibility with
V1.0. The function is superseeded by
\code{\link{with.mids}}.
}
\examples{
imp <- mice(nhanes)

# logistic regression on the imputed data
fit <- glm.mids((hyp==2)~bmi+chl, data=imp, family = binomial)
fit
}
\author{
Stef van Buuren, Karin Groothuis-Oudshoorn, 2000
}
\references{
Van Buuren, S., Groothuis-Oudshoorn, C.G.M. (2000)
\emph{Multivariate Imputation by Chained Equations: MICE
V1.0 User's manual.} Leiden: TNO Quality of Life.
}
\seealso{
\code{\link{with.mids}}, \code{\link{glm}},
\code{\link[=mids-class]{mids}},
\code{\link[=mira-class]{mira}}
}
\keyword{multivariate}

