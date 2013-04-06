\name{glm.mids}
\alias{glm.mids}
\title{Generalized Linear Model for Multiply Imputed Data}

\description{
  Applies \code{glm()} to a multiply imputed data set
}
\usage{
\method{glm}{mids}(formula, family = gaussian, data, \dots)
}

\arguments{
  \item{formula}{   a formula expression as for other regression models, of the form 
    response ~ predictors. See the documentation
    of \code{\link{lm}} and \code{\link{formula}} for details.}
  \item{family}{The family of the glm model}  
  \item{data}{  An object of type \code{mids}, which stands for 'multiply imputed data set', typically
    created by function \code{mice()}.}
  
  \item{\dots}{ Additional parameters passed to \code{\link{glm}}.}
}
\value{
  An objects of class \code{mira}, which stands for 'multiply imputed repeated analysis'.
  This object contains \code{data$m} distinct \code{glm.objects}, plus some descriptive information.
}

\details{
  This function is included for backward compatibility with V1.0. The function is
  superseeded by \code{\link{with.mids}}.
}

\references{
Van Buuren, S., Groothuis-Oudshoorn, C.G.M. (2000) 
\emph{Multivariate Imputation by Chained Equations:  MICE V1.0 User's manual.} 
Leiden: TNO Quality of Life.
\url{http://www.stefvanbuuren.nl/publications/MICE V1.0 Manual TNO00038 2000.pdf}
}

\seealso{
  \code{\link{with.mids}}, \code{\link{glm}},  \code{\link{mids}}, \code{\link{mira}}
}
\examples{

imp <- mice(nhanes)
glm.mids((hyp==2)~bmi+chl, data=imp)
    # fit
    # $call:
    # glm.mids(formula = (hyp == 2) ~ bmi + chl, data = imp)
    # 
    # $call1:
    # mice(data = nhanes)
    # 
    # $nmis:
    #  age bmi hyp chl 
    #    0   9   8  10
    # 
    # $analyses:
    # $analyses[[1]]:
    # Call:
    # glm(formula = formula, data = data.i)
    # 
    # Coefficients:
    #  (Intercept)         bmi         chl 
    #   -0.4746337 -0.01565534 0.005417846
    # 
    # Degrees of Freedom: 25 Total; 22 Residual
    # Residual Deviance: 2.323886 
    # 
    # $analyses[[2]]:
    # Call:
    # glm(formula = formula, data = data.i)
    # 
    # Coefficients:
    #  (Intercept)         bmi         chl 
    #   -0.1184695 -0.02885779 0.006090282
    # 
    # Degrees of Freedom: 25 Total; 22 Residual
    # Residual Deviance: 3.647927 
    # 
}

\author{Stef van Buuren, Karin Groothuis-Oudshoorn, 2000} 

\keyword{multivariate} 
