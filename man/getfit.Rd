\name{getfit}
\alias{getfit}

\title{Extracts fit objects from mira object}

\description{
\code{getfit} returns the list of objects containing the repeated analysis results,
or optionally, one of these fit objects.
}

\usage{
getfit(x, i= -1, simplify=FALSE)
}
\arguments{
  \item{x}{
     An object of class \code{mira}, typically produced by a call to 
     \code{with()}.}  
\item{i}{An integer between 1 and \code{x$m} signalling the number of the repeated analysis. 
The default \code{i= -1} return a list with all analyses.}
\item{simplify}{Should the return value be unlisted?}
}
\details{
This function is shorthand notation for \code{x$analyses} and \code{x$analyses[[i]].}
}
\value{
If \code{i = -1} an object containing all analyses, otherwise it returns the
fittd object of the i'th repeated analysis.
}
\author{
Stef van Buuren, March 2012.
}

\seealso{
    \code{\link{mira}},
    \code{\link{with.mids}}
}
\keyword{manip}

\examples{
imp <- mice(nhanes)
fit <- with(imp, lm(bmi~chl+hyp))
getfit(fit)
getfit(fit, 2)
}
