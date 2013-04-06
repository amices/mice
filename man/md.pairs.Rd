\name{md.pairs}
\alias{md.pairs}
\title{Missing data pattern by variable pairs}
\usage{
md.pairs(data)
}
\arguments{
\item{data}{A data frame or a matrix containing the incomplete data.
    Missing values are coded as \code{NA}. }
}
\description{
    Number of observations per variable pair.
}

\value{
    A list of four components named \code{rr}, \code{rm}, \code{mr} and \code{mm}.
    Each component is square numerical matrix containing the number observations
    within four missing data pattern.
    
}
\details{
    The four components in the output value is have the following interpretation:
    \describe{
    \item{\code{rr}}{response-response, both variables are observed}
    \item{\code{rm}}{response-missing,  row observed, column missing}
    \item{\code{mr}}{missing -response, row missing, column observed}
    \item{\code{mm}}{missing -missing,  both variables are missing}
  }

}

\references{
Van Buuren, S., Groothuis-Oudshoorn, K. (2011).
\code{mice}: Multivariate Imputation by Chained Equations in \code{R}.
\emph{Journal of Statistical Software}, \bold{45}(3), 1-67.
\url{http://www.jstatsoft.org/v45/i03/}
}

\examples{

pat <- md.pairs(nhanes)
pat

# show that these four matrices decompose the total sample size
# for each pair
pat$rr + pat$rm + pat$mr + pat$mm

# percentage of usable cases to impute row variable from column variable
round(100*pat$mr/(pat$mr+pat$mm))
}

\author{Stef van Buuren, Karin Groothuis-Oudshoorn, 2009}

\keyword{univar} 
