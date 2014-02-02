\name{fico}
\alias{fico}
\title{Fraction of incomplete cases among cases with observed}
\usage{
fico(data)
}
\arguments{
  \item{data}{A data frame or a matrix containing the
  incomplete data.  Missing values are coded as NA's.}
}
\value{
A vector of length \code{ncol(data)} of FICO statistics.
}
\description{
FICO is an outbound statistic defined by the fraction of
incomplete cases among cases with \code{Yj} observed (White
and Carlin, 2010).
}
\author{
Stef van Buuren, 2012
}
\references{
van Buuren, S. (2012). \emph{Flexible Imputation of Missing
Data.} Boca Raton, FL: Chapman & Hall/CRC Press.

White, I.R., Carlin, J.B. (2010). Bias and efficiency of
multiple imputation compared with complete-case analysis
for missing covariate values. \emph{Statistics in
Medicine}, \emph{29}, 2920-2931.
}
\seealso{
\code{\link{fluxplot}}, \code{\link{flux}},
\code{\link{md.pattern}}
}
\keyword{misc}

