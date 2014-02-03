\name{as.mids}
\alias{as.mids}
\title{Converts an multiply imputed dataset (long format) into a \code{mids} object}
\usage{
as.mids(data, .imp=1, .id=2)
}
\arguments{
  \item{data}{A multiply imputed data set in long format}

  \item{.imp}{Mandatory column indicator for the multiple
  imputation stream, where \code{0} indicates the
  incomplete data and \code{1} through \code{m} indicate
  the \code{m} multiple imputation streams. Default is
  \code{1}.}

  \item{.id}{Optional column indicator for the row numbers.
  Default is \code{2}.}
}
\value{
An object of class \code{mids}
}
\description{
This function converts imputed data stored in long format
into an object of class \code{mids}. The original
incomplete data set needs to be available so that we know
where the missing data are. The function is useful to
convert back operations applied to the imputed data back in
a \code{mids} object. It may also be used to store multiply
imputed data sets from other software into the format used
by \code{mice}.
}
\details{
If \code{.id} is specified, row names from the original
data (if supplied) will be copied to the \code{mids}
object.
}
\examples{
# nhanes example without .id
imp <- mice(nhanes, print = FALSE)
X <- complete(imp, action = "long", include = TRUE)[, -2]
test <- as.mids(X, .id = NULL)
is.mids(test)
test.dat <- complete(test, action = "long", include = TRUE)

# Test on boys data
imp <- mice(boys, print = FALSE, maxit = 1)
X <- complete(imp, action = "long", include = TRUE)
test <- as.mids(X)
is.mids(test)
test.dat <- complete(test, action = "long", include = TRUE)
# original rownumbers are automatically copied from .id
}
\author{
Gerko Vink, 2012
}
\keyword{mids}

