\name{rbind.mids}
\alias{rbind.mids}
\title{Rowwise combination of a \code{mids} object.}
\usage{
  rbind.mids(x, y, ...)
}
\arguments{
  \item{x}{A \code{mids} object.}

  \item{y}{A \code{mids} object or a \code{data.frame},
  \code{matrix}, \code{factor} or \code{vector}.}

  \item{\dots}{Additional \code{data.frame}, \code{matrix},
  \code{vector} or \code{factor}. These can be given as
  named arguments.}
}
\value{
  An S3 object of class \code{mids}
}
\description{
  Append \code{mids} objects by rows
}
\details{
  This function combines two \code{mids} objects rowwise
  into a single \code{mids} object or combines a
  \code{mids} object and a vector, matrix, factor or
  dataframe rowwise into a \code{mids} object. The number
  of columns in the (incomplete) data \code{x$data} and
  \code{y} (or \code{y$data} if \code{y} is a \code{mids}
  object) should be equal. If \code{y} is a \code{mids}
  object then the number of imputations in \code{x} and
  \code{y} should be equal.
}
\note{
  Component \code{call} is a vector, with first argument
  the \code{mice()} statement that created \code{x} and
  second argument the call to \code{rbind.mids()}.
  Component \code{data} is the rowwise combination of the
  (incomplete) data in \code{x} and \code{y}. Component
  \code{m} is equal to \code{x$m}. Component \code{nmis} is
  an array containing the number of missing observations
  per column, defined as \code{x$nmis} + \code{y$nmis}.
  Component \code{imp} is a list of \code{nvar} components
  with the generated multiple imputations.  Each part of
  the list is a \code{nmis[j]} by \code{m} matrix of
  imputed values for variable \code{j}. If \code{y} is a
  \code{mids} object then \code{imp[[j]]} equals
  \code{rbind(x$imp[[j]], y$imp[[j]])}; otherwise the
  original data of \code{y} will be copied into this list,
  including the missing values of \code{y} then \code{y} is
  not imputed. Component \code{method} is a vector of
  strings of \code{length(nvar)} specifying the elementary
  imputation method per column defined as \code{x$method}.
  Component \code{predictorMatrix} is a square matrix of
  size \code{ncol(data)} containing the predictor set
  defined as \code{x$predictorMatrix}. Component
  \code{visitSequence} is the sequence in which columns are
  visited, defined as \code{x$visitSequence}. Component
  \code{seed} is the seed value of the solution,
  \code{x$seed}. Component \code{iteration} is the last
  Gibbs sampling iteration number, \code{x$iteration}.
  Component \code{lastSeedValue} is the most recent seed
  value, \code{x$lastSeedValue} Component \code{chainMean}
  is set to \code{NA}. Component \code{chainVar} is set to
  \code{NA}. Component \code{pad} is set to \code{x$pad}, a
  list containing various settings of the padded imputation
  model, i.e. the imputation model after creating dummy
  variables. Component \code{loggedEvents} is set to
  \code{x$loggedEvents}.
}
\author{
  Karin Groothuis-Oudshoorn, Stef van Buuren, 2009
}
\references{
  van Buuren S and Groothuis-Oudshoorn K (2011).
  \code{mice}: Multivariate Imputation by Chained Equations
  in \code{R}. \emph{Journal of Statistical Software},
  \bold{45}(3), 1-67.
  \url{http://www.jstatsoft.org/v45/i03/}
}
\seealso{
  \code{\link{cbind.mids}}, \code{\link{ibind}},
  \code{\link[=mids-class]{mids}}
}
\keyword{manip}

