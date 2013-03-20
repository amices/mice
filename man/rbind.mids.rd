\name{rbind.mids}
\alias{rbind.mids}
\title{Combine a Multiply Imputed Data Set with other mids object or dataframe}
\usage{rbind.mids(x,y,\dots)
}

\description{Append \code{mids} objects by rows}

\arguments{
  \item{x}{A \code{mids} object.}
  \item{y}{A \code{mids} object or a dataframe, matrix, factor or vector.}
  \item{\dots}{Dataframes, matrices, vectors or factors. These can be given as named arguments.}
}

\value{
  \item{call}{A vector, with first argument the \code{mice()} statement that 
   created \code{x} and second argument the call to \code{rbind.mids()}}
  \item{data}{The rowwise combination of the (incomplete) data in \code{x} and \code{y}.}
  \item{m}{\code{x$m}}
  \item{nmis}{An array containing the number of missing observations per column,
  defined as \code{x$nmis} + \code{y$nmis}}
  \item{imp}{A list of \code{nvar} components with the generated multiple imputations.
    Each part of the list is a \code{nmis[j]} by \code{m} matrix of imputed values for 
    variable \code{j}. If \code{y} is a \code{mids} object then \code{imp[[j]]} equals 
    \code{rbind(x$imp[[j]], y$imp[[j]])}; otherwise the original
    data of \code{y} will be copied into this list, including the missing values of \code{y} 
    then \code{y} is not imputed.}
  \item{method}{A vector of strings of \code{length(nvar)} specifying the elementary
    imputation method per column defined as \code{x$method}}
  \item{predictorMatrix}{
    A square matrix of size \code{ncol(data)} containing code 0/1 data specifying
    the predictor set defined as \code{x$predictorMatrix}}
  \item{visitSequence}{
    The sequence in which columns are visited, defined as \code{x$visitSequence}.}
  \item{seed}{  The seed value of the solution, \code{x$seed}}
  \item{iteration}{
    Last Gibbs sampling iteration number, \code{x$iteration}}
  \item{lastSeedValue}{
    The most recent seed value, \code{x$lastSeedValue}}
  \item{chainMean}{ Set to \code{NA}}
  \item{chainVar}{Set to \code{NA}}
  \item{pad}{\code{x$pad}, a list containing various settings of the padded imputation model,
    i.e. the imputation model after creating dummy variables}
}

\details{
This function combines two \code{mids} objects rowwise into a
single \code{mids} object or combines a \code{mids} object and a vector, matrix, factor or dataframe
rowwise into a \code{mids} object. The number of columns in the (incomplete) 
data \code{x$data} and \code{y} (or \code{y$data} if \code{y} is a \code{mids} object)
should be equal. If \code{y} is a \code{mids} object then the number of imputations 
in \code{x} and \code{y} should be equal.
}

\references{
van Buuren S and Groothuis-Oudshoorn K (2011).
\code{mice}: Multivariate Imputation by Chained Equations in \code{R}.
\emph{Journal of Statistical Software}, \bold{45}(3), 1-67.
\url{http://www.jstatsoft.org/v45/i03/}
}

\seealso{\code{\link{cbind.mids}}, \code{\link{ibind}}, \code{\link{mids}}}


\author{Karin Groothuis-Oudshoorn, Stef van Buuren, 2009}

\keyword{manip}
