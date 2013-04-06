\name{ibind}
\alias{ibind}
\title{Combine imputations fitted to the same data}
\usage{
    ibind(x, y)
}

\description{Combine imputations fitted to the same data}

\arguments{
  \item{x}{A \code{mids} object.}
  \item{y}{A \code{mids} object.}
}

\value{
  \item{call}{A vector, with first argument the \code{mice} statement that created \code{x} and second argument the 
  call to \code{ibind()}.}
  \item{data}{The incomplete data in \code{x} and \code{y}.}
  \item{m}{Defined as \code{x$m}+\code{y$m}, the total number of imputations from \code{x} and \code{y}.}
  \item{nmis}{Defined as \code{x$nmis}, an array containing the number of missing observations per column of \code{x$data}.}
  \item{imp}{A combination of \code{x$imp} and \code{y$imp}.}
  \item{method}{Defined as \code{x$method}.}
  \item{predictorMatrix}{ Defined as \code{x$predictorMatrix}.}
  \item{visitSequence}{\code{x$visitSequence}}
  \item{seed}{Defined as \code{x$seed}.}
  \item{iteration}{
    Last Gibbs sampling iteration number, \code{x$iteration}.}
  \item{lastSeedValue}{Defined as \code{x$lastSeedValue}.}
  \item{chainMean}{
    Combination of \code{x$chainMean} and \code{y$chainMean}. }
  \item{chainVar}{
    Combination of \code{x$chainVar} and \code{y$chainVar}. }
  \item{pad}{Defined as \code{x$pad} (which should equal \code{y$pad}).}
}

\details{
This function combines two \code{mids} objects \code{x} and \code{y} into a
single \code{mids} object. The two \code{mids} objects should have the same underlying 
multiple imputation model and should
be fitted on exactly the same dataset. If the number of imputations in \code{x}
is \code{m(x)} and in \code{y} is \code{m(y)} then the combination of both
objects contains \code{m(x)+m(y)} imputations.
}

\seealso{\code{\link{rbind.mids}}, \code{\link{cbind.mids}}, \code{\link{mids}}}


\author{Karin Groothuis-Oudshoorn, Stef van Buuren, 2009}

\keyword{manip}

