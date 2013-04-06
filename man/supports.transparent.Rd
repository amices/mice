\name{supports.transparent}
\alias{supports.transparent}
\alias{transparent}

\title{Does the current graphic device support semi-transparent foreground colors?}

\usage{
supports.transparent()
}

\description{
This function is used by \code{mdc()} to find out whether the current device supports semi-transparent foreground colors. 
}


\details{
The function calls the function \code{dev.capabilities()} from the package \code{grDevices}. The function return \code{FALSE} if the status of the current device is unknown.
}
\value{\code{TRUE} or \code{FALSE}}

\seealso{
    \code{\link{mdc}}
    \code{\link{dev.capabilities}}
}
\keyword{hplot}

\examples{
supports.transparent()
}
