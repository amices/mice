\name{version}
\alias{version}
\title{Echoes the package version number}
\description{
  Echoes the package version number
}
\usage{
     version(pkg="mice")
}
\arguments{
  \item{pkg}{A character vector with the package name.}
}

\value{
  A character vector containing the package name, version number and installed directory.
}

\examples{
version()
version("base")
}

\author{Stef van Buuren, Oct 2010}

\keyword{misc} 
