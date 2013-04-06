\name{fdgs}
\alias{fdgs}
\alias{growth}
\docType{data}
\title{Fifth Dutch Growth Study 2009}
\description{
Age, height, weight and region of 10030 children measured within the Fifth Dutch Growth Study 2009}
\usage{
data(fdgs)
}
\format{
  \code{fdgs} is a data frame with 10030 rows and 8 columns:
  \describe{
	\item{\code{id}}{Person number}
	\item{\code{reg}}{Region (factor, 5 levels)}
	\item{\code{age}}{Age (years)}
	\item{\code{sex}}{Sex (boy, girl)}
	\item{\code{hgt}}{Height (cm)}
	\item{\code{wgt}}{Weight (kg)}
	\item{\code{hgt.z}}{Height Z-score}
	\item{\code{wgt.z}}{Weight Z-score}	
  }
} 
\details{
The data set contains data from children of Dutch descent (biological parents are born 
in the Netherlands). Children with growth-related diseases were excluded. The data 
were used to construct new growth charts of children of Dutch descent (Schonbeck 2012), 
and to calculate overweight and obesity prevalence (Schonbeck 2011).

Some groups were underrepresented. 
Multiple imputation was used to create synthetic cases that were used to
correct for the nonresponse. See Van Buuren (2012), chapter 8 for details.
}


\source{
Schonbeck, Y., Talma, H., van Dommelen, P., Bakker, B., Buitendijk, S. E., Hirasing, R. A., van Buuren, S. (2011). 
Increase in prevalence of overweight in Dutch children and adolescents: 
A comparison of nationwide growth studies in 1980, 1997 and 2009. 
\emph{PLoS ONE}, \emph{6}(11), e27608. 
\url{http://www.stefvanbuuren.nl/publications/2011 Increased overweight - PLoS ONE.pdf}

Schonbeck, Y., Talma, H., van Dommelen, P., Bakker, B., Buitendijk, S. E., Hirasing, R. A., van Buuren, S. (2012). 
The tallest nation stopped growing taller. Submitted for publication. 

van Buuren, S. (2012). \emph{Flexible Imputation of Missing Data.} Boca Raton, FL: Chapman & Hall/CRC Press. 

}

\examples{

data <- data(fdgs)
summary(data)
}
\keyword{datasets}

