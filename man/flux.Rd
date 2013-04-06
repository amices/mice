\name{flux}
\alias{flux}
\alias{fluxplot}
\alias{fico}

\title{Influx and outflux of multivatiate missing data patterns}

\description{Influx and outflux are statistics of the missing data pattern. These statistics are useful in selecting predictors that should go into the imputation model.}

\usage{

flux(data, local = names(data))

fluxplot(data, local = names(data),
                 plot = TRUE, labels = TRUE,
                 xlim = c(0,1), ylim = c(0,1), las = 1,
                 xlab = "Influx", ylab = "Outflux",
                 main=paste("Influx-outflux pattern for", deparse(substitute(data))),
                 eqscplot = TRUE, pty= "s" ,
                 ...)

fico(data)

}
\arguments{
\item{data}{A data frame or a matrix containing the incomplete data. 
  Missing values are coded as NA's. }
\item{local}{A vector of names of columns of \code{data}. The default is to include 
	    all columns in the calculations.}
\item{plot}{Should a graph be produced?}
\item{labels}{Should the points be labeled?}
\item{xlim}{See \code{par}.}
\item{ylim}{See \code{par}.}
\item{las}{See \code{par}.}
\item{xlab}{See \code{par}.}
\item{ylab}{See \code{par}.}
\item{main}{See \code{par}.}
\item{eqscplot}{Should a square plot be produced?}
\item{pty}{See \code{par}.}
\item{...}{Further arguments passed to \code{plot()} or \code{eqscplot()}.}
}

\value{
\code{flux()} and returns a data frame with \code{ncol(data)} rows and six columns:
\item{pobs}{Proportion observed}
\item{influx}{Influx}
\item{outflux}{Outflux}
\item{ainb}{Average inbound statistic}
\item{aout}{Averege outbound statistic}
\item{fico}{Fraction of incomplete cases among cases with \code{Yj} observed}.

\code{fluxplot()} returns the same result, but invisible.

\code{fico()} returns a vector of length \code{ncol(data)} of FICO statistics.	
    
}
\details{
Infux and outflux have been proposed by Van Buuren (2012), chapter 4.

Influx is equal to the number of variable pairs \code{(Yj , Yk)} with \code{Yj} missing and \code{Yk} observed, divided by the total number of observed data cells. Influx depends on the proportion of missing data of the variable. Influx of a completely observed variable is equal to 0, whereas for completely missing variables wehave influx = 1. For two variables with the same proportion of missing data, the variable with higher influx is better connected to the observed data, and might thus be easier to impute.

Outflux is equal to the number of variable pairs with \code{Yj} observed and \code{Yk} missing, divided by the total number of incomplete data cells. Outflux is an indicator of the potential usefulness of \code{Yj} for imputing other variables. Outflux depends on the proportion of missing data of the variable. Outflux of a completely observed variable is equal to 1, whereas outflux of a completely missing variable is equal to 0. For two variables having the same proportion of missing data, the variable with higher outflux is better connected to the missing data, and thus potentially more useful for imputing other variables.

FICO is an outbound statistic defined by the fraction of incomplete cases among cases with \code{Yj} observed (White and Carlin, 2010).

}
\references{
van Buuren, S. (2012). \emph{Flexible Imputation of Missing Data.} Boca Raton, FL: Chapman & Hall/CRC Press. 

White, I.R., Carlin, J.B. (2010).
Bias and efficiency of multiple imputation compared with complete-case analysis for missing covariate values.
\emph{Statistics in Medicine}, \emph{29}, 2920-2931.

}


\author{Stef van Buuren, 2012}     

\keyword{misc} 
