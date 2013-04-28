#'Influx and outflux of multivariate missing data patterns
#'
#'Influx and outflux are statistics of the missing data pattern. These
#'statistics are useful in selecting predictors that should go into the
#'imputation model.
#'
#'Infux and outflux have been proposed by Van Buuren (2012), chapter 4.
#'
#'Influx is equal to the number of variable pairs \code{(Yj , Yk)} with
#'\code{Yj} missing and \code{Yk} observed, divided by the total number of
#'observed data cells. Influx depends on the proportion of missing data of the
#'variable. Influx of a completely observed variable is equal to 0, whereas for
#'completely missing variables wehave influx = 1. For two variables with the
#'same proportion of missing data, the variable with higher influx is better
#'connected to the observed data, and might thus be easier to impute.
#'
#'Outflux is equal to the number of variable pairs with \code{Yj} observed and
#'\code{Yk} missing, divided by the total number of incomplete data cells.
#'Outflux is an indicator of the potential usefulness of \code{Yj} for imputing
#'other variables. Outflux depends on the proportion of missing data of the
#'variable. Outflux of a completely observed variable is equal to 1, whereas
#'outflux of a completely missing variable is equal to 0. For two variables
#'having the same proportion of missing data, the variable with higher outflux
#'is better connected to the missing data, and thus potentially more useful for
#'imputing other variables.
#'
#'FICO is an outbound statistic defined by the fraction of incomplete cases
#'among cases with \code{Yj} observed (White and Carlin, 2010).
#'
#'@aliases flux
#'@param data A data frame or a matrix containing the incomplete data.  Missing
#'values are coded as NA's.
#'@param local A vector of names of columns of \code{data}. The default is to
#'include all columns in the calculations.
#'@return A data frame with \code{ncol(data)} rows and six columns:
#'pobs = Proportion observed, 
#'influx = Influx
#'outflux = Outflux
#'ainb = Average inbound statistic
#'aout = Averege outbound statistic
#'fico = Fraction of incomplete cases among cases with \code{Yj} observed
#'@seealso \code{\link{fluxplot}}, \code{\link{md.pattern}}, \code{\link{fico}}
#'@author Stef van Buuren, 2012
#'@references van Buuren, S. (2012). \emph{Flexible Imputation of Missing
#'Data.} Boca Raton, FL: Chapman & Hall/CRC Press.
#'
#'White, I.R., Carlin, J.B. (2010). Bias and efficiency of multiple imputation
#'compared with complete-case analysis for missing covariate values.
#'\emph{Statistics in Medicine}, \emph{29}, 2920-2931.
#'@keywords misc
#'@export
flux <- function(data, local=names(data)){
  ## calculates influx and outflux statistics
  ## of the missing data pattern
  x <- colMeans(!is.na(data))
  pat <- md.pairs(data)
  pat$rr <- pat$rr[local,,drop=FALSE]
  pat$rm <- pat$rm[local,,drop=FALSE]
  pat$mr <- pat$mr[local,,drop=FALSE]
  pat$mm <- pat$mm[local,,drop=FALSE]
  ainb <- apply(pat$mr/(pat$mr+pat$mm), 1, mean)
  aout <- apply(pat$rm/(pat$rm+pat$rr), 1, mean)
  fico <- fico(data)
  outflux <-  rowSums(pat$rm)/(rowSums(pat$rm+pat$mm))
  influx <- rowSums(pat$mr)/(rowSums(pat$mr+pat$rr))
  return(data.frame(pobs=x, influx=influx, outflux=outflux, ainb=ainb, aout=aout, fico=fico))
}


#'Fluxplot of the missing data pattern
#'
#'Influx and outflux are statistics of the missing data pattern. These
#'statistics are useful in selecting predictors that should go into the
#'imputation model.
#'
#'Infux and outflux have been proposed by Van Buuren (2012), chapter 4.
#'
#'Influx is equal to the number of variable pairs \code{(Yj , Yk)} with
#'\code{Yj} missing and \code{Yk} observed, divided by the total number of
#'observed data cells. Influx depends on the proportion of missing data of the
#'variable. Influx of a completely observed variable is equal to 0, whereas for
#'completely missing variables wehave influx = 1. For two variables with the
#'same proportion of missing data, the variable with higher influx is better
#'connected to the observed data, and might thus be easier to impute.
#'
#'Outflux is equal to the number of variable pairs with \code{Yj} observed and
#'\code{Yk} missing, divided by the total number of incomplete data cells.
#'Outflux is an indicator of the potential usefulness of \code{Yj} for imputing
#'other variables. Outflux depends on the proportion of missing data of the
#'variable. Outflux of a completely observed variable is equal to 1, whereas
#'outflux of a completely missing variable is equal to 0. For two variables
#'having the same proportion of missing data, the variable with higher outflux
#'is better connected to the missing data, and thus potentially more useful for
#'imputing other variables.
#'
#'@aliases fluxplot
#'@param data A data frame or a matrix containing the incomplete data.  Missing
#'values are coded as NA's.
#'@param local A vector of names of columns of \code{data}. The default is to
#'include all columns in the calculations.
#'@param plot Should a graph be produced?
#'@param labels Should the points be labeled?
#'@param xlim See \code{par}.
#'@param ylim See \code{par}.
#'@param las See \code{par}.
#'@param xlab See \code{par}.
#'@param ylab See \code{par}.
#'@param main See \code{par}.
#'@param eqscplot Should a square plot be produced?
#'@param pty See \code{par}.
#'@param \dots Further arguments passed to \code{plot()} or \code{eqscplot()}.
#'@return An invisible data frame with \code{ncol(data)} rows and six columns: 
#'pobs = Proportion observed, 
#'influx = Influx
#'outflux = Outflux
#'ainb = Average inbound statistic
#'aout = Averege outbound statistic
#'fico = Fraction of incomplete cases among cases with \code{Yj} observed
#'@seealso \code{\link{flux}}, \code{\link{md.pattern}}, \code{\link{fico}}
#'@author Stef van Buuren, 2012
#'@references van Buuren, S. (2012). \emph{Flexible Imputation of Missing
#'Data.} Boca Raton, FL: Chapman & Hall/CRC Press.
#'
#'White, I.R., Carlin, J.B. (2010). Bias and efficiency of multiple imputation
#'compared with complete-case analysis for missing covariate values.
#'\emph{Statistics in Medicine}, \emph{29}, 2920-2931.
#'@keywords misc
#'@export
fluxplot <- function(data, local=names(data),
                 plot=TRUE, labels=TRUE,
                 xlim=c(0,1), ylim=c(0,1), las=1,
                 xlab="Influx", ylab="Outflux",
                 main=paste("Influx-outflux pattern for",deparse(substitute(data))),
                 eqscplot = TRUE, pty="s",
                 ...) {
f <- flux(data, local)
  if (plot){
    if (eqscplot)
      eqscplot(x=f$influx, y=f$outflux, type='n',
          main=main,
          xlab=xlab, ylab=ylab,
          xlim=xlim, ylim=ylim,
          las=las, pty=pty, ...)
    else
      plot(x=f$influx, y=f$outflux, type='n',
          main=main,
          xlab=xlab, ylab=ylab,
          xlim=xlim, ylim=ylim,
          las=las, pty=pty, ...)
    abline(1,-1,lty=2)
    if (labels) text(x=f$influx, y=f$outflux, label=names(data), ...)
    else points(x=f$influx, y=f$outflux, ...)
  }
  invisible(data.frame(f))
}

#' Fraction of incomplete cases among cases with observed
#'
#'FICO is an outbound statistic defined by the fraction of incomplete cases
#'among cases with \code{Yj} observed (White and Carlin, 2010).
#'
#'@aliases fico
#'@param data A data frame or a matrix containing the incomplete data.  Missing
#'values are coded as NA's.
#'@return A vector of length \code{ncol(data)} of FICO statistics.
#'@seealso \code{\link{fluxplot}}, \code{\link{flux}}, \code{\link{md.pattern}}
#'@author Stef van Buuren, 2012
#'@references van Buuren, S. (2012). \emph{Flexible Imputation of Missing
#'Data.} Boca Raton, FL: Chapman & Hall/CRC Press.
#'
#'White, I.R., Carlin, J.B. (2010). Bias and efficiency of multiple imputation
#'compared with complete-case analysis for missing covariate values.
#'\emph{Statistics in Medicine}, \emph{29}, 2920-2931.
#'@keywords misc
#'@export
fico <- function(data){
  ic <- ici(data)
  unlist(lapply(data, FUN <- function(x) sum((!is.na(x)) & ic)/sum(!is.na(x))))
}
