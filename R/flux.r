# flux.r
#
# Definition of flux chart
# SvB 15/12/2010

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


fico <- function(data){
  ic <- ici(data)
  unlist(lapply(data, FUN <- function(x) sum((!is.na(x)) & ic)/sum(!is.na(x))))
}
