mice.impute.qpois.boot <-
function (y, ry, x) 
{
    x <- cbind(1, as.matrix(x))
	xobs<-x[ry,]
	yobs<-y[ry]
	sel<-sample(1:length(yobs),length(yobs),replace=TRUE)
	xast<-xobs[sel,]
	yast<-yobs[sel]
    suppressWarnings(fit <- glm.fit(xast, yast, family = quasipoisson(link = log)))
    fit.sum <- summary.glm(fit)
    beta.star <- coef(fit)
    p <- exp((x[!ry, ] %*% beta.star))
	  if (fit.sum$dispersion>1)
	{ret=rnbinom(n=length(p),size=(p/(fit.sum$dispersion-1)),mu=p)}
else
 {
ret=rpois(length(p),p)
} 
return(ret)
}
