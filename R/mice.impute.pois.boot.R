mice.impute.pois.boot <-
function (y, ry, x) 
{
    x <- cbind(1, as.matrix(x))
	xobs<-x[ry,]
	yobs<-y[ry]
	sel<-sample(1:length(yobs),length(yobs),replace=TRUE)
	xast<-xobs[sel,]
	yast<-yobs[sel]
    suppressWarnings(fit <- glm.fit(xast, yast, family = poisson(link = log)))
    fit.sum <- summary.glm(fit)
    beta.star <- coef(fit)
    p <- exp((x[!ry, ] %*% beta.star))
	return(rpois(length(p),p))
}
