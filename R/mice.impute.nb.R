mice.impute.nb <-
function (y, ry, x) 
{
  Y=y[ry]
	X=x[ry,]

	nam=paste("V",1:ncol(X),sep="")
	colnames(X)=nam

	form=paste("Y","~",paste(nam,collapse="+"))
	form=as.formula(form)
	dat=data.frame(Y,X)

	fit <- glm.nb(form , data=dat)

	fit.sum <- summary(fit)
	beta <- coef(fit)
  rv <- t(chol(fit.sum$cov.unscaled))
	b.star <- beta + rv %*% rnorm(ncol(rv))
  p <- exp((data.matrix(cbind(1,x[!ry, ]))%*% b.star))
	return(rnegbin(n=length(p),theta=fit.sum$theta,mu=p))
}
