mice.impute.nb.boot <-
function (y, ry, x) 
{
	Y=y[ry]
	X=x[ry,]

	nam=paste("V",1:ncol(X),sep="")
	colnames(X)=nam

	form=paste("Y","~",paste(nam,collapse="+"))
	form=as.formula(form)
	dat=data.frame(Y,X)


	sel<-sample(1:length(Y),length(Y),replace=TRUE)
	datstar<-dat[sel,]

	fit <- glm.nb(form , data=datstar)

	fit.sum <- summary(fit)
	b.star <- coef(fit)
    p <- exp((data.matrix(cbind(1,x[!ry, ]))%*% b.star))
	ret=rnegbin(n=length(p),theta=fit.sum$theta,mu=p)
	return(ret)
}
