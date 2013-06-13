mice.impute.qpois <-
function (y, ry, x) 
{
    x <- cbind(1, as.matrix(x))
    fit <- glm.fit(x[ry, ], y[ry], family = quasipoisson(link = log))
    fit.sum <- summary.glm(fit)
    beta <- coef(fit)
    rv <- t(chol(fit.sum$cov.unscaled))
    beta.star <- beta + rv %*% rnorm(ncol(rv))
    p <- exp((x[!ry, ] %*% beta.star))
	  ret=rnbinom(n=length(p),size=(p/(fit.sum$dispersion-1)),mu=p) 
    return(ret)
}
