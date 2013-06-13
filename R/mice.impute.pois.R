mice.impute.pois <-
function (y, ry, x) 
{
    x <- cbind(1, as.matrix(x))
    suppressWarnings(fit <- glm.fit(x[ry, ], y[ry], family = poisson(link = log)))
    fit.sum <- summary.glm(fit)
    beta <- coef(fit)
    rv <- t(chol(fit.sum$cov.unscaled))
    beta.star <- beta + rv %*% rnorm(ncol(rv))
    p <- exp((x[!ry, ] %*% beta.star))
	return(rpois(length(p),p))
}
