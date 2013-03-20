### contributed by Gerko Vink (g.vink@uu.nl)

### imputation of incomplete covariate X that appears as X and X^2 
### in the complete-data model
mice.impute.quadratic <-function (y, ry, x, ...){
	x <- cbind(1, as.matrix(x))
	#x <- matrix(x,length(x),1)
	#create the square of y
	y2 <- y^2
	
	#create z based on B1 * y + B2 * y^2
 	parm  <- .norm.draw(x[,2], ry, cbind(1, y, y2))	
	zobs <- cbind(y, y2) %*% parm$coef[-1]  
	
	#impute z
	zmis <- mice.impute.pmm(zobs, ry, x)
	zstar <- zobs
	zstar[!ry] <- zmis
	zstar <- as.vector(zstar) #Otherwise the predictfunction crashes (nmatrix.1 error)
	
	#decompositions of z into roots
	b1 <- parm$coef[2]
	b2 <- parm$coef[3]
	y.low <- -(1/(2 * b2)) * (sqrt(4 * b2 * zstar + b1^2) + b1)
	y.up  <-  (1/(2 * b2)) * (sqrt(4 * b2 * zstar + b1^2) - b1)

	#calculate the abscissa at the parabolic minimum/maximum
	y.min <- -b1 / 2 * b2
	
	#calculate regression parameters for 
	q <- x[,2]
	vobs  <- glm(y > y.min ~ q + zstar + q * zstar, subset=ry, family=binomial)
	
	#impute Vmis
	newdata <- data.frame(q=x[!ry,2], zstar=zstar[!ry])
	prob    <- predict(vobs, newdata=newdata, type="response", na.action=na.exclude)
	idy 	<- rbinom(sum(!ry), 1, prob=prob)
	
	#create final imputation
	ystar 	<- y.low[!ry]
	ystar[idy==1] <- y.up[!ry][idy==1]
	
	return(ystar)
}
