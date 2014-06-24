### contributed by Gerko Vink (g.vink@uu.nl)

### imputation of incomplete covariate X that appears as X and X^2 
### in the complete-data model


#'Imputation of quadratric terms
#'
#'Imputes univariate missing data of incomplete variable that appears as both
#'main effect and quadratic effect in the complete-data model.
#'
#'This implements polynomial combination method. First, the polynomial
#'combination $Z = Y beta_1 + Y^2 beta_2$ is formed. $Z$ is imputed by
#'predictive mean matching, followed by a decomposition of the imputed data $Z$
#'into components $Y$ and $Y^2$.  See Van Buuren (2012, pp. 139-141) and Vink
#'et al (2012) for more details. The method ensures that 1) the imputed data
#'for $Y$ and $Y^2$ are mutually consistent, and 2) that provides unbiased
#'estimates of the regression weights in a complete-data linear regression that
#'use both $Y$ and $Y^2$.
#'
#'@aliases mice.impute.quadratic quadratic
#'@param y Incomplete data vector of length \code{n}
#'@param ry Vector of missing data pattern (\code{FALSE}=missing,
#'\code{TRUE}=observed)
#'@param x Matrix (\code{n} x \code{p}) of complete covariates.
#'@param ... Other named arguments.
#'@return A vector of length \code{nmis} with imputations.
#'@note There are two situations to consider. If only the linear term \code{Y}
#'is present in the data, calculate the quadratic term \code{YY} after
#'imputation. If both the linear term \code{Y} and the the quadratic term
#'\code{YY} are variables in the data, then first impute \code{Y} by calling
#'\code{mice.impute.quadratic()} on \code{Y}, and then impute \code{YY} by
#'passive imputation as \code{meth["YY"] <- "~I(Y^2)"}.  See example section
#'for details.  Generally, we would like \code{YY} to be present in the data if
#'we need to preserve quadratic relations between \code{YY} and any third
#'variables in the multivariate incomplete data that we might wish to impute.
#'@author Gerko Vink (University of Utrecht), \email{g.vink@@uu.nl}
#'@seealso \code{\link{mice.impute.pmm}}
#'@references van Buuren, S. (2012). \emph{Flexible Imputation of Missing
#'Data.} Boca Raton, FL: Chapman & Hall/CRC Press.
#'
#'Vink, G., Frank, L.E., van Buuren, S. (2012). Multiple Imputation of Squares.
#'\emph{Sociological Methods & Research}, accepted for publication.
#'@keywords datagen
#'@examples
#'require(lattice)
#'	
#'# Create Data
#'B1=.5
#'B2=.5
#'X<-rnorm(1000)
#'XX<-X^2
#'e<-rnorm(1000, 0, 1)
#'Y <- B1*X+B2*XX+e
#'dat <- data.frame(x=X, xx=XX, y=Y)
#'
#'# Impose 25 percent MCAR Missingness
#'dat[0 == rbinom(1000, 1, 1-.25), 1:2] <- NA
#'
#'# Prepare data for imputation
#'ini <- mice(dat, maxit=0)
#'meth <- c("quadratic", "~I(x^2)", "")
#'pred <- ini$pred
#'pred[,"xx"] <- 0
#'
#'# Impute data
#'imp <- mice(dat, meth=meth, pred=pred)
#'
#'# Pool results
#'pool(with(imp, lm(y~x+xx)))
#'
#'# Plot results
#'stripplot(imp)
#'plot(dat$x, dat$xx, col=mdc(1), xlab="x", ylab="xx")
#'points(complete(imp,1)$x[is.na(dat$x)], complete(imp,1)$xx[is.na(dat$x)], col=mdc(2))
#'
#'
#'@export
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
