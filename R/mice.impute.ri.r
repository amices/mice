### ------------------------MICE.IMPUTE.RI----------------------

#'Imputation by the random indicator method for nonignorable data
#'
#'Imputes univariate missing data using the random indicator method. 
#'This method estimates an offset between the distribution of the 
#'observed and missing data using an algorithm that iterates
#'over the response model and the imputation model. 
#'
#'@aliases mice.impute.ri ri
#'@param y Incomplete data vector of length \code{n}
#'@param ry Vector of missing data pattern (\code{FALSE}=missing,
#'\code{TRUE}=observed)
#'@param x Matrix (\code{n} x \code{p}) of complete covariates.
#'@param ri.maxit Number of inner iterations
#'@param \dots Other named arguments passed down to \code{.norm.draw()}
#'@return A vector of length \code{nmis} with imputations.
#'@author Shahab Jolani (University of Utrecht) \email{s.jolani@@uu.nl}
#'@references Jolani, S. (2012). 
#'\emph{Dual Imputation Strategies for Analyzing Incomplete Data}. 
#'Disseration. University of Utrecht, Dec 7 2012.
#'\url{http://igitur-archive.library.uu.nl/dissertations/2012-1120-200602/Jolani.pdf}
#'@keywords datagen
#'@export
mice.impute.ri <- function(y, ry, x, ri.maxit = 10, ...)
{
    x <- cbind(1, as.matrix(x))
    xy <- x  # for the moment, assume xr = xy
    xr <- xy
    y.dot <- y
    y.dot[!ry] <- mice.impute.sample(y, ry)
    for (k in 1:ri.maxit){
        r.dot <- .r.draw(y.dot, ry, xr, ...)
        y.dot <- .y.draw(y, ry, r.dot, xy, ...)
    }
    return(y.dot[!ry])
}

# generting a realization of the response indicator r 
.r.draw <- function(ydot, ry, xr, ...)
{
    n <- length(ry)
    xr <- cbind(xr, ydot)
    expr <- expression(glm.fit(xr, ry, family = binomial(link = logit)))
    fit <- suppressWarnings(eval(expr))
    fit.sum <- summary.glm(fit)
    psi <- coef(fit)
    rv <- t(chol(sym(fit.sum$cov.unscaled)))
    psi.star <- psi + rv %*% rnorm(ncol(rv))
    p <- 1/(1 + exp(-(xr %*% psi.star)))
    vec <- (runif(nrow(p)) <= p)
    vec[vec] <- 1
	rdot <- vec[1:n]
return(rdot)
}

# Imputation of y given rdot
.y.draw <- function(y, ry, rdot, xy, ...)
{
    parm <- .norm.draw(y, ry, cbind(xy, rdot), ...)
    if (all(rdot[ry] == 1) | all(rdot[ry] == 0)) parm$coef[length(parm$coef)] <- 0
    ydot <- y
    rydot <- as.logical(rdot)
    ydot[!ry] <- xy[!ry,] %*% parm$beta[-length(parm$coef),] + rnorm(sum(!ry)) * parm$sigma
    ydot[!ry & !rydot] <- ydot[!ry & !rydot] - parm$coef[length(parm$coef)]
    return(ydot)
}

