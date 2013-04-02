# -------------------------MICE.IMPUTE.LOGREG-------------------------

#'Imputation by logistic regression
#'
#'Imputes univariate missing data using logistic regression.
#'
#'Imputation for binary response variables by the Bayesian logistic regression
#'model (Rubin 1987, p. 169-170) or bootstrap logistic regression model.  The
#'Bayesian method consists of the following steps: \enumerate{ \item Fit a
#'logit, and find (bhat, V(bhat)) \item Draw BETA from N(bhat, V(bhat)) \item
#'Compute predicted scores for m.d., i.e. logit-1(X BETA) \item Compare the
#'score to a random (0,1) deviate, and impute.} The method relies on the
#'standard \code{glm.fit} function. Warnings from \code{glm.fit} are
#'suppressed.  The bootstrap method draws a bootstrap sample from \code{y[ry]}
#'and \code{x[ry,]}.  Perfect prediction is handled by the data augmentation
#'method.
#'
#'@aliases mice.impute.logreg
#'@param y Incomplete data vector of length \code{n}
#'@param ry Vector of missing data pattern of length \code{n}
#'(\code{FALSE}=missing, \code{TRUE}=observed)
#'@param x Matrix (\code{n} x \code{p}) of complete covariates.
#'@param ... Other named arguments.
#'@return A vector of length \code{nmis} with imputations (0 or 1).
#'@author Stef van Buuren, Karin Groothuis-Oudshoorn, 2000, 2011
#'@seealso \code{\link{mice}}, \code{\link{glm}}, \code{\link{glm.fit}}
#'@references Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}:
#'Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#'Statistical Software}, \bold{45}(3), 1-67.
#'\url{http://www.jstatsoft.org/v45/i03/}
#'
#'Brand, J.P.L. (1999). Development, Implementation and Evaluation of Multiple
#'Imputation Strategies for the Statistical Analysis of Incomplete Data Sets.
#'Ph.D. Thesis, TNO Prevention and Health/Erasmus University Rotterdam. ISBN
#'90-74479-08-1.
#'
#'Venables, W.N. & Ripley, B.D. (1997). Modern applied statistics with S-Plus
#'(2nd ed). Springer, Berlin.
#'
#'White, I., Daniel, R. and Royston, P (2010). Avoiding bias due to perfect
#'prediction in multiple imputation of incomplete categorical variables.
#'Computational Statistics and Data Analysis, 54:22672275.
#'@keywords datagen
#'@export
mice.impute.logreg <- function(y, ry, x, ...) {
    # Imputation for binary response variables by the Bayesian 
    # logistic regression model. See Rubin (1987, p. 169-170) for
    # a description of the method.
    #
    # The method consists of the following steps:
    # 1. Fit a logit, and find (bhat, V(bhat))
    # 2. Draw BETA from N(bhat, V(bhat))
    # 3. Compute predicted scores for m.d., i.e. logit-1(X BETA)
    # 4. Compare the score to a random (0,1) deviate, and impute.
    #
    # Authors: Stef van Buuren, Karin Groothuis-Oudshoorn, 1998
    #
    # Added statements May 2009
    aug <- augment(y, ry, x, ...)
    x <- aug$x
    y <- aug$y
    ry <- aug$ry
    w <- aug$w
    # end added statements May 2009
    
    x <- cbind(1, as.matrix(x))
    expr <- expression(glm.fit(x[ry, ], y[ry], family = binomial(link = logit), weights = w[ry]))
    fit <- suppressWarnings(eval(expr))
    fit.sum <- summary.glm(fit)
    beta <- coef(fit)
    rv <- t(chol(fit.sum$cov.unscaled))
    beta.star <- beta + rv %*% rnorm(ncol(rv))
    p <- 1/(1 + exp(-(x[!ry, ] %*% beta.star)))
    vec <- (runif(nrow(p)) <= p)
    vec[vec] <- 1
    if (is.factor(y)) {
        vec <- factor(vec, c(0, 1), levels(y))
    }
    return(vec)
}

# -------------------------MICE.IMPUTE.LOGREG.BOOT--------------------

mice.impute.logreg.boot <- function(y, ry, x, ...) {
    # mice.impute.logreg.boot
    # 
    # Bootstrap version of mice.impute.logreg.
    # 
    # Author: Stef van Buuren, 2011
    # 
    # draw a bootstrap sample for yobs and xobs
    xobs <- x[ry, ]
    yobs <- y[ry]
    n1 <- sum(ry)
    n0 <- sum(!ry)
    s <- sample(n1, n1, replace = TRUE)
    doty <- y
    doty[ry] <- yobs[s]
    dotx <- x
    dotx[ry, ] <- xobs[s, ]
    
    x <- dotx
    y <- doty
    
    x <- cbind(1, as.matrix(x))
    expr <- expression(glm.fit(x[ry, ], y[ry], family = binomial(link = logit)))
    fit <- suppressWarnings(eval(expr))
    beta.star <- beta <- coef(fit)
    p <- 1/(1 + exp(-(x[!ry, ] %*% beta.star)))
    vec <- (runif(nrow(p)) <= p)
    vec[vec] <- 1
    if (is.factor(y)) {
        vec <- factor(vec, c(0, 1), levels(y))
    }
    return(vec)
}



augment <- function(y, ry, x, maxcat = 50, ...) {
    # define augmented data for stabilizing logreg and polyreg
    # by the ad hoc procedure of White, Daniel & Royston, CSDA, 2010
    # This function will prevent augmented data beyond the min and
    # the max of the data
    # Input:
    # x: numeric data.frame (n rows)
    # y: factor or numeric vector (lengt n)
    # ry: logical vector (length n)
    # Output:
    # return a list with elements y, ry, x, and w with length n+2*(ncol(x))*length(levels(y))
    # SvB May 2009
    icod <- sort(unique(unclass(y)))
    k <- length(icod)
    if (k > maxcat) 
        stop(paste("Maximum number of categories (", maxcat, ") exceeded", sep = ""))
    p <- ncol(x)
    
    # skip augmentation if there are no predictors
    if (p == 0) 
        return(list(y = y, ry = ry, x = x, w = rep(1, length(y))))
    
    ## skip augmentation if there is only 1 missing value 12jul2012 this need to be fixed 12jul2011
    if (sum(!ry) == 1) 
        return(list(y = y, ry = ry, x = x, w = rep(1, length(y))))
    
    # calculate values to augment
    mean <- apply(x, 2, mean)
    sd <- sqrt(apply(x, 2, var))
    minx <- apply(x, 2, min)
    maxx <- apply(x, 2, max)
    nr <- 2 * p * k
    a <- matrix(mean, nrow = nr, ncol = p, byrow = TRUE)
    b <- matrix(rep(c(rep(c(0.5, -0.5), k), rep(0, nr)), length = nr * p), nrow = nr, ncol = p, byrow = FALSE)
    c <- matrix(sd, nrow = nr, ncol = p, byrow = TRUE)
    d <- a + b * c
    d <- pmax(matrix(minx, nrow = nr, ncol = p, byrow = TRUE), d)
    d <- pmin(matrix(maxx, nrow = nr, ncol = p, byrow = TRUE), d)
    e <- rep(rep(icod, each = 2), p)
    
    dimnames(d) <- list(paste("AUG", 1:nrow(d), sep = ""), dimnames(x)[[2]])
    xa <- rbind.data.frame(x, d)
    # beware, concatenation of factors
    if (is.factor(y)) 
        ya <- as.factor(levels(y)[c(y, e)]) else ya <- c(y, e)
    rya <- c(ry, rep(TRUE, nr))
    wa <- c(rep(1, length(y)), rep((p + 1)/nr, nr))
    
    return(list(y = ya, ry = rya, x = xa, w = wa))
}
