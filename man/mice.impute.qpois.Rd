\name{mice.impute.qpois}
\alias{mice.impute.qpois}
\alias{mice.impute.qpois.boot}
\alias{mice.impute.nb}
\alias{mice.impute.nb.boot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multiple Imputation of Overdispersed Count Data
}
\description{
Imputes univariate missing data based on either a Bayesian Quasi-Poisson or Negative Binomial model, or by a boostrap Quasi-Poisson or Negative Binomial model.
}
\usage{
mice.impute.qpois(y, ry, x)
mice.impute.qpois.boot(y, ry, x)
mice.impute.nb(y, ry, x)
mice.impute.nb.boot(y, ry, x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
Numeric vector with incomplete data
}
  \item{ry}{
Response pattern of \code{y} (\code{TRUE}=observed, \code{FALSE}=missing)
}
  \item{x}{matrix with \code{length(y)} rows containing complete covariates
}
}

\details{
Overdispersed count data (i.e. count data where the variance is larger in comparison to the mean) are typically analyzed by a Negative Binomial (NB) model or by a Quasi-Poisson model. The Quasi-Poisson model is identical to the Poisson model, except that it estimates an additional dispersion parameter. The Negative Binomial model also estimates an additional parameter that adresses dispersion (often labeled alpha or theta). For details about the Quasi-Poisson or the Negative Binomial model, see Zeileis, Kleiber, & Jackman (2008), or Hilbe (2007).

Our functions create multiple imputations based on a Bayesian regression Quasi-Poisson model (function \code{mice.impute.qpois}) or NB model (function \code{mice.impute.nb}). For details about Bayesian reression, see Rubin (1987, p. 169--170). Bootstrap variants of these functions are also available (function name ending with ``.boot'').
 

The Bayesian method consists of the following steps:

1. Fit the model, and find bhat, the posterior mean, and V(bhat), the posterior variance of model parameters b.

2. Draw b.star from N(bhat,V(bhat)).

3. Compute predicted scores p using \code{exp(x[!ry, ] \%*\% b.star)}

4. Simulate imputations from appropriate distribution.

Quasi-Poisson imputation relies on the standard glm.fit, using the \code{quasipoisson} family. NB imputation uses \code{glm.nb()} from package \pkg{MASS}. 

The bootstrap method draws a bootstrap sample from \code{y[ry]} and \code{x[ry,]} and consists of the following steps:

1. Fit the model to the bootstrap sample and get model parameters b

2. Compute predicted scores p using \code{exp(x[!ry, ] \%*\% b)}

3. Simulate imputations from appropriate distribution.

}
\value{
\item{imp}{Numeric vector of length \code{sum(!ry)} with imputations}
}
\references{
Hilbe, J. M. (2007). \emph{Negative binomial regression}. Cambridge: Cambridge University Press.

Kleinke, K., & Reinecke, J. (2013). \emph{countimp 1.0 -- A multiple imputation package for incomplete count data} [Technical Report]. University of Bielefeld, Faculty of Sociology, available from \url{www.uni-bielefeld.de/soz/kds/pdf/countimp.pdf}.

Rubin, D. B. (1987). \emph{Multiple imputation for nonresponse in surveys}. New York: Wiley.

Zeileis, A., Kleiber, C., & Jackman, S. (2008). Regression models for count data in R. \emph{Journal of Statistical Software}, 27(8), 1–-25.
}

\author{
Kristian Kleinke and Jost Reinecke
}\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## simulate overdespersed count data
set.seed( 1234 )
b0 <- 1
b1 <- .75
b2 <- -.25
b3 <- .5
N <- 5000
x1 <- rnorm(N)
x2 <- rnorm(N)
x3 <- rnorm(N)
mu <- exp( b0 + b1 * x1 + b2 * x2 + b3 * x3 )
y <- rnegbin( N, theta = 2, mu )
NB <- data.frame( y, x1, x2, x3 )

## introduce MAR missingness to simulated data
total <- round( .2 * N )  ##number of missing data in y     
sm <- which( NB[,2] < mean( NB[,2] ) )  ##subset: cases with x2<mean(x2)
gr <- which( NB[,2] > mean( NB[,2] ) )	##subset: cases with x2>mean(x2)
sel.sm <- sample( sm, round( .2 * total ) )	##select cases to set as missing
sel.gr <- sample( gr, round( .8 * total ) )	##select cases to set as missing
sel <- c( sel.sm,sel.gr )
MNB <- NB
MNB[sel,1] <- NA	##delete selected data

## impute missing data, do the repeated data analysis and print a summary of pooled results
require(mice)
imp1 <- mice( MNB, method = c( "qpois", "", "", "" ), seed = 1234 )
imp2 <- mice( MNB, method = c( "nb" ,"" ,"" ,"" ), seed = 1234 )
res1 <- with( imp1, glm.nb( y ~ x1 + x2 + x3) )
res2 <- with( imp2, glm.nb( y ~ x1 + x2 + x3) )
print( pool.res1 <- summary( pool( res1 ) ) )
print( pool.res2 <- summary( pool( res2 ) ) )

## get MI inferences for the dispersion parameter
EST1 <- EST2 <- SE1 <- SE2 <- vector( length = 5, "list" )

for (i in 1:5) {
  EST1[[i]] <- res1$analyses[[i]]$theta 
  SE1[[i]] <- res1$analyses[[i]]$SE.theta 
  EST2[[i]] <- res2$analyses[[i]]$theta 
  SE2[[i]] <- res2$analyses[[i]]$SE.theta 
 } 
res.alpha <- rbind( miinference ( EST1, SE1 ), miinference( EST2, SE2 ) )
row.names( res.alpha ) <- c( "qpois", "nb" )
print( res.alpha )
}