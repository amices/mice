\name{mice.impute.pois}
\alias{mice.impute.pois}
\alias{mice.impute.pois.boot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multiple Imputation of Poisson Distributed Count Data
}
\description{
Imputes univariate missing data using either Bayesian Poisson regression or a bootstrap Poisson regression model.
}
\usage{
mice.impute.pois(y, ry, x)
mice.impute.pois.boot(y, ry, x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
Numeric vector with incomplete data
}
  \item{ry}{
Response pattern of \code{y} (\code{TRUE}=observed, \code{FALSE}=missing)
}
  \item{x}{
matrix with \code{length(y)} rows containing complete covariates
}
}
\details{
Imputation for count response variables by a Bayesian regression Poisson model (function \code{mice.impute.pois}), or by a bootstrap Poisson regression model (function \code{mice.impute.pois.boot}). For details about Bayesian regression, see Rubin (1987, pp. 169--170). The Bayesian method consists of the following steps:

1. Fit a Poisson model, and find bhat, the posterior mean, and V(bhat), the posterior variance of model parameters b.

2. Draw b.star from N(bhat,V(bhat)).

3. Compute predicted scores p using \code{exp(x[!ry, ] \%*\% b.star)}

4. Draw imputations from \code{rpois(length(p),p)} with mean \code{p}.

The method relies on the standard glm.fit function. Warnings from glm.fit are suppressed. 

The bootstrap method draws a bootstrap sample from \code{y[ry]} and \code{x[ry,]} and consists of the following steps:

1. Fit a Poisson model to the bootstrap sample and get model parameters b

2. Compute predicted scores p using \code{exp(x[!ry, ] \%*\% b)}

3. Draw imputations from \code{rpois(length(p),p)} with mean \code{p}.

}
\value{
\item{imp}{Numeric vector of length \code{sum(!ry)} with imputations}
}
\references{
Kleinke, K., & Reinecke, J. (2013). \emph{countimp 1.0 -- A multiple imputation package for incomplete count data} [Technical Report]. University of Bielefeld, Faculty of Sociology, available from \url{www.uni-bielefeld.de/soz/kds/pdf/countimp.pdf}.

Rubin, D. B. (1987). \emph{Multiple imputation for nonresponse in surveys}. New York: Wiley.
}


\author{
Kristian Kleinke and Jost Reinecke
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  
}
\examples{
## simulate Poisson data
set.seed( 1234 )
b0 <- 1
b1 <- .75
b2 <- -.25
b3 <- .5
N <- 5000
x1 <- rnorm(N)
x2 <- rnorm(N)
x3 <- rnorm(N)
lam <- exp( b0 + b1 * x1 + b2 * x2 + b3 * x3 )
y <- rpois( N, lam )
POIS <- data.frame( y, x1, x2, x3 )

## introduce MAR missingness to simulated data
generate.md <- function( data, pos = 1, Z = 2, pmis = .5, strength = c( .5, .5 ) ) 
{
  total <- round( pmis * nrow(data) )
  sm <- which( data[,Z] < mean( data[,Z] ) )
  gr <- which( data[,Z] > mean( data[,Z] ) )
  sel.sm <- sample( sm, round( strength[1] * total ) )
  sel.gr <- sample( gr, round( strength[2] * total ) )
  sel <- c( sel.sm, sel.gr )
  data[sel,pos] <- NA
  return(data)
}
MPOIS <- generate.md( POIS, pmis = .2, strength = c( .2, .8) )

## impute missing data, do the repeated data anylsis and print a summary of the pooled results
require( "mice" )
imp <- mice( MPOIS, method = c( "pois" ,"" ,"" ,"" ), print = FALSE, seed = 1234)
res <- with( imp, glm( y ~ x1 + x2 + x3, family = "poisson" ) )
print( pool.res <- summary( pool( res ) ) )}