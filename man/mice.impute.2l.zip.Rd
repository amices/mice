\name{mice.impute.2l.zip}
\alias{mice.impute.2l.zip}
\alias{mice.impute.2l.zip.boot}
\alias{mice.impute.2l.zinb}
\alias{mice.impute.2l.zinb.boot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multiple Imputation of Zero-Inflated Count Data
}
\description{
Imputes univariate missing data using either a Bayesian regression ZIP (zero-inflated Poisson) or ZINB (zero-inflated Negative Binomial) or a bootstrap ZIP or ZINB regression model. Note that despite the function name \samp{.2l.} these functions do \emph{not} fit a two-level model. The argument \samp{.2l.} in the function name simply enables \pkg{mice} functionalities that are needed for the specification of more complex imputation models (please refer to the ``Details'' section).
}
\usage{
mice.impute.2l.zip(y, ry, x, type)
mice.impute.2l.zip.boot(y, ry, x, type)
mice.impute.2l.zinb(y, ry, x, type)
mice.impute.2l.zinb.boot(y, ry, x, type)
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

  \item{type}{
vector of length \code{ncol(x)} determining the imputation model; \code{type} is automatically extracted from the \code{predictorMatrix} argument of \code{mice()}. See \code{mice} user's manual for details about how to specify imputation models.
}
}
\details{
The functions multiply impute incomplete zero-inflated count data using the \code{zeroinfl()} function from package \pkg{pscl} (Zeileis, Kleiber, & Jackman, 2008). The functions abort with an error message if this package is not available. Zero-inflation models are mixture models and specify a zero model (here a logit model, determining if the observational unit has a ``certain zero'' or not) and a count model (here a Poisson or Negative Binomial model), determining, what count - both zero and non-zero - the observational unit has. A different set of covariates (predictors) may be used for the zero and the count models.
\tabular{ll}{
\code{y,ry,x} \tab these arguments are obtained from \code{mice()} \cr
\code{type} \tab \code{type} is automatically extracted from the respective row of the \code{predictorMatrix}. Allowed entries in the \code{predictorMatrix} are: 0=variable not included in imputation model; 1=variable will be included in zero \emph{and} count model; 2=count model only variable; 3=zero model only variable\cr
}%%  ~~ If necessary, more details than the description above ~~

The Bayesian regression variants (see Rubin 1987, pp. 169--170) consist of the following steps:

1. Fit the ZIP or ZINB model, and find bhat, the posterior mean, and V(bhat), the posterior variance of model parameters b.

2. Draw b* from N(bhat,V(bhat)).

3. Compute predicted probabilities for observing each count \code{p}

4. Draw imputations from observed counts with selection probabilities \code{p}


The bootstrap functions (function names ending with ``.boot'') draw a bootstrap sample from \code{y[ry]} and \code{x[ry,]} and consist of the following steps:

1. Fit the ZIP or ZINB model to the bootstrap sample and get model parameters b

2. Compute predicted probabilities for observing each count \code{p}

3. Draw imputations from observed counts with selection probabilities \code{p}

}
\value{
\item{imp}{Numeric vector of length \code{sum(!ry)} with imputations}
}
\references{
Kleinke, K., & Reinecke, J. (2013a). Multiple Imputation of incomplete zero-inflated count data. \emph{Statistica Neerlandica}, available from \url{http://onlinelibrary.wiley.com/doi/10.1111/stan.12009/abstract}.

Kleinke, K., & Reinecke, J. (2013b). \emph{countimp 1.0 -- A multiple imputation package for incomplete count data} [Technical Report]. University of Bielefeld, Faculty of Sociology, available from \url{www.uni-bielefeld.de/soz/kds/pdf/countimp.pdf}.

Rubin, D. B. (1987). \emph{Multiple imputation for nonresponse in surveys}. New York: Wiley.

Zeileis, A., Kleiber, C., & Jackman, S. (2008). Regression models for count data in R. \emph{Journal of Statistical Software}, 27(8), 1--25.
}
\author{
Kristian Kleinke and Jost Reinecke
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Simulate zero-inflated NB data
b0 <- 1
b1 <- .3
b2 <- .3
c0 <- 0
c1 <- 2
theta <- 1
require("pscl")
set.seed(1234)
N <- 10000
x1 <- rnorm(N)
x2 <- rnorm(N)
x3 <- rnorm(N)
mu <- exp( b0 + b1 * x1 + b2 * x2 )
yzinb <- rnegbin( N, mu, theta)
pzero <- plogis( c1 * x3 )        # zero-infl. prob. depends on x3
## Introduce zero-inflation
uni <- runif(N)
yzinb[uni < pzero] <- 0
zinbdata<-data.frame(yzinb,x1,x2,x3)

## Generate MAR missingness
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
zinbmdata <- generate.md( zinbdata, pmis = .3, strength = c( .2, .8) )

## Impute missing data
ini <- mice( zinbmdata, m = 5, maxit = 0)
pred <- ini$predictorMatrix 
pred[1,] <- c(0, 2, 2, 3)
meth<-ini$method
meth[1] <- "2l.zinb"
imp.zinb <- mice( zinbmdata, m = 5, maxit = 1, method = meth,
predictorMatrix = pred, seed = 1234, print = FALSE)

## Do repeated data analysis and print pooled results
res.zinb <- with( imp.zinb, 
zeroinfl( yzinb ~ x1 + x2 | x3, dist = "negbin", link = "logit" ) )
summary( pool( res.zinb ) )

## Get estimate of pooled dispersion parameter (on log scale)
EST <- SE <- vector( length = imp.zinb$m, "list" )
for ( i in 1:imp.zinb$m ){
  EST[[i]] <- log( res.zinb$analyses[[i]]$theta ) 
  SE[[i]] <- res.zinb$analyses[[i]]$SE.logtheta 
}
print( data.frame( miinference( EST, SE ) ) )
}