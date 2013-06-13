\name{miinference}
\alias{miinference}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multiple Imputation Inference
}
\description{
Combines \code{m} parmater estimates into an overall estimate using Rubin's (1987) rules for multiple imputation inference for a scalar estimand.}
\usage{
miinference(est, std.err, confidence=0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{est}{
a list of \code{m} vectors containing parameter estimates (e.g. regression coefficients)
}
  \item{std.err}{
a list of \code{m} vectors with corresponding standard errors
}
  \item{confidence}{
level of MI confidence interval
}
}

\details{
}
\value{
a list of vectors of \code{length(est[[1]])}

\item{est}{combined parameter estimates}

\item{std.err}{combined standard errors representing both between and  within-imputation variability}

\item{t.value}{t-ratio: \code{t.value}=\code{est}/\code{std.err}}

\item{df}{degrees of freedom}	
 
\item{signif}{p-values for the two-tailed tests that the respective combined estimate is zero}

\item{lower}{lower limits of the MI confidence intervals}

\item{upper}{upper limits of the MI confidence intervals}

\item{r}{relative increases in variance due to nonresponse}	

\item{fminf}{fractions of missing information}	

}
\references{
Rubin, D. B. (1987). \emph{Multiple imputation for nonresponse in surveys}. New York: Wiley.

Schafer, J. L. (1997). \emph{Analysis of incomplete multivariate data}. London: Chapman & Hall.
}

\author{
Kristian Kleinke and Jost Reinecke
}\note{
adaptation of the function \code{mi.inference()} from package \pkg{norm}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## simulate overdespersed count data
b0<-1
b1<-.75
b2<--.25
b3<-.5
N<-5000
x1<-rnorm(N)
x2<-rnorm(N)
x3<-rnorm(N)
mu<-exp(b0+b1*x1+b2*x2+b3*x3)
y<-rnegbin(N,theta=2,mu)
NB<-data.frame(y,x1,x2,x3)

## introduce MAR missingness to simulated data
total<-round(.2*N)  ##number of missing data in y     
sm<-which(NB[,2]<mean(NB[,2]))  ##subset: cases with x2<mean(x2)
gr<-which(NB[,2]>mean(NB[,2]))	##subset: cases with x2>mean(x2)
sel.sm<-sample(sm,round(.2*total))	##select cases to set as missing
sel.gr<-sample(gr,round(.8*total))	##select cases to set as missing
sel<-c(sel.sm,sel.gr)
MNB<-NB
MNB[sel,1]<-NA	##delete selected data

require(mice)
imp1<-mice(MNB,method=c("qpois","","",""))
imp2<-mice(MNB,method=c("nb","","",""))
res1<-with(imp1,glm.nb(y~x1+x2+x3))
res2<-with(imp2,glm.nb(y~x1+x2+x3))
print(pool.res1<-summary(pool(res1)))
print(pool.res2<-summary(pool(res2)))

## get MI inferences for dispersion parameter theta

EST1<-EST2<-vector(length=5,"list")
SE1<-SE2<-vector(length=5,"list")

for (i in 1:5){
  EST1[[i]]<-res1$analyses[[i]]$theta 
  SE1[[i]]<-res1$analyses[[i]]$SE.theta 
  EST2[[i]]<-res2$analyses[[i]]$theta 
  SE2[[i]]<-res2$analyses[[i]]$SE.theta 
  
}

miinference(EST1,SE1)
miinference(EST2,SE2)
}