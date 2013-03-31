\name{mice.internal}

\alias{is.passive}
\alias{.norm.draw}
\alias{.norm.fix}
\alias{padModel}
\alias{.pmm.match}
\alias{sampler}
\alias{squeeze}
\alias{augment}
\alias{check.df}
\alias{remove.lindep}
\alias{find.collinear}
\alias{updateLog}
\alias{expandvcov}
\alias{mice.df}
\alias{df.residual.mira}
\alias{df.residual.lme}
\alias{df.residual.mer}
\alias{df.residual.default}
\alias{rm.whitespace}
\alias{ridge}
\alias{donors}

\title{Internal mice functions}

\description{
Internal functions for package \code{mice}.
}

\usage{
is.passive(string)
.norm.draw(y, ry, x, ridge = 0.00001, ...)
.norm.fix(y, ry, x, ridge = 0.00001, ...)
padModel(data, method, predictorMatrix, visitSequence, post, 
 nmis, nvar)
.pmm.match(z, yhat = yhat, y = y, donors=3, ...)
sampler(p, data, m, imp, r, visitSequence, fromto, printFlag,...)
squeeze(x, bounds = c(min(x[r]), max(x[r])), r = rep(TRUE, 
 length(x)), ...)
augment(y, ry, x, maxcat=50, ...)
check.df(x, y, ry, ...)
remove.lindep(x, y, ry, eps = 0.0001, maxcor = 0.99, allow.na=FALSE,...) 
find.collinear(x, threshold=0.999, ...)
updateLog(out=NULL, meth=NULL, frame=2)
expandvcov(q, u)
mice.df(m, lambda, dfcom, method)
df.residual.mira(object, ...)
df.residual.lme(object, ...)
df.residual.mer(object, ...)
df.residual.default(object, q=1.3, ...)
rm.whitespace(string, side = "both")
}

\arguments{
    \item{method}{}
    \item{visitSequence}{}
    \item{data}{}
    \item{nmis}{}
    \item{nvar}{}
    \item{donors}{The number of potential donors used in predictive mean matching. The defaults is 3. Other popular values include 1, 5 and 10.}
    \item{predictorMatrix}{}
    \item{x}{}
    \item{y}{}
    \item{ry}{}
    \item{string}{}
    \item{ridge}{The ridge parameter to be used in \code{.norm.draw()}. Set \code{ridge=0.0001}, \code{ridge=0.001} or even \code{ridge=0.01} to solve singularity problems.}	
    \item{z}{}
    \item{yhat}{}
    \item{p}{}
    \item{m}{}
    \item{imp}{}
    \item{r}{}
    \item{post}{}
    \item{fromto}{}
    \item{printFlag}{}
    \item{\dots}{}
    \item{bounds}{}
    \item{nam}{}
    \item{eps}{}
    \item{maxcor}{}
    \item{allow.na}{}	
    \item{threshold}{}	
    \item{out}{}
    \item{frame}{}
    \item{meth}{}
    \item{gamma}{}
    \item{dfcom}{}
    \item{object}{}
    \item{q}{}
    \item{u}{}
    \item{string}{The string from which leading or trailing spaces should be removed.}
    \item{side}{Character vector: \code{"both"}, \code{"left"} or \code{"right"}.}
}

\author{Stef van Buuren, Karin Groothuis-Oudshoorn, Gerko Vink 2012} 

\keyword{internal}  
