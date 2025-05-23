% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/D2.R
\name{D2}
\alias{D2}
\title{Compare two nested models using D2-statistic}
\usage{
D2(fit1, fit0 = NULL, use = "wald")
}
\arguments{
\item{fit1}{An object of class \code{mira}, produced by \code{with()}.}

\item{fit0}{An object of class \code{mira}, produced by \code{with()}. The
model in \code{fit0} is a nested within \code{fit1}. The default null
model \code{fit0 = NULL} compares \code{fit1} to the intercept-only model.}

\item{use}{A character string denoting Wald- or likelihood-based based tests. Can be either \code{"wald"} or \code{"likelihood"}. Only used if \code{method = "D2"}.}
}
\description{
The D2-statistic pools test statistics from the repeated analyses.
The method is less powerful than the D1- and D3-statistics.
}
\note{
Warning: \code{D2()} assumes that the order of the variables is the
same in different models. See
\url{https://github.com/amices/mice/issues/420} for details.
}
\examples{
# Compare two linear models:
imp <- mice(nhanes2, seed = 51009, print = FALSE)
mi1 <- with(data = imp, expr = lm(bmi ~ age + hyp + chl))
mi0 <- with(data = imp, expr = lm(bmi ~ age + hyp))
D2(mi1, mi0)
\dontrun{
# Compare two logistic regression models
imp <- mice(boys, maxit = 2, print = FALSE)
fit1 <- with(imp, glm(gen > levels(gen)[1] ~ hgt + hc + reg, family = binomial))
fit0 <- with(imp, glm(gen > levels(gen)[1] ~ hgt + hc, family = binomial))
D2(fit1, fit0)
}
}
\references{
Li, K. H., X. L. Meng, T. E. Raghunathan, and D. B. Rubin. 1991.
Significance Levels from Repeated p-Values with Multiply-Imputed Data.
\emph{Statistica Sinica} 1 (1): 65–92.

\url{https://stefvanbuuren.name/fimd/sec-multiparameter.html#sec:chi}
}
\seealso{
\code{\link[mitml]{testModels}}
}
