% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcar.R
\name{mcar}
\alias{mcar}
\title{Jamshidian and Jalal's Non-Parametric MCAR Test}
\usage{
mcar(
  x,
  imputed = mice(x, method = "norm"),
  min_n = 6,
  method = "auto",
  replications = 10000,
  use_chisq = 30,
  alpha = 0.05
)
}
\arguments{
\item{x}{An object for which a method exists; usually a \code{data.frame}.}

\item{imputed}{Either an object of class \code{mids}, as returned by
\code{\link[=mice]{mice()}}, or a list of \code{data.frame}s.}

\item{min_n}{Atomic numeric, must be greater than 1. When there are missing
data patterns with fewer than \code{min_n} cases, all cases with that pattern will
be removed from \code{x} and \code{imputed}.}

\item{method}{Atomic character. If it is known (or assumed) that data are
either multivariate normally distributed or not, then use either
\code{method = "hawkins"} or \code{method = "nonparametric"}, respectively.
The default argument \code{method = "auto"} follows the procedure outlined in the
Details section, and in Figure 7 of Jamshidian and Jalal (2010).}

\item{replications}{Number of replications used to simulate the Neyman
distribution when performing Hawkins' test. As this method is based on random
sampling, use a high number of \code{replications} (and optionally,
\code{\link[=set.seed]{set.seed()}}) to minimize Monte Carlo error and ensure reproducibility.}

\item{use_chisq}{Atomic integer, indicating the minimum number of cases
within a group \emph{k} that triggers the use of asymptotic Chi-square
distribution instead of the emprical distribution in the Neyman uniformity
test, which is performed as part of Hawkins' test.}

\item{alpha}{Atomic numeric, indicating the significance level of tests.}
}
\value{
An object of class \code{mcar_object}.
}
\description{
Test whether missingness is contingent upon the observed variables,
according to the methodology developed by Jamshidian and Jalal (2010) (see
Details).
}
\details{
Three types of missingness have been distinguished in the literature
(Rubin, 1976):
Missing completely at random (MCAR), which means that missingness is random;
missing at random (MAR), which means that missingness is contingent on the
\emph{observed};
and missing not at random (MNAR), which means that missingness is related to
unobserved data.

Jamshidian and Jalal's non-parametric MCAR test assumes that the missing data
are either MCAR or MAR, and tests whether the missingness is independent of
the observed values. If so, the covariance matrices of the imputed data will
be equal accross groups with different patterns of missingness. This test
consists of the following procedure:
\enumerate{
\item Data are imputed.
\item The imputed data are split into \emph{k} groups according to the
\emph{k} missing data patterns in the original data (see
\code{\link[=md.pattern]{md.pattern()}}).
\item Perform Hawkins' test for equality of covariances across the \emph{k}
groups.
\item If the test is \emph{not significant}, conclude that there is no evidence
against multivariate normality of the data, nor against MCAR.
\item If the test \emph{is significant}, and multivariate normality of the data
can be assumed, then it can be concluded that missingness is MAR.
\item If multivariate normality cannot be assumed, then perform the
Anderson-Darling non-parametric test for equality of covariances across the
\emph{k} groups.
\item If the Anderson-Darling test is \emph{not significant}, this is evidence
against multivariate normality - but no evidence against MCAR.
\item If the Anderson-Darling test \emph{is significant}, this is evidence
it can be concluded that missingness is MAR.
}

Note that, despite its name in common parlance, an MCAR test can only
indicate whether missingness is MCAR or MAR. The procedure cannot distinguish
MCAR from MNAR, so a non-significant result does not rule out MNAR.

This is a re-implementation of the function \code{TestMCARNormality}, which was
originally published in the R-packgage \code{MissMech}, which has been removed
from CRAN. This new implementation is faster, as its backend is written in
C++. It also enhances the functionality of the original:
\itemize{
\item Multiply imputed data can now be used; the median p-value and test
statistic across replications is then reported, as suggested by
Eekhout, Wiel, and Heymans (2017).
\item The printing method for an \code{mcar_object} gives a warning when at
least one p-value of either test was significant. In this case, it is
recommended to inspect the range of p-values, and consider potential
violations of MCAR.
\item A plotting method for an \code{mcar_object} is provided.
\item A plotting method for the \verb{$md.pattern} element of an \code{mcar_object}
is provided.
}
}
\examples{
res <- mcar(nhanes)
# Examine test results
res
# Plot p-values across imputed data sets
plot(res)
# Plot md patterns used for the test
plot(res, type = "md.pattern")
# Note difference with the raw md.patterns:
md.pattern(nhanes)
}
\references{
Rubin, D. B. (1976). Inference and Missing Data. Biometrika, Vol. 63, No. 3,
pp. 581-592. \doi{10.2307/2335739}

Eekhout, I., M. A. Wiel, & M. W. Heymans (2017). Methods for Significance
Testing of Categorical Covariates in Logistic Regression Models After
Multiple Imputation: Power and Applicability Analysis. BMC Medical Research
Methodology 17 (1): 129.

Jamshidian, M., & Jalal, S. (2010). Tests of homoscedasticity, normality, and
missing completely at random for incomplete multivariate data. Psychometrika,
75(4), 649–674. \doi{10.1007/s11336-010-9175-3}
}
\author{
Caspar J. Van Lissa
}
\keyword{internal}
