#' Multiple imputation pooling: univariate version
#'
#' Pools univariate estimates of m repeated complete data analysis
#'
#' The function averages the univariate estimates of the complete data model,
#' computes the total variance over the repeated analyses, and computes the
#' relative increase in variance due to nonresponse and the fraction of missing
#' information.
#'
#' @param Q A vector of univariate estimates of \code{m} repeated complete data
#' analyses.
#' @param U A vector containing the corresponding \code{m} variances of the univariate
#' estimates.
#' @param n A number providing the sample size. If nothing is specified,
#' an infinite sample \code{n = Inf} is assumed.
#' @param k A number indicating the number of parameters to be estimated.
#' By default, \code{k = 1} is assumed.
#' @return Returns a list with components.
#'   \describe{
#'     \item{\code{m}:}{Number of imputations.}
#'     \item{\code{qhat}:}{The \code{m} univariate estimates of repeated complete-data analyses.}
#'     \item{\code{u}:}{The corresponding \code{m} variances of the univariate estimates.}
#'     \item{\code{qbar}:}{The pooled univariate estimate, formula (3.1.2) Rubin (1987).}
#'     \item{\code{ubar}:}{The mean of the variances (i.e. the pooled within-imputation variance),
#'     formula (3.1.3) Rubin (1987).}
#'     \item{\code{b}:}{The between-imputation variance, formula (3.1.4) Rubin (1987).}
#'     \item{\code{t}:}{The total variance of the pooled estimated, formula (3.1.5)
#'     Rubin (1987).}
#'     \item{\code{r}:}{The relative increase in variance due to nonresponse, formula
#'     (3.1.7) Rubin (1987).}
#'     \item{\code{df}:}{The degrees of freedom for t reference distribution by the
#'     method of Barnard-Rubin (1999).}
#'     \item{\code{fmi}:}{The fraction missing information due to nonresponse,
#'     formula (3.1.10) Rubin (1987).}
#'     }
#' @author Karin Groothuis-Oudshoorn and Stef van Buuren, 2009
#' @seealso \code{\link{pool}}
#' @references Rubin, D.B. (1987). Multiple Imputation for Nonresponse in
#' Surveys.  New York: John Wiley and Sons.
#' @examples
#' # example with manual and automatic pooling
#' imp <- mice(nhanes, maxit = 2, m = 2, print = FALSE, seed = 18210)
#' fit <- with(data = imp, lm(bmi ~ age))
#'
#' # manual pooling
#' summary(fit$analyses[[1]])
#' summary(fit$analyses[[2]])
#' pool.scalar(Q = c(-1.5457, -1.428), U = c(0.9723^2, 1.041^2), n = 25, k = 2)
#'
#' # automatic pooling using broom
#' pool(fit)
#' @export
pool.scalar <- function(Q, U, n = Inf, k = 1) {
  m <- length(Q)
  qbar <- mean(Q)
  ubar <- mean(U)
  b <- var(Q)
  t <- ubar + (m + 1) * b / m
  df <- barnard.rubin(m, b, t, dfcom = n - k)
  r <- (1 + 1 / m) * b / ubar
  fmi <- (r + 2 / (df + 3)) / (r + 1)

  list(m = m, qhat = Q, u = U, qbar = qbar, ubar = ubar, b = b, t = t, df = df, r = r, fmi = fmi)
}
