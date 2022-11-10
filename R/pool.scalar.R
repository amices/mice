#' Multiple imputation pooling: univariate version
#'
#' Pools univariate estimates of m repeated complete data analysis
#'
#' The function averages the univariate estimates of the complete data model,
#' computes the total variance over the repeated analyses, and computes the
#' relative increase in variance due to missing data or data synthesisation
#' and the fraction of missing information.
#'
#' @param Q A vector of univariate estimates of \code{m} repeated complete data
#' analyses.
#' @param U A vector containing the corresponding \code{m} variances of the univariate
#' estimates.
#' @param n A number providing the sample size. If nothing is specified,
#' an infinite sample \code{n = Inf} is assumed.
#' @param k A number indicating the number of parameters to be estimated.
#' By default, \code{k = 1} is assumed.
#' @inheritParams pool
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
#'     formula (3.1.10) Rubin (1987). (Not defined for synthetic data.)}
#'     }
#' @author Karin Groothuis-Oudshoorn and Stef van Buuren, 2009; Thom Volker, 2021
#' @seealso \code{\link{pool}}
#' @references
#' Rubin, D.B. (1987). Multiple Imputation for Nonresponse in
#' Surveys.  New York: John Wiley and Sons.
#'
#' Reiter, J.P. (2003). Inference for Partially Synthetic,
#' Public Use Microdata Sets. \emph{Survey Methodology}, \bold{29}, 181-189.
#' @examples
#' # missing data imputation with with manual pooling
#' imp <- mice(nhanes, maxit = 2, m = 2, print = FALSE, seed = 18210)
#' fit <- with(data = imp, lm(bmi ~ age))
#'
#' # manual pooling
#' summary(fit$analyses[[1]])
#' summary(fit$analyses[[2]])
#' pool.scalar(Q = c(-1.5457, -1.428), U = c(0.9723^2, 1.041^2), n = 25, k = 2)
#'
#' # check: automatic pooling using broom
#' pool(fit)
#'
#' # manual pooling for synthetic data created from complete data
#' imp <- mice(cars, maxit = 2, m = 2, print = FALSE, seed = 18210,
#'             where = matrix(TRUE, nrow(cars), ncol(cars)))
#' fit <- with(data = imp, lm(speed ~ dist))
#'
#' # manual pooling: extract Q and U
#' summary(fit$analyses[[1]])
#' summary(fit$analyses[[2]])
#' pool.scalar.syn(Q = c(0.12182, 0.13209), U = c(0.02121^2, 0.02516^2), n = 50, k = 2)
#'
#' # check: automatic pooling using broom
#' pool.syn(fit)
#' @export
pool.scalar <- function(Q, U, n = Inf, k = 1, rule = c("rubin1987", "reiter2003")) {
  rule <- match.arg(rule)

  m <- length(Q)
  qbar <- mean(Q)
  ubar <- mean(U)
  b <- var(Q)
  r <- (1 + 1 / m) * b / ubar

  if (rule == "rubin1987") {
    t <- ubar + (m + 1) * b / m
    df <- barnard.rubin(m, b, t, dfcom = n - k)
    fmi <- (r + 2 / (df + 3)) / (r + 1)
  }

  if (rule == "reiter2003") {
    t <- ubar + b / m
    df <- (m - 1) * (1 + (ubar / (b / m)))^2
    fmi <- NA_real_
  }

  list(m = m, qhat = Q, u = U, qbar = qbar, ubar = ubar, b = b, t = t,
       df = df, r = r, fmi = fmi)
}

#' @rdname pool.scalar
#' @export
pool.scalar.syn <- function(Q, U, n = Inf, k = 1, rule = "reiter2003") {
  pool.scalar(Q, U, n = Inf, k = 1, rule = rule)
}
