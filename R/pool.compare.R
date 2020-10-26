#' Compare two nested models fitted to imputed data
#'
#' This function is deprecated in V3. Use \code{\link{D1}} or
#' \code{\link{D3}} instead.
#'
#' Compares two nested models after m repeated complete data analysis
#'
#' The function is based on the article of Meng and Rubin (1992). The
#' Wald-method can be found in paragraph 2.2 and the likelihood method can be
#' found in paragraph 3.  One could use the Wald method for comparison of linear
#' models obtained with e.g. \code{lm} (in \code{with.mids()}).  The likelihood
#' method should be used in case of logistic regression models obtained with
#' \code{glm()} in \code{with.mids()}.
#'
#' The function assumes that \code{fit1} is the
#' larger model, and that model \code{fit0} is fully contained in \code{fit1}.
#' In case of \code{method='wald'}, the null hypothesis is tested that the extra
#' parameters are all zero.
#'
#' @param fit1 An object of class 'mira', produced by \code{with.mids()}.
#' @param fit0 An object of class 'mira', produced by \code{with.mids()}. The
#' model in \code{fit0} is a nested fit0 of \code{fit1}.
#' @param method Either \code{"wald"} or \code{"likelihood"} specifying
#' the type of comparison. The default is \code{"wald"}.
#' @param data No longer used.
#' @return A list containing several components. Component \code{call} is
#' the call to the \code{pool.compare} function. Component \code{call11} is
#' the call that created \code{fit1}. Component \code{call12} is the
#' call that created the imputations. Component \code{call01} is the
#' call that created \code{fit0}. Component \code{call02} is the
#' call that created the imputations. Components \code{method} is the
#' method used to compare two models: 'Wald' or 'likelihood'. Component
#' \code{nmis} is the number of missing entries for each variable.
#' Component \code{m} is the number of imputations.
#' Component \code{qhat1} is a matrix, containing the estimated coefficients of the
#' \emph{m} repeated complete data analyses from \code{fit1}.
#' Component \code{qhat0} is a matrix, containing the estimated coefficients of the
#' \emph{m} repeated complete data analyses from \code{fit0}.
#' Component \code{ubar1} is the mean of the variances of \code{fit1},
#' formula (3.1.3), Rubin (1987).
#' Component \code{ubar0} is the mean of the variances of \code{fit0},
#' formula (3.1.3), Rubin (1987).
#' Component \code{qbar1} is the pooled estimate of \code{fit1}, formula (3.1.2) Rubin
#' (1987).
#' Component \code{qbar0} is the pooled estimate of \code{fit0}, formula (3.1.2) Rubin
#' (1987).
#' Component \code{Dm} is the test statistic.
#' Component \code{rm} is the relative increase in variance due to nonresponse, formula
#' (3.1.7), Rubin (1987).
#' Component \code{df1}: df1 = under the null hypothesis it is assumed that \code{Dm} has an F
#' distribution with (df1,df2) degrees of freedom.
#' Component \code{df2}: df2.
#' Component \code{pvalue} is the P-value of testing whether the model \code{fit1} is
#' statistically different from the smaller \code{fit0}.
#' @author Karin Groothuis-Oudshoorn and Stef van Buuren, 2009
#' @seealso \code{\link{lm.mids}}, \code{\link{glm.mids}}
#' @references Li, K.H., Meng, X.L., Raghunathan, T.E. and Rubin, D. B. (1991).
#' Significance levels from repeated p-values with multiply-imputed data.
#' Statistica Sinica, 1, 65-92.
#'
#' Meng, X.L. and Rubin, D.B. (1992). Performing likelihood ratio tests with
#' multiple-imputed data sets.  Biometrika, 79, 103-111.
#'
#' van Buuren S and Groothuis-Oudshoorn K (2011). \code{mice}: Multivariate
#' Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical
#' Software}, \bold{45}(3), 1-67. \url{https://www.jstatsoft.org/v45/i03/}
#' @keywords htest
#' @export
pool.compare <- function(fit1, fit0, method = c("wald", "likelihood"),
                         data = NULL) {
  .Deprecated("D1")

  # Check the arguments
  call <- match.call()
  method <- match.arg(method)

  fits1 <- getfit(fit1)
  fits0 <- getfit(fit0)

  if (length(fits1) != length(fits0)) {
    stop("unequal number of imputations for 'fit1' and 'fit0'", call. = FALSE)
  }
  if (length(fits1) < 2L) {
    stop("at least two imputations are needed", call. = FALSE)
  }

  m <- length(fits1)
  est1 <- pool(fit1)
  est0 <- pool(fit0)
  dimQ1 <- length(getqbar(est1))
  dimQ2 <- dimQ1 - length(getqbar(est0))
  # Check: Only need the lm or lmer object
  formula1 <- formula(getfit(fit1, 1L))
  formula0 <- formula(getfit(fit0, 1L))
  vars1 <- est1$pooled$term
  vars0 <- est0$pooled$term

  if (is.null(vars1) || is.null(vars0)) {
    stop("coefficients do not have names", call. = FALSE)
  }
  if (dimQ2 < 1L) {
    stop("Model 'fit1' not larger than 'fit0'", call. = FALSE)
  }
  if (!setequal(vars0, intersect(vars0, vars1))) {
    stop("Model 'fit0' not contained in 'fit1'", call. = FALSE)
  }

  if (method == "wald") {
    # Reference: paragraph 2.2, Article Meng & Rubin,
    # Biometrika, 1992.  When two objects are to be compared
    # we need to calculate matrix Q
    Q <- diag(dimQ1)
    where_new_vars <- which(!(vars1 %in% vars0))
    Q <- Q[where_new_vars, , drop = FALSE]
    qbar <- Q %*% getqbar(est1)
    Ubar <- Q %*% diag(est1$pooled$ubar) %*% (t(Q))
    Bm <- Q %*% diag(est1$pooled$b) %*% (t(Q))
    rm <- (1 + 1 / m) * sum(diag(Bm %*% (solve(Ubar)))) / dimQ2
    Dm <- (t(qbar)) %*% (solve(Ubar)) %*% qbar / (dimQ2 * (1 + rm))
    deviances <- NULL
  }

  if (method == "likelihood") {
    # Calculate for each imputed dataset the deviance between the two
    # models with its estimated coefficients
    dev1.M <- lapply(fits1, glance) %>%
      bind_rows() %>%
      pull(.data$deviance)
    dev0.M <- lapply(fits0, glance) %>%
      bind_rows() %>%
      pull(.data$deviance)

    # Calculate for each imputed dataset the deviance between the two
    # models with the pooled coefficients
    qbar1 <- getqbar(pool(fits1))
    mds1 <- lapply(fits1, fix.coef, beta = qbar1)
    dev1.L <- lapply(mds1, glance) %>%
      bind_rows() %>%
      pull(.data$deviance)

    qbar0 <- getqbar(pool(fits0))
    mds0 <- lapply(fits0, fix.coef, beta = qbar0)
    dev0.L <- lapply(mds0, glance) %>%
      bind_rows() %>%
      pull(.data$deviance)

    deviances <- list(
      dev1.M = dev1.M, dev0.M = dev0.M,
      dev1.L = dev1.L, dev0.L = dev0.L
    )

    dev.M <- mean(dev0.M - dev1.M)
    dev.L <- mean(dev0.L - dev1.L)
    rm <- ((m + 1) / (dimQ2 * (m - 1))) * (dev.M - dev.L)
    Dm <- dev.L / (dimQ2 * (1 + rm))
  }

  # Degrees of freedom for F distribution, same for both methods
  v <- dimQ2 * (m - 1)
  if (v > 4) {
    # according to Li 1991
    w <- 4 + (v - 4) * ((1 + (1 - 2 / v) * (1 / rm))^2)
  } else {
    w <- v * (1 + 1 / dimQ2) * ((1 + 1 / rm)^2) / 2
  }

  statistic <- list(
    call = call, call11 = fit1$call, call12 = fit1$call1,
    call01 = fit0$call, call02 = fit0$call1,
    method = method, nmis = fit1$nmis, m = m,
    qbar1 = getqbar(est1), qbar0 = getqbar(est0),
    ubar1 = est1$pooled$ubar, ubar0 = est0$pooled$ubar,
    deviances = deviances,
    Dm = Dm, rm = rm, df1 = dimQ2, df2 = w,
    pvalue = 1 - pf(Dm, dimQ2, w)
  )
  statistic
}
