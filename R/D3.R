#' Compare two nested models using D3-statistic
#'
#' The D3-statistic is a likelihood-ratio test statistic.
#'
#' @details
#' The \code{D3()} function implements the likelihood-ratio method of
#' Meng and Rubin (1992). For each of the \code{m} imputed datasets, the
#' function calculates the deviance between the full and the null model
#' as fitted, and the deviance between the two models with all parameters
#' fixed at their estimates pooled over the imputations. The pooled
#' parameters include the nuisance parameters: the residual variance in
#' \code{lm} and gaussian \code{glm}, and the variance components and
#' residual variance in \code{lmer} models. Models fitted by REML are
#' refitted by ML.
#'
#' The log-likelihood at fixed parameters is evaluated exactly for
#' \code{lm}, for \code{glm} with the gaussian, binomial and poisson
#' families, and for \code{lmerMod}. For these classes \code{D3()} gives
#' the same result as \code{mitml::testModels(method = "D3")}. For other
#' classes, \code{D3()} constrains the regression coefficients by means of
#' \code{\link{fix.coef}} and re-estimates the remaining parameters in
#' each imputed dataset, which approximates the method.
#'
#' A negative \code{D3} statistic signals that the models constrained to
#' the pooled coefficients fit much worse than the models as fitted. This
#' typically happens when the coefficients are unstable across imputations,
#' for example under separation in logistic regression with small samples.
#' \code{D3()} warns in that case, and the result should not be trusted.
#'
#' @seealso \code{\link{fix.coef}}
#' @inheritParams D1
#' @return An object of class \code{mice.anova}
#' @references
#' Meng, X. L., and D. B. Rubin. 1992.
#' Performing Likelihood Ratio Tests with Multiply-Imputed Data Sets.
#' \emph{Biometrika}, 79 (1): 103–11.
#'
#' \url{https://stefvanbuuren.name/fimd/sec-multiparameter.html#sec:likelihoodratio}
#'
#' \url{http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#setting-residual-variances-to-a-fixed-value-zero-or-other}
#' @examples
#' # Compare two linear models:
#' imp <- mice(nhanes2, seed = 51009, print = FALSE)
#' mi1 <- with(data = imp, expr = lm(bmi ~ age + hyp + chl))
#' mi0 <- with(data = imp, expr = lm(bmi ~ age + hyp))
#' D3(mi1, mi0)
#' \dontrun{
#' # Compare two logistic regression models
#' imp <- mice(boys, maxit = 2, print = FALSE)
#' fit1 <- with(imp, glm(gen > levels(gen)[1] ~ hgt + hc + reg, family = binomial))
#' fit0 <- with(imp, glm(gen > levels(gen)[1] ~ hgt + hc, family = binomial))
#' D3(fit1, fit0)
#' }
#' @export
D3 <- function(fit1, fit0 = NULL, dfcom = NULL, df.com = NULL) {
  if (!missing(df.com)) {
    warning(
      "argument df.com is deprecated; please use dfcom instead.",
      call. = FALSE
    )
    dfcom <- df.com
  }

  model <- getfit(fit1, 1L)
  dfcom <- get.dfcom(model, dfcom)

  call <- match.call()
  fit1 <- getfit(fit1)
  m <- length(fit1)
  est1 <- pool(fit1, dfcom = dfcom)
  qbar1 <- getqbar(est1)

  if (is.null(fit0)) {
    # test all estimates equal to zero
    beta <- rep(0, length(qbar1))
    names(beta) <- names(qbar1)
    fit0 <- lapply(fit1, fix.coef, beta = beta)
  } else {
    fit0 <- getfit(fit0)
  }

  est0 <- pool(fit0, dfcom = dfcom)
  qbar0 <- getqbar(est0)
  k <- length(qbar1) - length(qbar0)

  # refit REML fits with ML, since D3 needs the likelihood
  reml <- vapply(
    c(fit1, fit0),
    function(f) inherits(f, "merMod") && lme4::isREML(f),
    logical(1)
  )
  if (any(reml)) {
    message("D3: refitting REML models with ML")
    fit1 <- lapply(fit1, d3.ml)
    fit0 <- lapply(fit0, d3.ml)
  }

  # For each imputed dataset, the deviance of the two models as fitted
  dev1.M <- -2 * vapply(fit1, function(f) as.numeric(logLik(f)), numeric(1))
  dev0.M <- -2 * vapply(fit0, function(f) as.numeric(logLik(f)), numeric(1))

  # For each imputed dataset, the deviance of the two models with all
  # parameters, including the nuisance parameters, fixed at their pooled
  # values (Meng and Rubin, 1992)
  dev1.L <- -2 * d3.restricted(fit1)
  dev0.L <- -2 * d3.restricted(fit0)

  deviances <- list(
    dev1.M = dev1.M,
    dev0.M = dev0.M,
    dev1.L = dev1.L,
    dev0.L = dev0.L
  )

  # scaled deviance, as fitted
  dev.M <- mean(dev0.M - dev1.M)
  # scaled deviance, restricted
  dev.L <- mean(dev0.L - dev1.L)
  rm <- ((m + 1) / (k * (m - 1))) * (dev.M - dev.L)
  Dm <- dev.L / (k * (1 + rm))

  # Degrees of freedom for F distribution
  v <- k * (m - 1)
  if (v > 4) {
    w <- 4 + (v - 4) * ((1 + (1 - 2 / v) * (1 / rm))^2)
  } else {
    w <- v * (1 + 1 / k) * ((1 + 1 / rm)^2) / 2
  }
  pvalue <- pf(Dm, k, w, lower.tail = FALSE)
  if (is.finite(Dm) && Dm < 0) {
    warning(
      "D3 statistic is negative (", format(Dm, digits = 3), "). ",
      "The coefficients are unstable across imputations, ",
      "e.g. because of separation. Do not trust this result.",
      call. = FALSE
    )
  }

  test <-
    out <- list(
      call = match.call(),
      result = c(Dm, k, w, pvalue, rm),
      formulas = list(
        `1` = formula(getfit(fit1, 1L)),
        `2` = formula(getfit(fit0, 1L))
      ),
      m = m,
      method = "D3",
      use = NULL,
      dfcom = dfcom,
      deviances = deviances
    )
  class(out) <- c("mice.anova", class(fit1))
  out
}
