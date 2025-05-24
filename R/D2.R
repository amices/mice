#' Compare two nested models using D2-statistic
#'
#' The D2-statistic pools test statistics from the repeated analyses.
#' The method is less powerful than the D1- and D3-statistics.
#'
#' @inheritParams D1
#' @inheritParams mitml::testModels
#' @note Warning: `D2()` assumes that the order of the variables is the
#' same in different models. See
#' \url{https://github.com/amices/mice/issues/420} for details.
#' @references
#' Li, K. H., X. L. Meng, T. E. Raghunathan, and D. B. Rubin. 1991.
#' Significance Levels from Repeated p-Values with Multiply-Imputed Data.
#' \emph{Statistica Sinica} 1 (1): 65–92.
#'
#' \url{https://stefvanbuuren.name/fimd/sec-multiparameter.html#sec:chi}
#' @examples
#' # Compare two linear models:
#' imp <- mice(nhanes2, seed = 51009, print = FALSE)
#' mi1 <- with(data = imp, expr = lm(bmi ~ age + hyp + chl))
#' mi0 <- with(data = imp, expr = lm(bmi ~ age + hyp))
#' D2(mi1, mi0)
#' \dontrun{
#' # Compare two logistic regression models
#' imp <- mice(boys, maxit = 2, print = FALSE)
#' fit1 <- with(imp, glm(gen > levels(gen)[1] ~ hgt + hc + reg, family = binomial))
#' fit0 <- with(imp, glm(gen > levels(gen)[1] ~ hgt + hc, family = binomial))
#' D2(fit1, fit0)
#' }
#' @seealso \code{\link[mitml]{testModels}}
#' @export
D2 <- function(fit1, fit0 = NULL, use = "wald") {
  install.on.demand("mitml")

  # fit1: a fitlist or mira-object
  # fit0: named numerical vector, character vector, or list
  fit1 <- as.mitml.result(fit1)
  est1 <- pool(fit1)
  qbar1 <- getqbar(est1)

  if (is.null(fit0)) {
    # test all estimates equal to zero
    beta <- rep(0, length(qbar1))
    names(beta) <- names(qbar1)
    fit0 <- lapply(fit1, fix.coef, beta = beta)
    fit0 <- as.mitml.result(fit0)
  } else if (is.mira(fit0)) {
    fit0 <- as.mitml.result(fit0)
  }

  tmr <- mitml::testModels(fit1, fit0, method = "D2", use = use)

  out <- list(
    call = match.call(),
    result = tmr$test,
    formulas = list(
      `1` = formula(fit1[[1L]]),
      `2` = formula(fit0[[1L]])
    ),
    m = tmr$m,
    method = "D2",
    use = use,
    dfcom = NA
  )
  class(out) <- c("mice.anova", class(fit1))
  out
}
