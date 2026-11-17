#' Compare two nested models using D2-statistic
#'
#' The D2-statistic pools test statistics from the repeated analyses.
#' The method is less powerful than the D1- and D3-statistics.
#'
#' @details
#' If \code{fit1} holds the results of a test of class \code{htest}, as
#' produced by e.g. \code{with(imp, chisq.test(...))} or
#' \code{with(imp, fisher.test(...))}, \code{D2()} pools the test results
#' directly with the formulas of Li et al. (1991). There is no null model,
#' so \code{fit0} is ignored.
#' \itemize{
#'   \item For tests with a chi-squared statistic, such as
#'   \code{chisq.test()}, \code{mcnemar.test()}, \code{mantelhaen.test()},
#'   \code{kruskal.test()} and \code{friedman.test()}, \code{D2()} pools
#'   the statistics, with their degrees of freedom.
#'   \item For other tests, such as \code{fisher.test()}, \code{D2()}
#'   converts each p-value into a chi-squared statistic with \code{df}
#'   degrees of freedom, and pools these. The default \code{df = 1} fits a
#'   2 x 2 table. For an r x c table, set \code{df = (r - 1) * (c - 1)}.
#'   The conversion of the p-value of an exact test is an approximation.
#' }
#' @param df The degrees of freedom of the test, used only if \code{fit1}
#' holds \code{htest} results that have no chi-squared statistic, such as
#' \code{fisher.test()}. See Details.
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
#'
#' # Pool chi-squared and Fisher tests of a 2 x 2 table
#' data <- nhanes2
#' data$chl <- cut(data$chl, breaks = 2)
#' imp <- mice(data, seed = 1, print = FALSE)
#' D2(with(imp, chisq.test(xtabs(~ hyp + chl), correct = FALSE)))
#' D2(with(imp, fisher.test(xtabs(~ hyp + chl))))
#' @seealso \code{\link[mitml]{testModels}}
#' @export
D2 <- function(fit1, fit0 = NULL, use = "wald", df = 1) {
  # pool the results of statistical tests (#508)
  fits <- getfit(fit1)
  if (length(fits) && all(vapply(fits, inherits, logical(1), "htest"))) {
    return(D2.htest(fits, df = df, call = match.call()))
  }

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

# D2 for a list of htest objects
D2.htest <- function(fits, df = 1, call = NULL) {
  stat <- lapply(fits, function(x) x$statistic)
  chisq <- vapply(
    stat,
    function(x) {
      length(x) == 1L &&
        grepl("chi-squared|X-squared|K-squared", names(x), ignore.case = TRUE)
    },
    logical(1)
  )

  if (all(chisq)) {
    d <- vapply(stat, unname, numeric(1))
    k <- unique(vapply(fits, function(x) unname(x$parameter[1L]), numeric(1)))
    if (length(k) != 1L) {
      stop(
        "D2(): the degrees of freedom of the tests differ between ",
        "imputations: ",
        paste(k, collapse = ", "),
        call. = FALSE
      )
    }
    use <- "statistic"
  } else {
    p <- vapply(fits, function(x) as.numeric(x$p.value[1L]), numeric(1))
    if (anyNA(p)) {
      stop("D2(): not all tests have a p-value", call. = FALSE)
    }
    if (!is.numeric(df) || length(df) != 1L || is.na(df) || df <= 0) {
      stop("D2(): `df` must be one positive number", call. = FALSE)
    }
    k <- df
    d <- stats::qchisq(p, df = k, lower.tail = FALSE)
    use <- "p.value"
  }

  first <- fits[[1L]]
  out <- list(
    call = call,
    result = D2.chisq(d, k),
    formulas = list(
      `1` = paste0(first$method, ": ", first$data.name),
      `2` = "null hypothesis"
    ),
    m = length(d),
    method = "D2",
    use = use,
    dfcom = NA
  )
  class(out) <- c("mice.anova", "list")
  out
}

# Li, Meng, Raghunathan and Rubin (1991): pool m chi-squared statistics `d`
# with `k` degrees of freedom. Returns the F-statistic, df1, df2, p-value
# and the relative increase in variance.
D2.chisq <- function(d, k) {
  m <- length(d)
  if (m < 2L) {
    stop("D2(): needs at least two imputations", call. = FALSE)
  }
  r <- (1 + 1 / m) * stats::var(sqrt(d))
  stat <- max((mean(d) / k - (m + 1) / (m - 1) * r) / (1 + r), 0)
  df2 <- k^(-3 / m) * (m - 1) * (1 + 1 / r)^2
  p <- stats::pf(stat, k, df2, lower.tail = FALSE)
  matrix(
    c(stat, k, df2, p, r),
    nrow = 1L,
    dimnames = list(NULL, c("F.value", "df1", "df2", "P(>F)", "RIV"))
  )
}
