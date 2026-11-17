data <- nhanes2
data$chl <- cut(data$chl, breaks = 2)
imp <- mice(data, seed = 1, print = FALSE)

# reference: Li, Meng, Raghunathan and Rubin (1991)
ref.D2 <- function(d, k) {
  m <- length(d)
  r <- (1 + 1 / m) * var(sqrt(d))
  stat <- max((mean(d) / k - (m + 1) / (m - 1) * r) / (1 + r), 0)
  df2 <- k^(-3 / m) * (m - 1) * (1 + 1 / r)^2
  c(stat, k, df2, pf(stat, k, df2, lower.tail = FALSE), r)
}

test_that("D2-HTEST-001: pools chi-squared statistics (#508)", {
  fit <- suppressWarnings(
    with(imp, chisq.test(xtabs(~ hyp + chl), correct = FALSE))
  )
  d <- vapply(fit$analyses, function(x) unname(x$statistic), numeric(1))
  res <- D2(fit)
  expect_s3_class(res, "mice.anova")
  expect_equal(unname(res$result[1, ]), ref.D2(d, 1))
  expect_identical(res$use, "statistic")
  expect_output(print(res), "statistic")
})

test_that("D2-HTEST-002: pools Fisher p-values as chi-squared (#508)", {
  fit <- with(imp, fisher.test(xtabs(~ hyp + chl)))
  p <- vapply(fit$analyses, function(x) x$p.value, numeric(1))
  res <- D2(fit)
  expect_equal(
    unname(res$result[1, ]),
    ref.D2(qchisq(p, 1, lower.tail = FALSE), 1)
  )
  expect_identical(res$use, "p.value")

  res3 <- D2(fit, df = 3)
  expect_equal(
    unname(res3$result[1, ]),
    ref.D2(qchisq(p, 3, lower.tail = FALSE), 3)
  )
  expect_error(D2(fit, df = 0), "positive")
})

test_that("D2-HTEST-003: uses the df of chi-squared tests", {
  fit <- with(imp, kruskal.test(bmi ~ age))
  res <- D2(fit)
  expect_identical(res$result[1, "df1"], c(df1 = 2))
})

test_that("D2-HTEST-004: pool() points to D2() for htest results", {
  fit <- with(imp, fisher.test(xtabs(~ hyp + chl)))
  expect_error(pool(fit), "Use D2\\(\\)")
})

test_that("D2-HTEST-005: model comparisons are unchanged", {
  imp2 <- mice(nhanes2, seed = 51009, print = FALSE)
  mi1 <- with(imp2, lm(bmi ~ age + hyp + chl))
  mi0 <- with(imp2, lm(bmi ~ age + hyp))
  res <- D2(mi1, mi0)
  expect_identical(res$use, "wald")
  expect_identical(res$result[1, "df1"], c(df1 = 1))
})
