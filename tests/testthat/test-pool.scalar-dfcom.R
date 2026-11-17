imp <- mice(nhanes, maxit = 2, m = 2, print = FALSE, seed = 18210)
fit <- with(imp, lm(bmi ~ age))
est <- summary(getfit(fit), type = "tidy")
age <- est[est$term == "age", ]

test_that("POOL-SCALAR-DFCOM-001: dfcom defaults to n - k", {
  a <- pool.scalar(age$estimate, age$std.error^2, n = 25, k = 2)
  b <- pool.scalar(age$estimate, age$std.error^2, dfcom = 23)
  expect_identical(a, b)
})

test_that("POOL-SCALAR-DFCOM-002: dfcom = df.residual() matches pool() (#483)", {
  ps <- pool.scalar(
    age$estimate,
    age$std.error^2,
    dfcom = df.residual(fit$analyses[[1]])
  )
  pl <- pool(fit)$pooled
  pl <- pl[pl$term == "age", ]
  expect_equal(ps$qbar, pl$estimate)
  expect_equal(ps$t, pl$t)
  expect_equal(ps$df, pl$df)
})

test_that("POOL-SCALAR-DFCOM-003: dfcom must be a single number", {
  expect_error(pool.scalar(1:2, 1:2, dfcom = "a"), "single number")
  expect_error(pool.scalar(1:2, 1:2, dfcom = 1:2), "single number")
})
