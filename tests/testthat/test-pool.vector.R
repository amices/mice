context("pool.vector")

# conventional flow
imp <- mice(nhanes2, m = 2, maxit = 2, seed = 1, print = FALSE)
fit <- with(imp, lm(chl ~ age + bmi + hyp))
est1 <- pool(fit)$pooled

# using pool.vector on tidy parameters
par <- summary(fit)[, c("term", "estimate", "std.error", "df.residual")]
est2 <- pool.vector(par)

test_that("pool(fit)$pooled and pool.vector(summary(fit)) are identical", {
  expect_identical(est1, est2)
})
