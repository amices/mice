context("pool.r.squared")

data(nhanes)
imp <- mice::mice(nhanes, maxit = 2, m = 2, seed = 10, print = FALSE)
fit_mira <- with(data = imp, exp = lm(chl ~ age + bmi))
fit_mipo <- mice::pool(fit_mira)

test_that("pool.r.squared mira", {
  result <- as.vector(pool.r.squared(fit_mira, adjusted = FALSE)[1, ])
  expect_equal(length(result), 4L)
})

test_that("r.squared mipo", {
  result <- as.vector(pool.r.squared(fit_mipo, adjusted = FALSE)[1, ])
  expect_equal(length(result), 4L)
})
