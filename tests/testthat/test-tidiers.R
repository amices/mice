context("tidiers")

data(nhanes)
imp <- mice::mice(nhanes, maxit = 2, m = 2, seed = 1, print = FALSE, use.matcher = TRUE)
fit_mira <- with(imp, lm(chl ~ age + bmi))
fit_mipo <- mice::pool(fit_mira)

test_that("glance.mipo: nhanes lm", {
  tmp <- glance(fit_mipo)
  expect_true(inherits(tmp, "data.frame"))
})

test_that("tidy.mipo: nhanes lm", {
  tmp <- tidy(fit_mipo)
  expect_true(inherits(tmp, "data.frame"))
  expect_equal(dim(tmp), c(3, 13))

  tmp <- tidy(fit_mipo, conf.int = TRUE)
  expect_true(inherits(tmp, "data.frame"))
  expect_equal(dim(tmp), c(3, 15))

  tmp <- tidy(fit_mipo, conf.int = TRUE, conf.level = .99)
  expect_true(inherits(tmp, "data.frame"))
  expect_equal(dim(tmp), c(3, 15))
})
