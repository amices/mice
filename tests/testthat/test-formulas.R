context("formulas")

data <- nhanes

test_that("model.matrix() deletes incomplete cases", {
  expect_identical(dim(model.matrix(~age, data)), c(25L, 2L))
  expect_identical(dim(model.matrix(~chl, data)), c(15L, 2L))
  expect_identical(dim(model.matrix(~ poly(age), data)), c(25L, 2L))
  expect_error(
    model.matrix(~ poly(chl), data),
    "missing values are not allowed in 'poly'"
  )
  expect_identical(dim(model.matrix(~ poly(chl, raw = TRUE), data)), c(15L, 2L))
})

# in MICE we can now use poly()

fm <- list(bmi ~ age + hyp + cut(chl, 3))
expect_warning(mice(nhanes, formulas = fm, m = 1, maxit = 2, print = FALSE, autoremove = TRUE))
expect_silent(mice(nhanes, formulas = fm, m = 1, maxit = 2, print = FALSE, autoremove = FALSE))