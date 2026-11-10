data <- nhanes

test_that("FORMULAS-001: model.matrix() deletes incomplete cases", {
  expect_identical(dim(model.matrix(~age, data)), c(25L, 2L))
  expect_identical(dim(model.matrix(~chl, data)), c(15L, 2L))
  expect_identical(dim(model.matrix(~ poly(age), data)), c(25L, 2L))
  expect_error(
    model.matrix(~ poly(chl), data),
    "missing values are not allowed in 'poly'"
  )
  expect_identical(dim(model.matrix(~ poly(chl, raw = TRUE), data)), c(15L, 2L))
})

test_that("FORMULAS-002: mice() accepts poly() in a formula", {
  form <- list(bmi ~ poly(chl, 2) + age + hyp)
  pred <- make.predictorMatrix(nhanes)
  expect_silent(mice(data, form = form, pred = pred, m = 1, maxit = 2, print = FALSE))
})
