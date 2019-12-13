context("formulas")

data <- nhanes

test_that("model.matrix() deletes incomplete cases", {
  expect_identical(dim(model.matrix(~ age, data)), c(25L, 2L))
  expect_identical(dim(model.matrix(~ chl, data)), c(15L, 2L))
  expect_identical(dim(model.matrix(~ poly(age), data)), c(25L, 2L))
  expect_error(model.matrix(~ poly(chl), data), 
               "missing values are not allowed in 'poly'")
  expect_identical(dim(model.matrix(~ poly(chl, raw = TRUE), data)), c(15L, 2L))
})

# in MICE we can now use poly()

form <- list(bmi ~ poly(chl, 2) + age + hyp)
pred <- make.predictorMatrix(nhanes)
imp1 <- mice(data, form = form, pred = pred, m = 1, maxit = 2, print = FALSE)
