context("check.visitSequence")

data <- nhanes

test_that("mice() takes numerical and character visitSequence", {
  expect_silent(imp <- mice(data, visitSequence = 4:1, m = 1, print = FALSE))
  expect_silent(imp <- mice(data, visitSequence = rev(names(data)), m = 1, print = FALSE))
})
