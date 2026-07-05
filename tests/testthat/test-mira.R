test_that("list prints without an error", {
  imp <- mice(nhanes, print = FALSE, maxit = 1, seed = 121, m = 1)
  fit <- with(imp, sd(bmi))
  expect_output(print(fit))
})
