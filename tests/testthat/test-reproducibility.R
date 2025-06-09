context("reproducibility")

# Test reproducibility of mice with different settings
imp1 <- mice(nhanes, m = 2, maxit = 2, seed = 1, print = FALSE, use.future = FALSE)
imp2 <- mice(nhanes, m = 2, maxit = 2, seed = 1, print = FALSE, use.future = TRUE)
imp3 <- mice(nhanes, m = 2, maxit = 2, seed = 1, print = FALSE, n.core = 1)
imp4 <- mice(nhanes, m = 2, maxit = 2, seed = 1, print = FALSE, n.core = 2)

test_that("changing use.future or n.core produces same imputations", {
  expect_identical(imp1$imp, imp2$imp)
  expect_identical(imp1$imp, imp3$imp)
  expect_identical(imp1$imp, imp4$imp)
})
