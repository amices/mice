context("reproducibility")

# Test reproducibility of mice with different settings
# imp1 <- mice(nhanes, m = 2, maxit = 2, seed = 1, print = FALSE, use.future = FALSE)
imp2 <- mice(nhanes, m = 2, maxit = 2, seed = 1, print = FALSE, use.future = TRUE)
imp3 <- mice(nhanes, m = 2, maxit = 2, seed = 1, print = FALSE, cores = 1)
imp4 <- mice(nhanes, m = 2, maxit = 2, seed = 1, print = FALSE, cores = 2)

test_that("changing cores produces same imputations", {
  expect_identical(imp2$imp, imp3$imp)
  expect_identical(imp2$imp, imp4$imp)
})



