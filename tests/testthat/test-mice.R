context("mice")

nhanes_mids <- mice(nhanes, m = 2, print = FALSE)
nhanes_complete <- complete(nhanes_mids)

test_that("No missing values remain in imputed nhanes data set", {
  expect_gt(sum(is.na(nhanes)), 0)
  expect_equal(sum(is.na(nhanes_complete)), 0)
})

test_that("Data set in returned mids object is identical to nhanes data set", {
  expect_identical(nhanes_mids$data, nhanes)
})

# blocks

# imp <- mice(nhanes, blocks = list(B1 = c("age", "bmi", "hyp"), chl = "chl"))

# where

# # all TRUE
# imp <- mice(nhanes, where = matrix(TRUE, nrow = 25, ncol = 4), maxit = 1)
# 
# # all FALSE
# imp <- mice(nhanes, where = matrix(FALSE, nrow = 25, ncol = 4), maxit = 1)
# 
# # alternate
# imp <- mice(nhanes, where = matrix(c(FALSE, TRUE), nrow = 25, ncol = 4), maxit = 1)
# 
# 
# # nhanes2
# # all TRUE
# imp <- mice(nhanes2, where = matrix(TRUE, nrow = 25, ncol = 4), maxit = 1)
# 
# # all FALSE
# imp <- mice(nhanes2, where = matrix(FALSE, nrow = 25, ncol = 4), maxit = 1)
# 
# # alternate
# imp <- mice(nhanes2, where = matrix(c(FALSE, TRUE), nrow = 25, ncol = 4), maxit = 1)
# 
# # error on complete.cases
# imp <- mice(nhanes2, where = matrix(TRUE, nrow = 25, ncol = 4), maxit = 1, meth = c("pmm", "", "", ""))
