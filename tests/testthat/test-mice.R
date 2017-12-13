context("mice complete")

nhanes_mids <- mice(nhanes, m = 2, print = FALSE)
nhanes_complete <- complete(nhanes_mids)

test_that("No missing values remain in imputed nhanes data set", {
  expect_gt(sum(is.na(nhanes)), 0)
  expect_equal(sum(is.na(nhanes_complete)), 0)
})

test_that("Data set in returned mids object is identical to nhanes data set", {
  expect_identical(nhanes_mids$data, nhanes)
})

context("mice blocks")

imp1 <- mice(nhanes, blocks = list(c("age", "hyp"), chl = "chl", "bmi"), 
            print = FALSE, m = 1, maxit = 1)
imp2 <- mice(nhanes2, blocks = list(c("age", "hyp", "bmi"), "chl", "bmi"), 
            print = FALSE, m = 1, maxit = 1)
imp3 <- mice(nhanes2, blocks = list(c("hyp", "hyp", "hyp"), "chl", "bmi"), 
             print = FALSE, m = 1, maxit = 1)
imp4 <- mice(boys, blocks = list(c("gen", "phb"), "tv"), 
             print = FALSE, m = 1, maxit = 1)
imp5 <- mice(nhanes, blocks = list(c("age", "hyp")), 
             print = FALSE, m = 1, maxit = 1)

test_that("Block names are generated automatically", {
  expect_identical(names(imp1$blocks), c("B1", "chl", "bmi"))
})
test_that("Method `pmm` is used for mixed variable types", {
  expect_identical(unname(imp2$method[1]), "pmm")
})
test_that("Method `logreg` if all are binary", {
  expect_identical(unname(imp3$method[1]), "logreg")
})
test_that("Method `polr` if all are ordered", {
  expect_identical(unname(imp4$method[1]), "polr")
})
test_that("Method `polr` works with one block", {
  expect_identical(unname(imp5$method[1]), "pmm")
})


test_that("Unknown names not accepted", {
  expect_error(
    mice(nhanes, blocks = list(c("bmi", "chl", "hey"), "weird"), 
         print = FALSE, m = 1, maxit = 1))
})


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
