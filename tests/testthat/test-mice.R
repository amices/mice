context("mice: complete")

nhanes_mids <- mice(nhanes, m = 2, print = FALSE)
nhanes_complete <- complete(nhanes_mids)

test_that("No missing values remain in imputed nhanes data set", {
  expect_gt(sum(is.na(nhanes)), 0)
  expect_equal(sum(is.na(nhanes_complete)), 0)
})

test_that("Data set in returned mids object is identical to nhanes data set", {
  expect_identical(nhanes_mids$data, nhanes)
})

context("mice: blocks")

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
test_that("Method `cart` if all are binary", {
  expect_identical(unname(imp3$method[1]), "cart")
})
test_that("Method `cart` if all are ordered", {
  expect_identical(unname(imp4$method[1]), "cart")
})
test_that("Method `pmm` works with one block", {
  expect_identical(unname(imp5$method[1]), "pmm")
})


# check for equality of `scatter` and `collect` for univariate models
# the following models yield the same imputations
imp1 <- mice(nhanes, blocks = make.blocks(nhanes, "scatter"), 
             print = FALSE, m = 1, maxit = 1, seed = 123)
imp2 <- mice(nhanes, blocks = make.blocks(nhanes, "collect"), 
             print = FALSE, m = 1, maxit = 1, seed = 123)
imp3 <- mice(nhanes, blocks = list("age", c("bmi", "hyp", "chl")), 
             print = FALSE, m = 1, maxit = 1, seed = 123)
imp4 <- mice(nhanes, blocks = list(c("bmi", "hyp", "chl"), "age"), 
             print = FALSE, m = 1, maxit = 1, seed = 123)

test_that("Univariate yield same imputes for `scatter` and `collect`", {
  expect_identical(complete(imp1), complete(imp2))
  expect_identical(complete(imp1), complete(imp3))
  expect_identical(complete(imp1), complete(imp4))
})

# potentially, we may also change the visitSequence, but mice 
# is quite persistent in overwriting a user-specified
# visitSequence for complete columns, so this not 
# currently not an option. Defer optimizing this to later.

# another trick is to specify where for age by hand, so it forces 
# mice to impute age by pmm, but then, this would need to be 
# done in both imp1 and imp2 models.



context("mice: where")

# # all TRUE
imp1 <- mice(nhanes, where = matrix(TRUE, nrow = 25, ncol = 4), maxit = 1, 
             m = 1, print = FALSE)
 
# # all FALSE
imp2 <- mice(nhanes, where = matrix(FALSE, nrow = 25, ncol = 4), maxit = 1, 
             m = 1, print = FALSE)

# # alternate
imp3 <- mice(nhanes, where = matrix(c(FALSE, TRUE), nrow = 25, ncol = 4), 
             maxit = 1, m = 1, print = FALSE)
 
# # whacky situation where we expect no imputes for the incomplete cases
imp4 <- mice(nhanes2, where = matrix(TRUE, nrow = 25, ncol = 4), 
             maxit = 1, 
             meth = c("pmm", "", "", ""), m = 1, print = FALSE)

test_that("`where` produces correct number of imputes", {
  expect_identical(nrow(imp1$imp$age), 25L)
  expect_identical(nrow(imp2$imp$age), 0L)
  expect_identical(nrow(imp3$imp$age), 12L)
  expect_identical(sum(is.na(imp4$imp$age)), nrow(nhanes2) - sum(complete.cases(nhanes2)))
})
