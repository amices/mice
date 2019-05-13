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

test_that("blocks run as expected", {
  expect_silent(imp1b <<- mice(nhanes, 
                              blocks = list(c("age", "hyp"), chl = "chl", "bmi"), 
                              print = FALSE, m = 1, maxit = 1, seed = 1))
  expect_silent(imp2b <<- mice(nhanes2, 
                              blocks = list(c("age", "hyp", "bmi"), "chl", "bmi"), 
                              print = FALSE, m = 1, maxit = 1, seed = 1))
  # expect_silent(imp3b <<- mice(nhanes2, 
  #                             blocks = list(c("hyp", "hyp", "hyp"), "chl", "bmi"), 
  #                             print = FALSE, m = 1, maxit = 1, seed = 1))
  expect_silent(imp4b <<- mice(boys, 
                               blocks = list(c("gen", "phb"), "tv"),
                               print = FALSE, m = 1, maxit = 1, seed = 1))
  expect_silent(imp5b <<- mice(nhanes, 
                              blocks = list(c("age", "hyp")), 
                              print = FALSE, m = 1, maxit = 1, seed = 1))
  })

test_that("Block names are generated automatically", {
  expect_identical(names(imp1b$blocks), c("B1", "chl", "bmi"))
})
test_that("Method `pmm` is used for mixed variable types", {
  expect_identical(unname(imp2b$method[1]), "pmm")
})
# test_that("Method `logreg` if all are binary", {
#   expect_identical(unname(imp3b$method[1]), "logreg")
# })
test_that("Method `polr` if all are ordered", {
  expect_identical(unname(imp4b$method[1]), "polr")
})
test_that("Method `polr` works with one block", {
  expect_identical(unname(imp5b$method[1]), "pmm")
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

context("mice: formulas")

test_that("formulas run as expected", {
  expect_silent(imp1f <<- mice(nhanes, 
                              formulas = list(age + hyp ~ chl + bmi, 
                                              chl ~ age + hyp + bmi,
                                              bmi ~ age + hyp + chl),
                              print = FALSE, m = 1, maxit = 1, seed = 1))
  expect_warning(imp2f <<- mice(nhanes2, 
                              formulas = list(age + hyp + bmi ~ chl + bmi, 
                                              chl ~ age + hyp + bmi + bmi,
                                              bmi ~ age + hyp + bmi + chl),
                              print = FALSE, m = 1, maxit = 1, seed = 1))
  # expect_silent(imp3f <<- mice(nhanes2, 
  #                             formulas = list( hyp + hyp + hyp ~ chl + bmi,
  #                                              chl ~ hyp + hyp + hyp + bmi,
  #                                              bmi ~ hyp + hyp + hyp + chl), 
  #                             print = FALSE, m = 1, maxit = 1, seed = 1))
  expect_silent(imp4f <<- mice(boys, 
                               formulas = list(gen + phb ~ tv,
                                               tv ~ gen + phb), 
                               print = FALSE, m = 1, maxit = 1, seed = 1))
  expect_silent(imp5f <<- mice(nhanes, 
                              formulas = list(age + hyp ~ 1), 
                              print = FALSE, m = 1, maxit = 1, seed = 1))
})

test_that("Formula names are generated automatically", {
  expect_identical(names(imp1f$blocks), c("F1", "chl", "bmi"))
})
test_that("Method `pmm` is used for mixed variable types", {
  expect_identical(unname(imp2f$method[1]), "pmm")
})
# test_that("Method `logreg` if all are binary", {
#   expect_identical(unname(imp3f$method[1]), "logreg")
# })
test_that("Method `polr` if all are ordered", {
  expect_identical(unname(imp4f$method[1]), "polr")
})
test_that("Method `polr` works with one block", {
  expect_identical(unname(imp5f$method[1]), "pmm")
})


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
