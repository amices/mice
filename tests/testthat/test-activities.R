context("actions")

# We have to test the following cases:

# - Does train-run setup with a factor variable produce imputations?
# - Does train-run setup with a factor variable produce imputations when the factor has fewer categories during running than training?
# - Does train-run setup with a factor variable produce imputations when the factor has more categories during running than training?
# - Does train-run setup with a factor variable produce imputations when only one factor level is present during training?
# - Does train-run setup with a factor variable produce imputations when only one factor level is present during running?

test_that("actions work with factor with same number of categories", {
  expect_silent(imp1 <- mice(nhanes2, m = 2, maxit = 2, act = "train", method = "pmmsplit", print = FALSE))
  expect_false(is.null(imp1$models$bmi$"1"$lookup))
  expect_silent(imp2 <- mice(nhanes2, m = 2, maxit = 2, act = "run", methode = "pmmsplit", models = imp1$models, print = FALSE))
  expect_identical(imp1$models$bmi$"1"$lookup, imp2$models$bmi$"1"$lookup)
})

test_that("training works on completely observed variables", {
  expect_silent(imp1 <- mice(nhanes2, m = 2, maxit = 2, act = "train", method = "pmmsplit", print = FALSE))
  expect_false(is.null(imp1$models$age$"1"$lookup))
  expect_silent(imp2 <- mice(nhanes2, m = 2, maxit = 2, act = "run", methode = "pmmsplit", models = imp1$models, print = FALSE))
  expect_identical(imp1$models$age$"1"$lookup, imp2$models$age$"1"$lookup)
})

# make a few missing values in age
nhanes2$age[10:15] <- NA
# remove category 60-99 from age during running
nhanes3 <- nhanes2
nhanes3[["age"]] <- droplevels(nhanes3[["age"]], exclude = "60-99")

test_that("training and running works with factor with different number of categories", {
  expect_silent(imp1 <- mice(nhanes2, m = 2, maxit = 2, act = "train", method = "pmmsplit", print = FALSE))
  expect_false(is.null(imp1$models$age$"1"$lookup))
  expect_silent(imp2 <- mice(nhanes3, m = 2, maxit = 2, act = "run", methode = "pmmsplit", models = imp1$models, print = FALSE))
  expect_identical(imp1$models$age$"1"$lookup, imp2$models$age$"1"$lookup)
})

