context("actions")

# We have to test the following cases:

# - Does train-run setup with a factor variable produce imputations?
# - Does train-run setup with a factor variable produce imputations when the factor has fewer categories during running than training?
# - Does train-run setup with a factor variable produce imputations when the factor has more categories during running than training?
# - Does train-run setup with a factor variable produce imputations when only one factor level is present during training?
# - Does train-run setup with a factor variable produce imputations when only one factor level is present during running?

test_that("actions work with factor with same number of categories", {
  expect_silent(imp1 <- mice(nhanes2, m = 1, maxit = 1, act = "train", method = "pmmsplit", print = FALSE))
  expect_false(is.null(imp1$models$bmi$"1"$lookup))
  expect_silent(imp2 <- mice(nhanes2, m = 1, maxit = 1, act = "run", methode = "pmmsplit", models = imp1$models, print = FALSE))
  expect_identical(imp1$models$bmi$"1"$lookup, imp2$models$bmi$"1"$lookup)
})

test_that("training works on completely observed variables", {
  expect_silent(imp1 <- mice(nhanes2, m = 1, maxit = 1, act = "train", method = "pmmsplit", print = FALSE))
  expect_false(is.null(imp1$models$age$"1"$lookup))
  expect_silent(imp2 <- mice(nhanes2, m = 1, maxit = 1, act = "run", methode = "pmmsplit", models = imp1$models, print = FALSE))
  expect_identical(imp1$models$age$"1"$lookup, imp2$models$age$"1"$lookup)
})

