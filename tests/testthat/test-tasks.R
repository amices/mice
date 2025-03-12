context("tasks")

# We have to test the following cases:

# - Does train-run setup with a factor variable produce imputations?
# - Does train-run setup with a factor variable produce imputations when the factor has fewer categories during running than training?
# - Does train-run setup with a factor variable produce imputations when the factor has more categories during running than training?
# - Does train-run setup with a factor variable produce imputations when only one factor level is present during training?
# - Does train-run setup with a factor variable produce imputations when only one factor level is present during running?

test_that("tasks work with factor with same number of categories", {
  expect_silent(imp1 <- mice(nhanes2, m = 3, maxit = 1, task = "train", method = "pmm", print = FALSE))
  expect_false(is.null(imp1$models$bmi$"1"$lookup))
  expect_error(imp2 <- mice(nhanes2, m = 4, maxit = 1, task = "fill", method = "pmm", models = imp1$models, print = FALSE), "Number of imputations")
  expect_silent(imp2 <- mice(nhanes2, m = 2, maxit = 1, task = "fill", method = "pmm", models = imp1$models, print = FALSE))
  expect_silent(imp2 <- mice(nhanes2[1,], m = 2, maxit = 1, task = "fill", method = "pmm", models = imp1$models, print = FALSE))
})

test_that("fully synthetic datasets can be created from completely observed variables", {
  dataset <- complete(mice(nhanes2, m = 1, maxit = 1, method = "pmm", print = FALSE))
  expect_silent(imp1 <- mice(dataset, m = 2, maxit = 1, task = "train", method = "pmm", print = FALSE))
  expect_false(is.null(imp1$models$age$"1"$lookup))
  expect_silent(imp2 <- mice(dataset, where = make.where(dataset, "all"), m = 2, maxit = 3, task = "fill", method = "pmm", models = imp1$models, print = FALSE))
  synt1 <- complete(imp2, 1)
  synt2 <- complete(imp2, 2)
})

