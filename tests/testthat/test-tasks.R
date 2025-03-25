context("tasks")

# We have to test the following cases:

# - Does train-run setup with a factor variable produce imputations? YES
# - Does train-run setup with a factor variable produce imputations when the factor has fewer categories during running than training? YES
# - Does train-run setup with a factor variable produce imputations when the factor has more categories during running than training?

test_that("m filling recycles training models", {
  expect_silent(imp1 <- mice(nhanes2, m = 2, maxit = 1, task = "train", method = "pmm", print = FALSE))
  expect_false(is.null(imp1$models$bmi[[1]]$lookup))
  expect_silent(imp2 <- mice(nhanes2, m = 4, maxit = 1, task = "fill", method = "pmm", models = imp1$models, print = FALSE))
  expect_silent(imp2 <- mice(nhanes2[1, ], m = 2, maxit = 1, task = "fill", method = "pmm", models = imp1$models, print = FALSE))
})

test_that("fully synthetic datasets can be created from completely observed variables", {
  dataset <- complete(mice(nhanes2, m = 1, maxit = 1, method = "pmm", print = FALSE))
  expect_silent(imp1 <- mice(dataset, m = 2, maxit = 1, task = "train", method = "pmm", print = FALSE))
  expect_false(is.null(imp1$models$age[[1]]$lookup))
  expect_silent(imp2 <- mice(dataset, where = make.where(dataset, "all"), m = 2, maxit = 3, task = "fill", method = "pmm", models = imp1$models, print = FALSE))
  synt1 <- complete(imp2, 1)
  synt2 <- complete(imp2, 2)
})

test_that("the procedure informs the user about a mismatch between model and data", {
  expect_silent(imp1 <- mice(nhanes2, m = 3, maxit = 1, task = "train", method = "pmm", print = FALSE))
  newdata <- nhanes2
  newdata$age <- factor(newdata$age, levels = c(levels(newdata$age), "not_a_level"))
  newdata$age[1] <- "not_a_level"
  newdata$age[2] <- NA
  expect_silent(imp2 <- mice(newdata, m = 1, maxit = 1, task = "fill", method = "pmm", models = imp1$models, print = FALSE))
  newdata <- nhanes2
  levels(newdata$age)[levels(newdata$age) == "60-99"] <- "60+"
  expect_error(imp2 <- mice(newdata, m = 1, maxit = 1, task = "fill", method = "pmm", models = imp1$models, print = FALSE), "Model-Data mismatch")
})
