context("models")

trained <- mice(nhanes2, m = 2, maxit = 1, print = FALSE, tasks = "train", seed = 1)
models_env <- import.models.env(trained$models)
models_list <- export.models.env(models_env)

test_that("list and environment representation have equal size", {
  expect_identical(object.size(trained$models), object.size(models_list))
})
