context("check.formulas")

# predictorMatrix specification (baseline)
imp_pm <- mice(nhanes, seed = 51212, print = FALSE, m = 1)

test_that("predictorMatrix and dot-formula yield same imputations", {
  form <- list(age ~ ., bmi ~ ., hyp ~ ., chl ~ .)
  imp_f <- mice(nhanes, formulas = form, seed = 51212, print = FALSE, m = 1)
  expect_identical(complete(imp_pm), complete(imp_f))
  expect_identical(imp_pm$imp, imp_f$imp)
})

test_that("name.formulas names formulas by LHS variable", {
  form <- name.formulas(list(bmi ~ ., hyp ~ ., chl ~ .))
  expect_named(form, c("bmi", "hyp", "chl"))
  expect_true(all(sapply(form, inherits, "formula")))
})

test_that("name.formulas dot-formula produces a valid mids object", {
  form <- name.formulas(list(bmi ~ ., hyp ~ ., chl ~ .))
  imp <- mice(nhanes, formulas = form, seed = 51212, print = FALSE, m = 1)
  expect_true(is.mids(imp))
})

test_that("name.blocks on formulas names by B-prefix", {
  form <- name.blocks(list(bmi ~ ., hyp ~ ., chl ~ .))
  expect_named(form, c("B1", "B2", "B3"))
  expect_true(all(sapply(form, inherits, "formula")))
})

test_that("name.blocks dot-formula produces a valid mids object", {
  form <- name.blocks(list(bmi ~ ., hyp ~ ., chl ~ .))
  imp <- mice(nhanes, formulas = form, seed = 51212, print = FALSE, m = 1)
  expect_true(is.mids(imp))
})

test_that("intercept-only formula (~ 1) produces a valid mids object", {
  skip("not yet implemented: ~ 1 formula has no LHS, check.method cannot find variable name")
  imp <- mice(nhanes,
    formulas = list(bmi = ~ 1, chl = ~ 1, hyp = ~ 1),
    method = "norm.predict", seed = 1, m = 1, print = FALSE
  )
  expect_true(is.mids(imp))
})

test_that("character string formulas produce a valid mids object", {
  skip("not yet implemented: name.formulas() rejects character strings")
  imp <- mice(nhanes,
    formulas = list(
      bmi = "bmi ~ hyp + chl",
      hyp = "hyp ~ bmi + chl",
      chl = "chl ~ bmi + hyp"
    ),
    seed = 1, m = 1, print = FALSE
  )
  expect_true(is.mids(imp))
})
