context("mice-initialize")

data <- nhanes

# case A: no predictorMatrix, blocks or formulas arguments

imp1 <- mice(data, print = FALSE, m = 1, maxit = 1)
pred <- imp1$predictorMatrix
form <- imp1$formulas
test_that("Case A finds blocks", {
  expect_identical(names(imp1$blocks), colnames(data))
})
test_that("Case A finds formulas", {
  expect_identical(attr(terms(form[["bmi"]]), "term.labels"),
                   names(pred["bmi", ])[pred["bmi", ] == 1])
})

# case B: only predictorMatrix argument

pred1 <- matrix(1, nrow = 4, ncol = 4)
pred2 <- matrix(1, nrow = 2, ncol = 2)
pred3 <- matrix(1, nrow = 2, ncol = 2, 
                dimnames = list(c("bmi", "hyp"), c("bmi", "hyp")))
pred4 <- matrix(1, nrow = 2, ncol = 3, 
                dimnames = list(c("bmi", "hyp"), c("bmi", "hyp", "chl")))
imp1 <- mice(data, predictorMatrix = pred1, print = FALSE, m = 1, maxit = 1)
imp3 <- mice(data, predictorMatrix = pred3, print = FALSE, m = 1, maxit = 1)
test_that("Case B tests the predictorMatrix", {
  expect_equal(nrow(imp1$predictorMatrix), 4L)
  expect_error(mice(data, predictorMatrix = pred2,
                    "Missing row/column names in `predictorMatrix`."))
  expect_equal(nrow(imp3$predictorMatrix), 2L)
  expect_error(mice(data, predictorMatrix = pred4))
})

pred <- imp3$predictorMatrix
blocks <- imp3$blocks
test_that("Case B finds blocks", {
  expect_identical(names(blocks), c("bmi", "hyp"))
})

form <- imp3$formulas
test_that("Case B finds formulas", {
  expect_identical(attr(terms(form[["bmi"]]), "term.labels"),
                   names(pred["bmi", ])[pred["bmi", ] == 1])
})


# Case C: Only blocks argument

imp1 <- mice(data, blocks = list("bmi", "chl", "hyp"), print = FALSE, m = 1, maxit = 1, seed = 11)
imp2 <- mice(data, blocks = list(c("bmi", "chl"), "hyp"), print = FALSE, m = 1, maxit = 1, seed = 11)
imp3 <- mice(data, blocks = list(all = c("bmi", "chl", "hyp")), print = FALSE, m = 1, maxit = 1, seed = 11)

test_that("Case C finds blocks", {
  expect_identical(names(imp2$blocks), c("B1", "hyp"))
  expect_identical(names(imp3$blocks), c("all"))
})

test_that("Case C finds predictorMatrix", {
  expect_identical(imp2$predictorMatrix["hyp", "hyp"], 0)
  expect_identical(dim(imp3$predictorMatrix), c(1L, 4L))
})

test_that("Case C finds formulas", {
  expect_identical(attr(terms(imp2$formulas[["B1"]]), "term.labels"),
                   colnames(data))
})

test_that("Case C yields same imputations for FCS and multivariate", {
  expect_identical(complete(imp1), complete(imp2))
  expect_identical(complete(imp1), complete(imp3))
})



# Case D: Only formulas argument

# univariate models
form1 <- list(bmi ~ age + hyp + chl,
              hyp ~ age + bmi + chl,
              chl ~ age + bmi + hyp)
imp1 <- mice(data, formulas = form1, print = FALSE, m = 1, maxit = 1, seed = 12199)

# same model using dot notation
form2 <- list(bmi ~ ., hyp ~ ., chl ~ .)
imp2 <- mice(data, formulas = form2, print = FALSE, m = 1, maxit = 1, seed = 12199)

# multivariate models (= repeated univariate)
form3 <- list(bmi + hyp ~ age + chl,
              chl ~ age + bmi + hyp)
imp3 <- mice(data, formulas = form3, print = FALSE, m = 1, maxit = 1, seed = 12199)

# same model using dot notation
form4 <- list(bmi + hyp ~ ., chl ~ .)
imp4 <- mice(data, formulas = form4, print = FALSE, m = 1, maxit = 1, seed = 12199)

test_that("Case D yields same imputations for dot notation", {
  expect_identical(complete(imp1), complete(imp2))
  # expect_identical(complete(imp3), complete(imp4))   FIXME
})

test_that("Case D yields same imputations for FCS and multivariate", {
  # expect_identical(complete(imp1), complete(imp3))   FIXME
  expect_identical(complete(imp2), complete(imp4))
})


