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
  expect_identical(
    attr(terms(form[["bmi"]]), "term.labels"),
    names(pred["bmi", ])[pred["bmi", ] == 1]
  )
})

# case B: only predictorMatrix argument

pred1 <- matrix(1, nrow = 4, ncol = 4)
pred2 <- matrix(1, nrow = 2, ncol = 2)
pred3 <- matrix(1,
  nrow = 2, ncol = 2,
  dimnames = list(c("bmi", "hyp"), c("bmi", "hyp"))
)
pred4 <- matrix(1,
  nrow = 2, ncol = 3,
  dimnames = list(c("bmi", "hyp"), c("bmi", "hyp", "chl"))
)
imp1 <- mice(data, predictorMatrix = pred1, print = FALSE, m = 1, maxit = 1)
imp3 <- mice(data, predictorMatrix = pred3, print = FALSE, m = 1, maxit = 1)
test_that("Case B tests the predictorMatrix", {
  expect_equal(nrow(imp1$predictorMatrix), 4L)
  expect_error(mice(data,
    predictorMatrix = pred2,
    "Missing row/column names in `predictorMatrix`."
  ))
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
  expect_identical(
    attr(terms(form[["bmi"]]), "term.labels"),
    names(pred["bmi", ])[pred["bmi", ] == 1]
  )
})


# Case C: Only blocks argument

imp1.0 <- mice(data, blocks = list("bmi", "chl", "hyp"), m = 1, maxit = 0, seed = 11)
imp2.0 <- mice(data, blocks = list(c("bmi", "chl"), "hyp"), m = 1, maxit = 0, seed = 11)
imp3.0 <- mice(data, blocks = list(all = c("bmi", "chl", "hyp")), m = 1, maxit = 0, seed = 11)

test_that("Case C imputations are identical after initialization", {
  expect_identical(complete(imp1.0), complete(imp2.0))
  expect_identical(complete(imp1.0), complete(imp3.0))
})

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
  expect_identical(sort(all.vars(imp2$formulas[["B1"]])), sort(colnames(data)))
})

test_that("Case C yields same imputations for FCS and multivariate", {
  expect_identical(complete(imp1), complete(imp2))
  expect_identical(complete(imp1), complete(imp3))
})



# Case D: Only formulas argument

# univariate models
form1 <- list(
  bmi ~ age + hyp + chl,
  hyp ~ age + bmi + chl,
  chl ~ age + bmi + hyp
)
imp1 <- mice(data,
  formulas = form1, method = "norm.nob",
  print = FALSE, m = 1, maxit = 1, seed = 12199
)

# same model using dot notation
form2 <- list(bmi ~ ., hyp ~ ., chl ~ .)
imp2 <- mice(data,
  formulas = form2, method = "norm.nob",
  print = FALSE, m = 1, maxit = 1, seed = 12199
)

# multivariate models (= repeated univariate)
form3 <- list(
  bmi + hyp ~ age + chl,
  chl ~ age + bmi + hyp
)
imp3 <- mice(data,
  formulas = form3, method = "norm.nob",
  print = FALSE, m = 1, maxit = 1, seed = 12199
)

# same model using dot notation
form4 <- list(bmi + hyp ~ ., chl ~ .)
imp4 <- mice(data,
  formulas = form4, method = "norm.nob",
  print = FALSE, m = 1, maxit = 1, seed = 12199
)

test_that("Case D yields same imputations for dot notation", {
  expect_identical(complete(imp1), complete(imp2))
  expect_identical(complete(imp3), complete(imp4))
})

test_that("Case D yields same imputations for FCS and multivariate", {
  expect_equal(complete(imp1), complete(imp3))
  expect_equal(complete(imp2), complete(imp4))
})


# Case E: predictMatrix and blocks
blocks1 <- make.blocks(c("bmi", "chl", "hyp", "age"))
blocks2 <- make.blocks(list(c("bmi", "chl"), "hyp"))
blocks3 <- make.blocks(list(all = c("bmi", "chl", "hyp")))

pred1 <- make.predictorMatrix(data, blocks = blocks1)
pred2 <- make.predictorMatrix(data, blocks = blocks2)
pred3 <- make.predictorMatrix(data, blocks = blocks3)

imp1 <- mice(data, blocks = blocks1, pred = pred1, m = 1, maxit = 1, print = FALSE)
imp1a <- mice(data, blocks = blocks1, pred = matrix(1, nr = 4, nc = 4), m = 1, maxit = 1, print = FALSE)
imp2 <- mice(data, blocks = blocks2, pred = pred2, m = 1, maxit = 1, print = FALSE)
imp2a <- mice(data, blocks = blocks2, pred = matrix(1, nr = 2, nc = 4), m = 1, maxit = 1, print = FALSE)
imp3 <- mice(data, blocks = blocks3, pred = pred3, m = 1, maxit = 1, print = FALSE)
imp3a <- mice(data, blocks = blocks3, pred = matrix(1, nr = 1, nc = 4), m = 1, maxit = 1, print = FALSE)

test_that("Case E borrows rownames from blocks", {
  expect_identical(rownames(imp1a$predictorMatrix), names(blocks1))
  expect_identical(rownames(imp2a$predictorMatrix), names(blocks2))
  expect_identical(rownames(imp3a$predictorMatrix), names(blocks3))
})

test_that("Case E borrows colnames from data", {
  expect_identical(colnames(imp1a$predictorMatrix), names(data))
  expect_identical(colnames(imp2a$predictorMatrix), names(data))
  expect_identical(colnames(imp3a$predictorMatrix), names(data))
})

test_that("Case E name setting fails on incompatible sizes", {
  expect_error(
    mice(data, blocks = blocks2, pred = matrix(1, nr = 2, nc = 2)),
    "Unable to set column names of predictorMatrix"
  )
  expect_error(
    mice(data, blocks = blocks2, pred = matrix(1, nr = 1, nc = 4)),
    "Unable to set row names of predictorMatrix"
  )
  expect_error(mice(data, blocks = blocks2, pred = matrix(1, nr = 4, nc = 4)))
})

colnames(pred1) <- c("A", "B", "chl", "bmi")
pred2a <- pred2[, -(1:4), drop = FALSE]
test_that("Case E detects incompatible arguments", {
  expect_error(
    mice(data, blocks = blocks1, pred = pred1),
    "Names not found in data: A, B"
  )
  expect_error(
    mice(data, blocks = blocks1, pred = pred2),
    "Names not found in blocks: B1"
  )
  expect_error(
    mice(data, blocks = blocks2, pred = matrix(1, nr = 1, nc = 4)),
    "Unable to set row names of predictorMatrix"
  )
  expect_error(mice(data, blocks = blocks2, pred = matrix(1, nr = 4, nc = 4)))
  expect_error(
    mice(data, blocks = blocks2, pred = pred2a),
    "predictorMatrix has no rows or columns"
  )
})


# Case F: predictMatrix and formulas

blocks1 <- make.blocks(c("bmi", "chl", "hyp", "age"))
blocks2 <- make.blocks(list(c("bmi", "hyp"), "hyp"))

pred1 <- make.predictorMatrix(data, blocks = blocks1)
pred2 <- make.predictorMatrix(data, blocks = blocks2)

form1 <- list(
  bmi ~ age + hyp + chl,
  hyp ~ age + bmi + chl,
  chl ~ age + bmi + hyp
)
form2 <- list(bmi ~ ., hyp ~ ., chl ~ .)
form3 <- list(
  bmi + hyp ~ age + chl,
  chl ~ age + bmi + hyp
)
form4 <- list(bmi + hyp ~ ., chl ~ .)

# blocks1 and form1 are compatible
imp1 <- mice(data, formulas = form1, pred = matrix(1, nr = 4, nc = 4), m = 1, maxit = 1, print = FALSE, seed = 3)
test_that("Case F combines forms and pred in blocks", {
  expect_identical(unname(attr(imp1$blocks, "calltype")), c(rep("formula", 3), "type"))
})

# dots and unnamed predictorMatrix
imp2 <- mice(data, formulas = form2, pred = matrix(1, nr = 4, nc = 4), m = 1, maxit = 1, print = FALSE, seed = 3)
test_that("Case F dots and specified form produce same imputes", {
  expect_identical(complete(imp1), complete(imp2))
})

# error
test_that("Case F generates error if it cannot handle non-square predictor", {
  expect_error(
    mice(data, formulas = form2, pred = pred2),
    "If no blocks are specified, predictorMatrix must have same number of rows and columns"
  )
})

## Error in formulas[[h]] : subscript out of bounds
imp3 <- mice(data, formulas = form3, pred = pred1, m = 1, maxit = 0, print = FALSE, seed = 3)
imp3a <- mice(data, formulas = form3, pred = pred1, m = 1, maxit = 1, print = FALSE, seed = 3)

# err on matrix columns
nh <- nhanes
nh$hyp <- as.matrix(nh$hyp)
test_that("MICE does not accept data.frames with embedded matrix ", {
  expect_error(
    mice(nh),
    "Cannot handle columns with class matrix: hyp"
  )
})

