skip_if_not_installed("pan")

d1 <- brandsma[1:200, c("sch", "lpo")]
pred1 <- make.predictorMatrix(d1)
pred1["lpo", "sch"] <- -2

d2 <- brandsma[1:200, c("sch", "iqv", "lpo")]
pred2 <- make.predictorMatrix(d2)
pred2["lpo", "sch"] <- -2

test_that("MICE-IMPUTE-2L-PAN-001: mice() runs the empty model", {
  imp <- mice(
    d1,
    method = "2l.pan",
    pred = pred1,
    m = 1,
    maxit = 1,
    seed = 1,
    print = FALSE
  )
  expect_false(anyNA(complete(imp)))
})

test_that("MICE-IMPUTE-2L-PAN-002: mice() runs with a fixed level-1 predictor", {
  imp <- mice(
    d2,
    method = c("", "", "2l.pan"),
    pred = pred2,
    m = 1,
    maxit = 1,
    seed = 1,
    print = FALSE
  )
  expect_false(anyNA(complete(imp)))
})

test_that("MICE-IMPUTE-2L-PAN-003: Direct call returns requested length", {
  y <- d2$lpo
  ry <- !is.na(y)
  x <- d2[, c("sch", "iqv")]
  type <- c(-2, 1)
  set.seed(1)
  imp <- mice.impute.2l.pan(y, ry, x, type, paniter = 50)
  expect_length(imp, sum(!ry))
  expect_false(anyNA(imp))
})
