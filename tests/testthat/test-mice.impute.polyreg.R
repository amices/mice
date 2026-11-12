set.seed(1)
n <- 100
x <- cbind(x1 = rnorm(n), x2 = rnorm(n))
y <- factor(c("a", "b", "c")[1 + (x[, 1] + rnorm(n) > 0) + (x[, 2] > 1)])
y[sample(n, 15)] <- NA
ry <- !is.na(y)
wy2 <- rep(TRUE, n)

y2 <- factor(ifelse(x[, 1] + rnorm(n) > 0, "no", "yes"))
y2[sample(n, 10)] <- NA
ry2 <- !is.na(y2)

test_that("MICE-IMPUTE-POLYREG-001: Returns requested length", {
  expect_length(mice.impute.polyreg(y, ry, x), sum(!ry))
  expect_length(mice.impute.polyreg(y, ry, x, wy = wy2), sum(wy2))
})

test_that("MICE-IMPUTE-POLYREG-002: Imputations are level labels of y", {
  imp <- mice.impute.polyreg(y, ry, x)
  expect_type(imp, "character")
  expect_true(all(imp %in% levels(y)))
  expect_false(anyNA(imp))
})

test_that("MICE-IMPUTE-POLYREG-003: Works for a two-level factor", {
  imp <- mice.impute.polyreg(y2, ry2, x)
  expect_type(imp, "character")
  expect_true(all(imp %in% levels(y2)))
  expect_length(imp, sum(!ry2))
})

test_that("MICE-IMPUTE-POLYREG-004: Passes nnet arguments through", {
  expect_no_error(
    mice.impute.polyreg(y, ry, x, nnet.maxit = 5, nnet.MaxNWts = 100)
  )
})

test_that("MICE-IMPUTE-POLYREG-005: mice() runs with method polyreg", {
  dat <- data.frame(x, y = y)
  imp <- mice(
    dat,
    method = c("", "", "polyreg"),
    m = 2,
    maxit = 2,
    seed = 1,
    print = FALSE
  )
  cmp <- complete(imp)
  expect_false(anyNA(cmp))
  expect_identical(levels(cmp$y), levels(y))
})
