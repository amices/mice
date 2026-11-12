set.seed(1)
y <- rnorm(30)
y[c(2, 9, 15, 27)] <- NA
ry <- !is.na(y)
x <- cbind(x1 = rnorm(30))
wy2 <- rep(TRUE, length(y))
wy3 <- rep(FALSE, length(y))

yf <- factor(sample(c("a", "b", "c"), 30, replace = TRUE))
yf[c(2, 9)] <- NA
ryf <- !is.na(yf)

test_that("MICE-IMPUTE-SAMPLE-001: Returns requested length", {
  expect_length(mice.impute.sample(y, ry, x), sum(!ry))
  expect_length(mice.impute.sample(y, ry, x, wy = wy2), sum(wy2))
  expect_length(mice.impute.sample(y, ry, x, wy = wy3), sum(wy3))
})

test_that("MICE-IMPUTE-SAMPLE-002: Draws come from the observed values", {
  imp <- mice.impute.sample(y, ry, x, wy = wy2)
  expect_true(all(imp %in% y[ry]))
})

test_that("MICE-IMPUTE-SAMPLE-003: Preserves factor type and levels", {
  imp <- mice.impute.sample(yf, ryf, x)
  expect_s3_class(imp, "factor")
  expect_identical(levels(imp), levels(yf))
  expect_true(all(imp %in% yf[ryf]))
})

test_that("MICE-IMPUTE-SAMPLE-004: mice() runs with method sample on mixed data", {
  imp <- mice(
    nhanes2,
    method = "sample",
    m = 2,
    maxit = 1,
    seed = 1,
    print = FALSE
  )
  cmp <- complete(imp)
  expect_false(anyNA(cmp))
  expect_identical(sapply(cmp, class), sapply(nhanes2, class))
})
