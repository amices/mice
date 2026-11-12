set.seed(1)
y <- rnorm(20)
y[c(3, 7, 11)] <- NA
ry <- !is.na(y)
x <- cbind(x1 = rnorm(20))
wy2 <- rep(TRUE, length(y))
wy3 <- rep(FALSE, length(y))

test_that("MICE-IMPUTE-MEAN-001: Returns requested length", {
  expect_length(mice.impute.mean(y, ry, x), sum(!ry))
  expect_length(mice.impute.mean(y, ry, x, wy = wy2), sum(wy2))
  expect_length(mice.impute.mean(y, ry, x, wy = wy3), sum(wy3))
})

test_that("MICE-IMPUTE-MEAN-002: Imputes the observed mean and ignores x", {
  expect_equal(unique(mice.impute.mean(y, ry, x)), mean(y[ry]))
  expect_equal(mice.impute.mean(y, ry), mice.impute.mean(y, ry, x))
})

test_that("MICE-IMPUTE-MEAN-003: mice() runs with method mean", {
  imp <- mice(
    nhanes,
    method = "mean",
    m = 2,
    maxit = 1,
    seed = 1,
    print = FALSE
  )
  expect_false(anyNA(complete(imp)))
  expect_true(all(imp$imp$bmi == mean(nhanes$bmi, na.rm = TRUE)))
})
