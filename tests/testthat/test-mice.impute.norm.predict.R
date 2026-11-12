xname <- c("age", "hgt", "wgt")
br <- boys[c(1:10, 101:110, 501:510, 601:620, 701:710), ]
r <- stats::complete.cases(br[, xname])
x <- br[r, xname]
y <- br[r, "tv"]
ry <- !is.na(y)
wy2 <- rep(TRUE, length(y))
wy3 <- rep(FALSE, length(y))
wy4 <- rep(c(TRUE, FALSE), times = c(1, length(y) - 1))

test_that("MICE-IMPUTE-NORM-PREDICT-001: Returns requested length", {
  expect_length(mice.impute.norm.predict(y, ry, x), sum(!ry))
  expect_length(mice.impute.norm.predict(y, ry, x, wy = wy2), sum(wy2))
  expect_length(mice.impute.norm.predict(y, ry, x, wy = wy3), sum(wy3))
  expect_length(mice.impute.norm.predict(y, ry, x, wy = wy4), sum(wy4))
})

test_that("MICE-IMPUTE-NORM-PREDICT-002: Is deterministic", {
  expect_identical(
    mice.impute.norm.predict(y, ry, x),
    mice.impute.norm.predict(y, ry, x)
  )
})

test_that("MICE-IMPUTE-NORM-PREDICT-003: Equals the least squares prediction", {
  fit <- lm(y ~ age + hgt + wgt, data = data.frame(y = y, x), subset = ry)
  expected <- unname(predict(fit, newdata = x[!ry, ]))
  expect_equal(
    as.vector(mice.impute.norm.predict(y, ry, x)),
    expected,
    tolerance = 1e-6
  )
})

test_that("MICE-IMPUTE-NORM-PREDICT-004: mice() runs with method norm.predict", {
  imp <- mice(
    nhanes,
    method = "norm.predict",
    m = 2,
    maxit = 2,
    seed = 1,
    print = FALSE
  )
  expect_false(anyNA(complete(imp)))
})
