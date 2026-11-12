xname <- c("age", "hgt", "wgt")
br <- boys[c(1:10, 101:110, 501:510, 601:620, 701:710), ]
r <- stats::complete.cases(br[, xname])
x <- br[r, xname]
y <- br[r, "tv"]
ry <- !is.na(y)
wy2 <- rep(TRUE, length(y))
wy3 <- rep(FALSE, length(y))
wy4 <- rep(c(TRUE, FALSE), times = c(1, length(y) - 1))

test_that("MICE-IMPUTE-NORM-BOOT-001: Returns requested length", {
  expect_length(mice.impute.norm.boot(y, ry, x), sum(!ry))
  expect_length(mice.impute.norm.boot(y, ry, x, wy = wy2), sum(wy2))
  expect_length(mice.impute.norm.boot(y, ry, x, wy = wy3), sum(wy3))
  expect_length(mice.impute.norm.boot(y, ry, x, wy = wy4), sum(wy4))
})

test_that("MICE-IMPUTE-NORM-BOOT-002: Is reproducible with a seed and stochastic without", {
  set.seed(1)
  a <- mice.impute.norm.boot(y, ry, x)
  set.seed(1)
  b <- mice.impute.norm.boot(y, ry, x)
  set.seed(2)
  c <- mice.impute.norm.boot(y, ry, x)
  expect_identical(a, b)
  expect_false(identical(a, c))
})

test_that("MICE-IMPUTE-NORM-BOOT-003: mice() runs with method norm.boot", {
  imp <- mice(
    nhanes,
    method = "norm.boot",
    m = 2,
    maxit = 2,
    seed = 1,
    print = FALSE
  )
  expect_false(anyNA(complete(imp)))
})
