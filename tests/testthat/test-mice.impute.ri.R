xname <- c("age", "hgt", "wgt")
br <- boys[c(1:10, 101:110, 501:510, 601:620, 701:710), ]
r <- stats::complete.cases(br[, xname])
x <- br[r, xname]
y <- br[r, "tv"]
ry <- !is.na(y)
wy2 <- rep(TRUE, length(y))

test_that("MICE-IMPUTE-RI-001: Returns requested length", {
  expect_length(mice.impute.ri(y, ry, x), sum(!ry))
  expect_length(mice.impute.ri(y, ry, x, wy = wy2), sum(wy2))
})

test_that("MICE-IMPUTE-RI-002: Accepts ri.maxit and is reproducible with a seed", {
  set.seed(1)
  a <- mice.impute.ri(y, ry, x, ri.maxit = 2)
  set.seed(1)
  b <- mice.impute.ri(y, ry, x, ri.maxit = 2)
  expect_identical(a, b)
  expect_length(a, sum(!ry))
})

test_that("MICE-IMPUTE-RI-003: mice() runs with method ri", {
  # ri draws a response indicator and uses it as predictor. With very few
  # missing values that indicator can become constant, so use data with a
  # substantial fraction missing.
  set.seed(1)
  n <- 200
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  d$y <- d$x1 + rnorm(n)
  d$y[rbinom(n, 1, 0.3) == 1] <- NA
  imp <- mice(d, method = "ri", m = 2, maxit = 2, seed = 1, print = FALSE)
  expect_false(anyNA(complete(imp)))
})
