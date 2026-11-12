xname <- c("age", "hgt", "wgt")
br <- boys[c(1:10, 101:110, 501:510, 601:620, 701:710), ]
r <- stats::complete.cases(br[, xname])
x <- br[r, xname]
y <- br[r, "tv"]
ry <- !is.na(y)
wy2 <- rep(TRUE, length(y))

test_that("MICE-IMPUTE-MIDASTOUCH-001: Returns requested length", {
  expect_length(mice.impute.midastouch(y, ry, x), sum(!ry))
  expect_length(mice.impute.midastouch(y, ry, x, wy = wy2), sum(wy2))
})

test_that("MICE-IMPUTE-MIDASTOUCH-002: Imputations are drawn from observed values", {
  imp <- mice.impute.midastouch(y, ry, x, wy = wy2)
  expect_true(all(imp %in% y[ry]))
})

test_that("MICE-IMPUTE-MIDASTOUCH-003: Accepts outout = FALSE", {
  imp <- mice.impute.midastouch(y, ry, x, outout = FALSE)
  expect_length(imp, sum(!ry))
  expect_true(all(imp %in% y[ry]))
})

test_that("MICE-IMPUTE-MIDASTOUCH-004: mice() runs with method midastouch", {
  imp <- mice(
    nhanes,
    method = "midastouch",
    m = 2,
    maxit = 2,
    seed = 1,
    print = FALSE
  )
  expect_false(anyNA(complete(imp)))
})
