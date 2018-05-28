context("mice.impute.pmm")

xname <- c("age", "hgt", "wgt")
br <- boys[c(1:10, 101:110, 501:510, 601:620, 701:710), ]
r <- stats::complete.cases(br[, xname])
x <- br[r, xname]
y <- br[r, "tv"]
ry <- !is.na(y)

wy1 <- !ry
wy2 <- rep(TRUE, length(y))
wy3 <- rep(FALSE, length(y))
wy4 <- rep(c(TRUE, FALSE), times = c(1, length(y) - 1))

test_that("Returns requested length", {
  expect_equal(length(mice.impute.pmm(y, ry, x)), sum(!ry))
  expect_equal(length(mice.impute.pmm(y, ry, x, wy = wy1)), sum(wy1))
  expect_equal(length(mice.impute.pmm(y, ry, x, wy = wy2)), sum(wy2))
  expect_equal(length(mice.impute.pmm(y, ry, x, wy = wy3)), sum(wy3))
  expect_equal(length(mice.impute.pmm(y, ry, x, wy = wy4)), sum(wy4))
})
