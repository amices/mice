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

test_that("Excludes donors", {
  expect_false(all(c(15:25) %in% mice.impute.pmm(y, ry, x, exclude = c(15:25))))
})

imp1 <- mice(nhanes, printFlag = FALSE, seed = 123)
imp2 <- mice(nhanes, printFlag = FALSE, seed = 123, exclude = c(-1, 1032))
test_that("excluding unobserved values does not impact pmm", {
  expect_identical(imp1$imp, imp2$imp)
})
