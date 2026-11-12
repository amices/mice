skip_if_not_installed("kernlab")

xname <- c("age", "hgt", "wgt")
br <- boys[c(1:10, 101:110, 501:510, 601:620, 701:710), ]
r <- stats::complete.cases(br[, xname])
x <- br[r, xname]
y <- br[r, "tv"]
ry <- !is.na(y)
wy2 <- rep(TRUE, length(y))
wy3 <- rep(FALSE, length(y))

set.seed(1)
n <- 100
xf <- cbind(x1 = rnorm(n), x2 = rnorm(n))
yf <- factor(ifelse(xf[, 1] + rnorm(n) > 0, "no", "yes"))
yf[sample(n, 15)] <- NA
ryf <- !is.na(yf)

test_that("MICE-IMPUTE-SVM-001: Returns requested length for numeric y", {
  expect_length(mice.impute.svm(y, ry, x), sum(!ry))
  expect_length(mice.impute.svm(y, ry, x, wy = wy2), sum(wy2))
  expect_length(mice.impute.svm(y, ry, x, wy = wy3), sum(wy3))
})

test_that("MICE-IMPUTE-SVM-002: Returns a factor with the levels of a binary y", {
  imp <- mice.impute.svm(yf, ryf, xf)
  expect_s3_class(imp, "factor")
  expect_identical(levels(imp), levels(yf))
  expect_length(imp, sum(!ryf))
})

test_that("MICE-IMPUTE-SVM-003: mice() runs with method svm", {
  imp <- mice(nhanes, method = "svm", m = 1, maxit = 1, seed = 1, print = FALSE)
  expect_false(anyNA(complete(imp)))
})
