context("mice.impute.norm")

#########################
#TEST 1: Simple problem #
#########################
set.seed(123)

#generate data
y <- rnorm(10)
x <- y * .3 + rnorm(10, 0, .25)
x2 <- x + rnorm(10, 2, 3)
x <- cbind(1, x, x2)

#make missingness
y[5:6] <- NA
ry <- !is.na(y)

set.seed(123)
svd <- .norm.draw(y, ry, x, ls.meth = "svd")
set.seed(123)
ridge <- .norm.draw(y, ry, x, ls.meth = "ridge")
set.seed(123)
qr <- .norm.draw(y, ry, x, ls.meth = "qr")

#tests for test1
test_that("Estimates are equal", {
  expect_equal(svd$coef, matrix(qr$coef))
  expect_equal(svd$beta, matrix(qr$beta))
  expect_equal(svd$sigma, qr$sigma)
})
test_that("Correct estimation method used", {
  expect_equal(svd$estimation, "svd")
  expect_equal(qr$estimation, "qr")
  expect_equal(ridge$estimation, "ridge")
})
#svd and qr deliver same estimates; ridge should be different!

#####################################
#TEST 2: extremely high correlation #
#####################################
x <- matrix(c(1:1000, seq(from = 2, to = 2000, by=2)) + rnorm(1000), nrow = 1000, ncol = 2)
y <- t(c(5, 3) %*% t(x))
y[5:6] <- NA
ry <- !is.na(y)

svd <- .norm.draw(y, ry, x, ls.meth = "svd")
ridge <- .norm.draw(y, ry, x, ls.meth = "ridge")
qr <- .norm.draw(y, ry, x, ls.meth = "qr")

#tests for test2
test_that("Estimates are equal", {
  expect_equal(svd$coef, matrix(qr$coef))
  expect_equal(svd$beta, matrix(qr$beta))
  expect_equal(svd$sigma, qr$sigma)
})
test_that("Correct estimation method used", {
  expect_equal(svd$estimation, "svd")
  expect_equal(qr$estimation, "qr")
  expect_equal(ridge$estimation, "ridge")
})
#svd and qr deliver same estimates; ridge should be different!

#####################################
#TEST 3: correct imputation model   #
#####################################

imp.qr <- mice(mammalsleep[, -1], ls.meth = "qr", seed = 123, print = FALSE)
imp.svd <- mice(mammalsleep[, -1], ls.meth = "svd", seed = 123, print = FALSE)
imp.ridge <- mice(mammalsleep[, -1], ls.meth = "ridge", seed = 123, print = FALSE)

test_that("Imputations are equal", {
  expect_equal(imp.qr$imp, imp.svd$imp)
  expect_false(identical(imp.qr$imp, imp.ridge$imp))
})

#####################################
#TEST 4: exactly singular system    #
#####################################
# test on faulty imputation model (exactly singular system)

imp.qr <- mice(mammalsleep, ls.meth = "qr", seed = 123, print = FALSE)
imp.svd <- mice(mammalsleep, ls.meth = "svd", seed = 123, print = FALSE)
imp.ridge <- mice(mammalsleep, ls.meth = "ridge", seed = 123, print = FALSE)

test_that("Imputations are not equal", {
  expect_false(identical(imp.qr$imp, imp.svd$imp))
  expect_false(identical(imp.qr$imp, imp.ridge$imp))
})
#difference stems from added ridge penalty when necessary (when and where depends
#on starting state of algorithm).

#####################################
#TEST 4: returns requested length   #
#####################################
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
  expect_equal(length(mice.impute.norm(y, ry, x)), sum(!ry))
  expect_equal(length(mice.impute.norm(y, ry, x, wy = wy1)), sum(wy1))
  expect_equal(length(mice.impute.norm(y, ry, x, wy = wy2)), sum(wy2))
  expect_equal(length(mice.impute.norm(y, ry, x, wy = wy3)), sum(wy3))
  expect_equal(length(mice.impute.norm(y, ry, x, wy = wy4)), sum(wy4))
})

