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

context("optimal scaling")

# Factor quantification
y <- factor(br[r, "tv"])

# impute factor by optimizing canonical correlation y, x
data1 <- data.frame(y, x)
test_that("cancor proceeds normally", {
  expect_silent(imp1 <- mice(data1, method = "pmm", remove.collinear = FALSE, eps = 0,
                             maxit = 1, m = 1, print = FALSE, seed = 1))
})

# > cca$xcoef[, 2L]
#    yf6     yf8    yf10    yf12    yf13    yf15    yf16    yf20    yf25
# 0.8458 -0.0469 -0.3182 -0.3458  0.0825  0.0799  0.0383 -0.0517 -0.0460

# include duplicate x
data2 <- data1
data2$age2 <- data2$age
data2$age3 <- data2$age
data2$age4 <- data2$age
data2$age5 <- data2$age
data2$age6 <- data2$age
data2$age7 <- data2$age
data2$age8 <- data2$age
data2$age9 <- data2$age
data2$age10 <- data2$age
data2$age11 <- data2$age
data2$age12 <- data2$age
data2$age13 <- data2$age
data2$age14 <- data2$age
data2$age15 <- data2$age
data2$age16 <- data2$age
data2$age17 <- data2$age
data2$age18 <- data2$age
data2$age19 <- data2$age
data2$age20 <- data2$age
data2$age21 <- data2$age
data2$age22 <- data2$age
data2$age23 <- data2$age
data2$age24 <- data2$age
data2$age25 <- data2$age

# impute factor by optimizing canonical correlation y, x
test_that("cancor proceeds normally with many duplicates", {
  expect_warning(imp2 <- mice(data2, method = "pmm", remove.collinear = FALSE, eps = 0,
                             maxit = 1, m = 1, seed = 1, print = FALSE))
})

# add junk variables
data3 <- data1
data3$j1 <- rnorm(nrow(data3))
data3$j2 <- rnorm(nrow(data3))
data3$j3 <- rnorm(nrow(data3))
data3$j4 <- rnorm(nrow(data3))
data3$j5 <- rnorm(nrow(data3))
data3$j6 <- rnorm(nrow(data3))
data3$j7 <- rnorm(nrow(data3))
data3$j8 <- rnorm(nrow(data3))
data3$j9 <- rnorm(nrow(data3))
data3$j10 <- rnorm(nrow(data3))
data3$j11 <- rnorm(nrow(data3))
data3$j12 <- rnorm(nrow(data3))
data3$j13 <- rnorm(nrow(data3))
data3$j14 <- rnorm(nrow(data3))
data3$j15 <- rnorm(nrow(data3))
data3$j16 <- rnorm(nrow(data3))
data3$j17 <- rnorm(nrow(data3))
data3$j18 <- rnorm(nrow(data3))
data3$j19 <- rnorm(nrow(data3))
data3$j20 <- rnorm(nrow(data3))
data3$j21 <- rnorm(nrow(data3))
data3$j22 <- rnorm(nrow(data3))
data3$j23 <- rnorm(nrow(data3))
data3$j24 <- rnorm(nrow(data3))
data3$j25 <- rnorm(nrow(data3))


test_that("cancor with many junk variables does not crash", {
  expect_warning(imp3 <- mice(data3, method = "pmm", remove.collinear = FALSE, eps = 0,
                              maxit = 1, m = 1, seed = 1, print = FALSE))
})
