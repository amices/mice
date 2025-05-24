context("mice.impute.2lonly.mean")

set.seed(66322)
y <- popmis$texp
y[rbinom(length(y), size = 1, prob = 0.5) == 1] <- NA
x <- popmis[, c("pupil", "school", "sex")]
ry <- !is.na(y)
wy1 <- !ry
wy2 <- rep(TRUE, length(y))
wy3 <- rep(FALSE, length(y))
wy4 <- rep(c(TRUE, FALSE), times = c(1, length(y) - 1))
type <- c(1, -2, 1)
yn <- y

test_that("Returns requested length, for numeric", {
  expect_equal(length(mice.impute.2lonly.mean(y, ry, x, type, wy1)), sum(wy1))
  expect_equal(length(mice.impute.2lonly.mean(y, ry, x, type, wy2)), sum(wy2))
  expect_equal(length(mice.impute.2lonly.mean(y, ry, x, type, wy3)), sum(wy3))
  expect_equal(length(mice.impute.2lonly.mean(y, ry, x, type, wy4)), sum(wy4))
})

# test extension to factors
set.seed(66322)
y <- popmis$texp
y <- cut(y, breaks = c(0, 5, 10, 20, 30))
y[rbinom(length(y), size = 1, prob = 0.5) == 1] <- NA

test_that("Returns requested length, for factor", {
  expect_equal(length(mice.impute.2lonly.mean(y, ry, x, type, wy1)), sum(wy1))
  expect_equal(length(mice.impute.2lonly.mean(y, ry, x, type, wy2)), sum(wy2))
  expect_equal(length(mice.impute.2lonly.mean(y, ry, x, type, wy3)), sum(wy3))
  expect_equal(length(mice.impute.2lonly.mean(y, ry, x, type, wy4)), sum(wy4))
})

# check whether imputes for numeric and factor are identical
# tn <- mice.impute.2lonly.mean(yn, ry, x, type, wy1)
# tf <- mice.impute.2lonly.mean(y, ry, x, type, wy1)

# check what happens if all values within a class are missing
yn[1:100] <- NA
imn <- mice.impute.2lonly.mean(yn, ry, x, type, wy1)
zn <- table(imn, useNA = "al")

y[1:100] <- NA
imf <- mice.impute.2lonly.mean(y, ry, x, type, wy1)
zf <- table(imf, useNA = "al")

test_that("Return NA for classes without values", {
  expect_equal(as.numeric(zn[length(zn)]), 39)
  expect_equal(as.numeric(zf[length(zf)]), 39)
})
