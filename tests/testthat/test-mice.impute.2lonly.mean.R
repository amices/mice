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

test_that("Returns requested length", {
  expect_equal(length(mice.impute.2lonly.mean(y, ry, x, type, wy1)), sum(wy1))
  expect_equal(length(mice.impute.2lonly.mean(y, ry, x, type, wy2)), sum(wy2))
  expect_equal(length(mice.impute.2lonly.mean(y, ry, x, type, wy3)), sum(wy3))
  expect_equal(length(mice.impute.2lonly.mean(y, ry, x, type, wy4)), sum(wy4))
})
