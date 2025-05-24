context("mice.impute.mpmm")

set.seed(1)
beta2 <- beta1 <- .5
x <- rnorm(1000)
e <- rnorm(1000, 0, 1)
y <- beta1 * x + beta2 * x^2 + e
# dat <- data.frame(x = x, x2 = x^2, y = y)  # worked
dat <- data.frame(y = y, x = x, x2 = x^2)  # did not work
m <- as.logical(rbinom(1000, 1, 0.25))
dat[m, c("x", "x2")] <- NA
blk <- list("y", c("x", "x2"))
meth <- c("", "mpmm")
imp <- mice(dat, blocks = blk, method = meth, print = FALSE, m = 1, maxit = 1)

test_that("mpmm() works for any column order in data", {
  expect_identical(complete(imp)$x^2, complete(imp)$x2)
})


