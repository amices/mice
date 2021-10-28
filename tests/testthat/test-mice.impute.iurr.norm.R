context("mice.impute.iurr.norm")

#########################
# TEST 1: Simple problem #
#########################
set.seed(123)

# generate data
n <- 1e3
y <- rnorm(n)
x <- y * .3 + rnorm(n, 0, .25)
x2 <- x + rnorm(n, 2, 3)
x <- cbind(x, x2)

# make missingness
y[sample(1:n, n*.3)] <- NA
ry <- !is.na(y)
wy <- !ry

# Use univariate imputation model
set.seed(123)
imps_t1 <- mice.impute.iurr.norm(y, ry, x)

test_that("Returns requested length", {
  expect_equal(length(imps_t1), sum(!ry))
})

#########################
# TEST 2: Nothing is important #
#########################

n <- 1e2
p <- 10
b0 <- 100
bs <- rep(0, p)
x <- matrix(rnorm(n * p), n, p)
y <- b0 + x %*% bs + rnorm(n)

# Missing values
y[sample(1:n, n*.3)] <- NA
ry <- !is.na(y)
wy <- !ry

# Use univariate imputation model
set.seed(123)
imps_t2 <- mice.impute.iurr.norm(y, ry, x)

test_that("Works for intercept only model", {
  expect_equal(class(imps_t2), "numeric")
})

#########################
# TEST 3: Everything is important #
#########################

n <- 1e2
p <- 10
b0 <- 100
bs <- rep(1, p)
x <- matrix(rnorm(n * p), n, p)
y <- b0 + x %*% bs + rnorm(n)

# Missing values
y[sample(1:n, n*.3)] <- NA
ry <- !is.na(y)
wy <- !ry

# Use univariate imputation model
set.seed(123)
imps_t3 <- mice.impute.iurr.norm(y, ry, x)

test_that("Works when all predictors are important", {
  expect_equal(class(imps_t3), "numeric")
})

#########################
# TEST 4: Use it within mice call #
#########################

boys_cont <- boys[, 1:4]
iurr_default <- mice(boys_cont, m = 2, maxit = 2, method = "iurr.norm", eps = 0)
iurr_custom <- mice(boys_cont, m = 2, maxit = 2, method = "iurr.norm", eps = 0,
                    nfolds = 5)

test_that("mice call works", {
  expect_equal(class(iurr_custom), "mids")
})

test_that("mice call works w/ custom arguments", {
  expect_equal(class(iurr_custom), "mids")
})