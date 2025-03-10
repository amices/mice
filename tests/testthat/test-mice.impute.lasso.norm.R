context("mice.impute.lasso.norm")

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
y[sample(1:n, n * .3)] <- NA
ry <- !is.na(y)

# Use univariate imputation model
set.seed(123)
imps <- mice.impute.lasso.norm(y, ry, x)

test_that("Returns requested length", {
  expect_equal(length(imps), sum(!ry))
})


#########################
# TEST 2: Use it within mice call #
#########################

boys_cont <- boys[, 1:4]
durr_default <- mice(boys_cont,
                     m = 2, maxit = 2, method = "lasso.norm",
                     print = FALSE
)
durr_custom <- mice(boys_cont,
                    m = 2, maxit = 2, method = "lasso.norm",
                    nfolds = 5,
                    print = FALSE
)

test_that("mice call works", {
  expect_equal(class(durr_custom), "mids")
})

test_that("mice call works w/ custom arguments", {
  expect_equal(class(durr_custom), "mids")
})

#########################
# TEST 3: Failure because removals by remove.lindep()  #577
#########################

# library(mice, warn.conflicts = FALSE)
# set.seed(123)
#
# n <- 100
# y <- rnorm(n)
# x <- rep(1, n)
# y[sample(1:n, n * .3)] <- NA
# ry <- !is.na(y)
#
# # Test univariate imputation outside mice
# imps <- mice.impute.lasso.norm(y, ry, as.matrix(x))
# imps <- mice.impute.lasso.norm(y, ry, as.matrix(x)[, -1])
#
# # Test inside mice
# input <- data.frame(y = y, x = 1)
# imp <- mice(input, m = 1, maxit = 1, method = "lasso.norm", print = FALSE)
# imp <- mice(input, m = 1, maxit = 1, method = "lasso.norm", print = FALSE, eps = 0)
# imp <- mice(input, m = 1, maxit = 1, method = "lasso.norm", print = FALSE, eps = 0, remove.constant = FALSE)
#
# test_that("skip remove.lindep()", {
#   expect_silent(mice(data, m = 1, maxit = 1, method = "lasso.norm", eps = 0, print = FALSE))
# })
#

context("mice.impute.lasso.select.norm")

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
y[sample(1:n, n * .3)] <- NA
ry <- !is.na(y)
wy <- !ry

# Use univariate imputation model
set.seed(123)
imps_t1 <- mice.impute.lasso.select.norm(y, ry, x)

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
y[sample(1:n, n * .3)] <- NA
ry <- !is.na(y)
wy <- !ry

# Use univariate imputation model
set.seed(123)
imps_t2 <- mice.impute.lasso.select.norm(y, ry, x)

test_that("Returns requested length w/ intercept only model", {
  expect_equal(length(imps_t2), sum(!ry))
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
y[sample(1:n, n * .3)] <- NA
ry <- !is.na(y)
wy <- !ry

# Use univariate imputation model
set.seed(123)
imps_t3 <- mice.impute.lasso.select.norm(y, ry, x)

test_that("Returns requested length when all predictors are important", {
  expect_equal(length(imps_t3), sum(!ry))
})

#########################
# TEST 4: Use it within mice call #
#########################

boys_cont <- boys[, 1:4]
iurr_default <- mice(boys_cont,
                     m = 2, maxit = 2, method = "lasso.select.norm",
                     print = FALSE
)
iurr_custom <- mice(boys_cont,
                    m = 2, maxit = 2, method = "lasso.select.norm",
                    nfolds = 5,
                    print = FALSE
)

test_that("mice call works", {
  expect_equal(class(iurr_custom), "mids")
})

test_that("mice call works w/ custom arguments", {
  expect_equal(class(iurr_custom), "mids")
})
