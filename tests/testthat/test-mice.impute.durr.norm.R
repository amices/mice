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
