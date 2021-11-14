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
y <- as.numeric(cut(y, 2)) - 1

# make missingness
y[sample(1:n, n * .3)] <- NA
ry <- !is.na(y)
wy <- !ry

# Use univariate imputation model
set.seed(123)
imps <- mice.impute.lasso.logreg(y, ry, x)

test_that("Returns a matrix of dimensionality sum(wy) x 1", {
  expect_equal(class(imps), c("matrix", "array"))
  expect_equal(dim(imps), c(sum(wy), 1))
})

#########################
# TEST 2: Use it within mice call #
#########################

# Generate some dichotomous data
n <- 1e2
p <- 4
Sigma <- matrix(.7, nrow = p, ncol = p)
diag(Sigma) <- 1
X <- as.data.frame(MASS::mvrnorm(n, rep(0, p), Sigma))

# Discretize and impose miss
for (j in 1:2) {
  X[, j] <- cut(X[, j], 2) # Make it discrete
  X[sample(1:n, n * .3), j] <- NA # Impose missings
}

# Imputations
durr_default <- mice(X,
  m = 2, maxit = 2, method = "lasso.logreg",
  print = FALSE
)
durr_custom <- mice(X,
  m = 2, maxit = 2, method = "lasso.logreg",
  nfolds = 5,
  print = FALSE
)
logreg_default <- mice(X,
  m = 2, maxit = 2, method = "logreg",
  print = FALSE
)

# Tests
test_that("mice call works", {
  expect_equal(class(durr_custom), "mids")
})

test_that("mice call works w/ custom arguments", {
  expect_equal(class(durr_custom), "mids")
})

test_that("same class as logreg default method", {
  expect_equal(
    class(complete(logreg_default)[, 1]),
    class(complete(durr_default)[, 1])
  )
})

#########################
# TEST 3: Perfect Prediction / Complete Separation #
#########################
set.seed(123)

# Generate some dichotomous data
n <- 1e2
p <- 4
Sigma <- matrix(.7, nrow = p, ncol = p)
diag(Sigma) <- 1
x <- MASS::mvrnorm(n, rep(0, p), Sigma)

# Create Perfect Predictor
y <- factor(x[, 1] < 0, labels = c("y", "n"))

# Missing values
y[sample(1:n, n * .3)] <- NA
ry <- !is.na(y)
wy <- !ry

# Imputation well behaved
wellBehaved <- tryCatch(
  expr = {
    mice.impute.lasso.logreg(y = y, ry = ry, x = x[, -1])
  },
  error = function(e) {
    e
  },
  warning = function(w) {
    w
  }
)

# Imputation perfect prediction
perfectPred <- tryCatch(
  expr = {
    mice.impute.lasso.logreg(y = y, ry = ry, x = x)
  },
  error = function(e) {
    e
  },
  warning = function(w) {
    w
  }
)

# Test
test_that("Complete separation results in same class as well behaved case", {
  expect_true(all.equal(class(wellBehaved), class(perfectPred)))
})
