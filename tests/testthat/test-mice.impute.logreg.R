context("test-mice.impute.logreg.R")

#########################
# TEST 1: Perfect Prediction / Complete Separation #
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
    mice.impute.logreg(y = y, ry = ry, x = x[, -1])
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
    mice.impute.logreg(y = y, ry = ry, x = x)
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
