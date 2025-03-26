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

## Check how logreg works with logical fills

set.seed(123)
df <- data.frame(
  factor2 = factor(sample(c("Yes", "No"), 20, replace = TRUE)),
  factor3 = factor(sample(c("Low", "Medium", "High"), 20, replace = TRUE)),
  factor4 = factor(sample(c("A", "B", "C", "D"), 20, replace = TRUE), ordered = TRUE),
  logical1 = sample(c(TRUE, FALSE), 20, replace = TRUE),
  logical2 = sample(c(TRUE, FALSE), 20, replace = TRUE),
  numeric1 = rnorm(20)
)
n <- prod(dim(df))
na_count <- round(0.3 * n)
missing_idx <- arrayInd(sample(n, na_count), .dim = dim(df))
for (i in seq_len(nrow(missing_idx))) {
  df[missing_idx[i, 1], missing_idx[i, 2]] <- NA
}

# Convert logicals to factors.
# This breaks consistency, so let's avoid this.
#df[] <- lapply(df, function(col) {
#  if (is.logical(col)) factor(col, levels = c(TRUE, FALSE)) else col
#})

expect_warning(trained <<- mice(df, m = 2, maxit = 2, seed = 1, tasks = "train", print = FALSE))

# now fill
newdata <- rbind(df[1:2, ], data.frame(
  factor2 = NA,
  factor3 = NA,
  factor4 = NA,
  logical1 = NA,
  logical2 = NA,
  numeric1 = NA
))

test_that("df and newdata have same types before fill", {
          expect_identical(sapply(df, class), sapply(newdata, class))
})

# fill0 <- mice(newdata, tasks = "fill", models = trained$models, m = 20, maxit = 0, print = FALSE, seed = 2)
# fill0$imp
#
# fill1 <- mice(newdata, tasks = "fill", models = trained$models, m = 20, maxit = 1, print = FALSE, seed = 2)
# fill1$imp

test_that("Filling logicals work without converting to factors", {
  expect_silent(filled <- mice(newdata, tasks = "fill", models = trained$models, print = FALSE))
  expect_identical(sapply(newdata, class), sapply(complete(filled), class))
})

