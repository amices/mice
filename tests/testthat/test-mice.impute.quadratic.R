set.seed(1)
n <- 200
X <- rnorm(n)
Y <- 0.5 * X + 0.5 * X^2 + rnorm(n)
dat <- data.frame(x = X, xx = X^2, y = Y)
dat[rbinom(n, 1, 0.25) == 1, 1:2] <- NA

ry <- !is.na(dat$x)
xmat <- cbind(y = dat$y)

test_that("MICE-IMPUTE-QUADRATIC-001: Requires a valid quad.outcome", {
  expect_error(mice.impute.quadratic(dat$x, ry, xmat), "quad.outcome")
  expect_error(
    mice.impute.quadratic(dat$x, ry, xmat, quad.outcome = "z"),
    "can not be found"
  )
})

test_that("MICE-IMPUTE-QUADRATIC-002: Returns requested length", {
  expect_length(
    mice.impute.quadratic(dat$x, ry, xmat, quad.outcome = "y"),
    sum(!ry)
  )
})

test_that("MICE-IMPUTE-QUADRATIC-003: mice() keeps the square consistent through passive imputation", {
  ini <- mice(dat, maxit = 0, print = FALSE)
  pred <- ini$pred
  pred[, "xx"] <- 0
  imp <- mice(
    dat,
    method = c("quadratic", "~I(x^2)", ""),
    pred = pred,
    quad.outcome = "y",
    m = 2,
    maxit = 2,
    seed = 1,
    print = FALSE
  )
  cmp <- complete(imp)
  expect_false(anyNA(cmp))
  expect_equal(cmp$xx, cmp$x^2)
})
