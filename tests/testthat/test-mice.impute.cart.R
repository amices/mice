xname <- c("age", "hgt", "wgt")
br <- boys[c(1:10, 101:110, 501:510, 601:620, 701:710), ]
r <- stats::complete.cases(br[, xname])
x <- br[r, xname]
y <- br[r, "tv"]
ry <- !is.na(y)
wy2 <- rep(TRUE, length(y))
wy3 <- rep(FALSE, length(y))

set.seed(1)
n <- 100
xf <- cbind(x1 = rnorm(n), x2 = rnorm(n))
yf <- factor(c("a", "b", "c")[1 + (xf[, 1] + rnorm(n) > 0) + (xf[, 2] > 1)])
yf[sample(n, 15)] <- NA
ryf <- !is.na(yf)

test_that("MICE-IMPUTE-CART-001: Returns requested length", {
  expect_length(mice.impute.cart(y, ry, x), sum(!ry))
  expect_length(mice.impute.cart(y, ry, x, wy = wy2), sum(wy2))
  expect_length(mice.impute.cart(y, ry, x, wy = wy3), sum(wy3))
})

test_that("MICE-IMPUTE-CART-002: Numeric imputations are drawn from observed values", {
  imp <- mice.impute.cart(y, ry, x, wy = wy2)
  expect_true(all(imp %in% y[ry]))
})

test_that("MICE-IMPUTE-CART-003: Categorical imputations are level labels of y", {
  imp <- mice.impute.cart(yf, ryf, xf)
  expect_type(imp, "character")
  expect_true(all(imp %in% levels(yf)))
  expect_length(imp, sum(!ryf))
})

test_that("MICE-IMPUTE-CART-004: mice() runs with method cart on mixed data", {
  imp <- mice(
    nhanes2,
    method = "cart",
    minbucket = 4,
    m = 2,
    maxit = 2,
    seed = 1,
    print = FALSE
  )
  cmp <- complete(imp)
  expect_false(anyNA(cmp))
  expect_identical(sapply(cmp, class), sapply(nhanes2, class))
})
