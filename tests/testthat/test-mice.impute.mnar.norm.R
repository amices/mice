set.seed(1)
n <- 100
x <- cbind(x1 = rnorm(n))
y <- x[, 1] + rnorm(n)
y[sample(n, 20)] <- NA
ry <- !is.na(y)

test_that("MICE-IMPUTE-MNAR-NORM-001: Requires an unidentifiable model specification", {
  expect_error(mice.impute.mnar.norm(y, ry, x), "ums")
  expect_error(mice.impute.mnar.norm(y, ry, x, ums = "2*x1"), "intercept")
})

test_that("MICE-IMPUTE-MNAR-NORM-002: Returns requested length and shifts by the ums", {
  set.seed(1)
  a <- mice.impute.mnar.norm(y, ry, x, ums = "0")
  set.seed(1)
  b <- mice.impute.mnar.norm(y, ry, x, ums = "10")
  expect_length(a, sum(!ry))
  expect_equal(as.vector(b - a), rep(10, sum(!ry)))
})

test_that("MICE-IMPUTE-MNAR-NORM-003: ums may refer to covariates and umx", {
  set.seed(1)
  a <- mice.impute.mnar.norm(y, ry, x, ums = "0")
  set.seed(1)
  b <- mice.impute.mnar.norm(y, ry, x, ums = "1+2*x1")
  expect_equal(as.vector(b - a), 1 + 2 * x[!ry, 1])
  aux <- cbind(even = rep(0:1, length.out = n))
  set.seed(1)
  d <- mice.impute.mnar.norm(y, ry, x, ums = "0+3*even", umx = aux)
  expect_equal(as.vector(d - a), 3 * aux[!ry, 1])
})

test_that("MICE-IMPUTE-MNAR-NORM-004: mice() runs the NARFCS example", {
  blot <- list(X = list(ums = "-4"), Y = list(ums = "2+1*ZCat1-3*ZCat2"))
  imp <- mice(
    mnar_demo_data,
    method = c("mnar.logreg", "mnar.norm", ""),
    blots = blot,
    m = 2,
    maxit = 2,
    seed = 234235,
    print = FALSE
  )
  expect_false(anyNA(complete(imp)))
})
