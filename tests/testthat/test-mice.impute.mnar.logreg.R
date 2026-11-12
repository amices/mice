set.seed(1)
n <- 200
x <- cbind(x1 = rnorm(n))
y <- factor(ifelse(x[, 1] + rnorm(n) > 0, "yes", "no"))
y[sample(n, 50)] <- NA
ry <- !is.na(y)

test_that("MICE-IMPUTE-MNAR-LOGREG-001: Requires an unidentifiable model specification", {
  expect_error(mice.impute.mnar.logreg(y, ry, x), "ums")
})

test_that("MICE-IMPUTE-MNAR-LOGREG-002: Returns a factor of requested length", {
  imp <- mice.impute.mnar.logreg(y, ry, x, ums = "0")
  expect_s3_class(imp, "factor")
  expect_identical(levels(imp), levels(y))
  expect_length(imp, sum(!ry))
})

test_that("MICE-IMPUTE-MNAR-LOGREG-003: A large positive ums pushes imputations to the second level", {
  set.seed(1)
  lo <- mice.impute.mnar.logreg(y, ry, x, ums = "-20")
  set.seed(1)
  hi <- mice.impute.mnar.logreg(y, ry, x, ums = "20")
  expect_true(all(lo == "no"))
  expect_true(all(hi == "yes"))
})

test_that("MICE-IMPUTE-MNAR-LOGREG-004: mice() runs with method mnar.logreg", {
  blot <- list(X = list(ums = "-4"))
  imp <- mice(
    mnar_demo_data,
    method = c("mnar.logreg", "pmm", ""),
    blots = blot,
    m = 2,
    maxit = 2,
    seed = 1,
    print = FALSE
  )
  cmp <- complete(imp)
  expect_false(anyNA(cmp))
  expect_s3_class(cmp$X, "factor")
})
