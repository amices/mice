skip_if_not_installed("MASS")

set.seed(1)
n <- 100
x <- cbind(x1 = rnorm(n), x2 = rnorm(n))
y <- factor(c("a", "b", "c")[1 + (x[, 1] + rnorm(n) > 0) + (x[, 2] > 1)])
y[sample(n, 15)] <- NA
ry <- !is.na(y)
wy2 <- rep(TRUE, n)

test_that("MICE-IMPUTE-LDA-001: Returns requested length", {
  expect_length(mice.impute.lda(y, ry, x), sum(!ry))
  expect_length(mice.impute.lda(y, ry, x, wy = wy2), sum(wy2))
})

test_that("MICE-IMPUTE-LDA-002: Imputations are level labels of y", {
  imp <- mice.impute.lda(y, ry, x)
  expect_type(imp, "character")
  expect_true(all(imp %in% levels(y)))
  expect_false(anyNA(imp))
})

test_that("MICE-IMPUTE-LDA-003: mice() runs with method lda for a factor", {
  imp <- mice(
    nhanes2,
    method = c("", "pmm", "lda", "pmm"),
    m = 2,
    maxit = 2,
    seed = 1,
    print = FALSE
  )
  cmp <- complete(imp)
  expect_false(anyNA(cmp))
  expect_s3_class(cmp$hyp, "factor")
  expect_identical(levels(cmp$hyp), levels(nhanes2$hyp))
})
