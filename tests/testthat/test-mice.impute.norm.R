context("mice.impute.norm")

x <- airquality[, c("Wind", "Temp", "Month")]
y <- airquality[, "Ozone"]
ry <- !is.na(y)

# do imputations depend on x column order?
x1 <- x[, c(1, 2, 3)]
x2 <- x[, c(1, 3, 2)]

set.seed(123); pmm1 <- mice.impute.pmm(y, ry, x1)
set.seed(123); pmm2 <- mice.impute.pmm(y, ry, x2)
set.seed(123); norm1 <- mice.impute.norm(y, ry, x1)
set.seed(123); norm2 <- mice.impute.norm(y, ry, x2)
set.seed(123); norm.nob1 <- mice.impute.norm.nob(y, ry, x1)
set.seed(123); norm.nob2 <- mice.impute.norm.nob(y, ry, x2)
set.seed(123); norm.predict1 <- mice.impute.norm.predict(y, ry, x1)
set.seed(123); norm.predict2 <- mice.impute.norm.predict(y, ry, x2)
set.seed(123); norm.boot1 <- mice.impute.norm.boot(y, ry, x1)
set.seed(123); norm.boot2 <- mice.impute.norm.boot(y, ry, x2)

test_that("Imputations are invariant to column order", {
  # expect_equal(pmm1, pmm2)
  # expect_equal(norm1, norm2)
  expect_equal(norm.nob1, norm.nob2)
  expect_equal(norm.predict1, norm.predict2)
  expect_equal(norm.boot1, norm.boot2)
})
