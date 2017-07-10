context("cbind.mids")

data(nhanes)
data1 <- nhanes[, c("age", "bmi")]
data2 <- nhanes[, c("hyp", "chl")]

imp1 <- mice(data1, m = 2, maxit = 1, print = FALSE)
imp2 <- mice(data2, m = 2, maxit = 1, print = FALSE)

test_that("Produces broader imputed dataset", {
  expect_equal(ncol(complete(cbind(imp1, imp2))), 4)
})
