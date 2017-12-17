context("cbind.mids")

data(nhanes)
data1 <- nhanes[, c("age", "bmi")]
data2 <- nhanes[, c("hyp", "chl")]

imp1 <- mice(data1, m = 2, maxit = 1, print = FALSE)
imp2 <- mice(data2, m = 2, maxit = 1, print = FALSE)

test_that("Combines into correct number of columns", {
  expect_equal(ncol(complete(cbind(imp1, imp2))), 4)
})

imp3 <- mice(nhanes, blocks = list(c("bmi", "chl"), "hyp"), print = FALSE)
agevar <- nhanes$age
agevar[1:5] <- NA
imp3a <- mice:::cbind.mids(imp3, data.frame(myage = agevar))
