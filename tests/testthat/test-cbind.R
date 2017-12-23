context("cbind.mids")

data <- nhanes
data1 <- data[, c("age", "bmi")]
data2 <- data[, c("hyp", "chl")]

imp1 <- mice(data1, m = 2, maxit = 1, print = FALSE)
imp2 <- mice(data2, m = 2, maxit = 1, print = FALSE)
imp <- cbind(imp1, imp2)

test_that("combines imputations", {
  expect_identical(ncol(complete(imp)), 4L)
  expect_identical(complete(imp1), complete(imp)[, c("age", "bmi")])
})

# when using blocks
data <- nhanes
data1 <- data[, c("age", "bmi")]
data2 <- data[, c("hyp", "chl")]

imp1 <- mice(data1, m = 1, maxit = 1, print = FALSE)
imp2 <- mice(data2, blocks = list(c("hyp", "chl")), m = 1, maxit = 1, print = FALSE)
imp <- cbind(imp1, imp2)

test_that("combines imputations with blocks", {
  expect_identical(ncol(complete(imp)), 4L)
  expect_identical(complete(imp1), complete(imp)[, c("age", "bmi")])
})

# handling of duplicate variable names
data <- nhanes
data1 <- data[, c("age", "bmi", "hyp")]
data2 <- data[, c("hyp", "chl")]

imp1 <- mice(data1, m = 1, maxit = 1, print = FALSE)
imp2 <- mice(data2, m = 1, maxit = 1, print = FALSE)
imp <- cbind(imp1, imp2)
mice.mids(imp, max = 2)

# imp0 <- mice(data, m = 1, maxit = 1, print = FALSE)
# imp0.2 <- mice.mids(imp0, max = 2)

# handling of duplicate blocks names


# imp3 <- mice(nhanes, blocks = list(c("bmi", "chl"), "hyp"), print = FALSE)
# agevar <- nhanes$age
# agevar[1:5] <- NA
# imp3a <- mice:::cbind.mids(imp3, data.frame(myage = agevar))
