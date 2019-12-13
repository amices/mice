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
impc <- mice.mids(imp, max = 2, print = FALSE)

test_that("duplicate variable adds a column", {
  expect_identical(ncol(complete(impc)), 5L)
})

# handling of duplicate blocks
imp1 <- mice(data1, blocks = list(c("age", "bmi"), "hyp"), m = 1, maxit = 1, print = FALSE)
imp2 <- mice(data2, blocks = list(c("hyp", "chl")), m = 1, maxit = 1, print = FALSE)
imp <- cbind(imp1, imp2)
impc <- mice.mids(imp, max = 2, print = FALSE)

test_that("duplicate blocks names renames block", {
  expect_identical(names(impc$blocks)[3], "B1.1")
})


# cbind - no second argument
imp1 <- mice(nhanes, blocks = list(c("bmi", "chl"), "hyp"), print = FALSE, maxit = 1, m = 1)
imp2 <- cbind(imp1)
imp3 <- cbind(imp1, NULL)
imp4 <- cbind(imp1, character(0))
test_that("returns imp1 object if there is nothing to bind", {
  expect_identical(imp2, imp1)
  expect_identical(imp3, imp1)
  expect_identical(imp4, imp1)
})

# cbind - unnamed constant
imp2 <- cbind(imp1, 1)
imp3 <- cbind(imp1, NA)
imp4 <- cbind(imp1, "male")
test_that("replicates unnamed constant", {
  expect_identical(ncol(complete(imp2)), 5L)
  expect_identical(ncol(complete(imp3)), 5L)
  expect_identical(ncol(complete(imp4)), 5L)
})

imp6 <- cbind(imp1, int = 51:75, out = 15, NA)
test_that("appends names vectors and constants", {
  expect_identical(ncol(complete(imp6)), 7L)
  expect_error(cbind(imp1, c(NA, 9)), 
               "arguments imply differing number of rows: 25, 2")
})

# matrix, factor, data.frame
# NOTE: cbind() dispatches to wrong function if there is a data.frame 
# so use cbind.mids()
imp8 <- mice:::cbind.mids(imp1, 
                          ma = matrix(1:50, nrow = 25, ncol = 2), 
                          age = nhanes2$age, 
                          df = nhanes2[, c("hyp", "chl")])
test_that("appends matrix, factor and data.frame", {
  expect_identical(ncol(complete(imp8)), 9L)
})
# impc <- mice.mids(imp8, max = 2, print = FALSE)


# NOTE: now using own version of cbind()
imp9 <- cbind(imp1, 
              ma = matrix(1:50, nrow = 25, ncol = 2), 
              age = nhanes2$age, 
              df = nhanes2[, c("hyp", "chl")])
test_that("appends matrix, factor and data.frame", {
  expect_identical(ncol(complete(imp9)), 9L)
})

impc <- mice.mids(imp9, max = 2, print = FALSE)
test_that("combined object works as input to mice.mids", {
  expect_true(is.mids(impc))
})

test_that("cbind does not throw a warning (#114)", {
  expect_silent(cbind(ordered(c(1,2))))
})

# # cbind data.frame (rename to age.1)
# imp1 <- mice(nhanes, blocks = list(c("bmi", "chl"), "hyp"), print = FALSE, maxit = 1, m = 1)
# agevar <- nhanes$age
# agevar[1:5] <- NA
# imp2 <- mice:::cbind.mids(imp1, data.frame(age = agevar, hyp = "test"))
# imp3 <- mice.mids(imp2, max = 2, print = FALSE)
# complete(imp3)
# 
# # cbind data.frame (use quoted name)
# imp1 <- mice(nhanes, blocks = list(c("bmi", "chl"), "hyp"), print = FALSE, maxit = 1, m = 1)
# agevar <- nhanes$age
# agevar[1:5] <- NA
# imp2 <- mice:::cbind.mids(imp1, age = agevar, hyp = "test")
# imp3 <- mice.mids(imp2, max = 2, print = FALSE)
# complete(imp3)
# 

