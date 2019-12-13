context("update.design")

data <- nhanes2
design <- mice:::obtain.design(data)
d1 <- mice:::update.design(design, data, varname = "age")
d2 <- mice:::update.design(design, data, varname = "bmi")
d3 <- mice:::update.design(design, data, varname = "hyp")
d4 <- mice:::update.design(design, data, varname = ".")
d5 <- mice:::update.design(design, data, varname = "xxxx")
d6 <- mice:::update.design(design, data, varname = "")
d7 <- mice:::update.design(design, data, varname = c("xxx", "age"))

test_that("updates itself", {
  expect_identical(design, d1)
  expect_identical(design, d2)
  expect_identical(design, d3)
  expect_identical(design, d4)
  expect_identical(design, d5)
  expect_identical(design, d6)
  expect_identical(design, d7)
})

data <- nhanes2
design <- mice:::obtain.design(data)
data$age[1:4] <- data$age[4:1]
data$bmi[1:4] <- data$bmi[4:1]
data$hyp[1:4] <- data$hyp[4:1]
d1 <- mice:::update.design(design, data, varname = "age")
d2 <- mice:::update.design(design, data, varname = "bmi")
d3 <- mice:::update.design(design, data, varname = ".")

test_that("caries through selected row reversals", {
  expect_identical(as.vector(design[1:4, 2:3]), 
                   as.vector(d1[4:1, 2:3]))
  expect_identical(as.vector(design[1:4, 4]), 
                   as.vector(d2[4:1, 4]))
  expect_identical(as.vector(design[1:4, 2:5]), 
                   as.vector(d3[4:1, 2:5]))
})
