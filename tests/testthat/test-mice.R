context("complete")

nhanes_mids <- mice(nhanes, m = 2, print = FALSE)
nhanes_complete <- complete(nhanes_mids)

test_that("No missing values remain in imputed nhanes data set", {
  expect_true(sum(is.na(nhanes)) > 0)
  expect_true(sum(is.na(nhanes_complete)) == 0)
})
