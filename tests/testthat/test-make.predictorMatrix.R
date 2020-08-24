context("make.predictorMatrix")

blocks1 <- make.blocks(c("bmi", "chl", "hyp", "age"))

test_that("errors on invalid data arguments", {
  expect_error(
    make.predictorMatrix(data, blocks = blocks1),
    "Data should be a matrix or data frame"
  )
})
