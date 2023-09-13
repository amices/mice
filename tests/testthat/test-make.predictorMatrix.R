context("make.predictorMatrix")

blocks1 <- make.blocks(c("bmi", "chl", "hyp", "age"))

test_that("errors on invalid data arguments", {
  expect_error(
    make.predictorMatrix(data, blocks = blocks1),
    "Data should be a matrix or data frame"
  )
})

# put all incomplete covariate into one blocks, and
# test whether predictorMatrix has zero rows for
# those covariates
data <- cbind(mice::nhanes2, covariate = c(1, rep(c(1, 2), 12)))
imp <- mice(data, blocks = list("bmi", c("age", "covariate"), "chl"), print = FALSE)
test_that("complete variables in a block will get zero rows", {
  expect_identical(unname(imp$predictorMatrix["age", ]), rep(0, 5))
  expect_identical(unname(imp$predictorMatrix["covariate", ]), rep(0, 5))
})
