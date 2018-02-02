context("mice.impute.2l.lmer")

d1 <- brandsma[1:200, c("school", "langpost")]
pred <- make.predictorMatrix(d1)
pred["langpost", "school"] <- -2

test_that("mice::mice.impute.2l.lmer() runs empty model", {
  expect_silent(imp <- mice(d1, method = "2l.lmer", print = FALSE, pred = pred, m = 1, maxit = 1))
  expect_false(anyNA(complete(imp)))
})
