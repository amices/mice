context("mice.impute.2l.lmer")

d <- brandsma[1:200, c("sch", "lpo")]
pred <- make.predictorMatrix(d)
pred["lpo", "sch"] <- -2

test_that("mice::mice.impute.2l.lmer() runs empty model", {
  expect_silent(imp <- mice(d, method = "2l.lmer", print = FALSE, pred = pred, m = 1, maxit = 1))
  expect_false(anyNA(complete(imp)))
})

d <- brandsma[1:200, c("sch", "lpo", "iqv")]
d[c(1, 11, 21), "iqv"] <- NA
pred <- make.predictorMatrix(d)
pred[c("lpo", "iqv"), "sch"] <- -2

test_that("2l.lmer() runs random intercept, one predictor", {
  expect_silent(imp <- mice(d, method = "2l.lmer", print = FALSE, pred = pred, m = 1, maxit = 1))
  expect_false(anyNA(complete(imp)))
})
