d1 <- brandsma[1:200, c("sch", "lpo")]
pred <- make.predictorMatrix(d1)
pred["lpo", "sch"] <- -2

test_that("MICE-IMPUTE-2L-NORM-001: mice::mice.impute.2l.norm() runs empty model", {
  expect_silent(
    imp <- mice(
      d1,
      method = "2l.norm",
      print = FALSE,
      pred = pred,
      m = 1,
      maxit = 1
    )
  )
  expect_false(anyNA(complete(imp)))
})
