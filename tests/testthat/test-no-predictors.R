test_that("NO-PREDICTORS-001: quickpred() selects predictors for complete variables (#602)", {
  pred <- quickpred(nhanes)
  expect_gt(sum(pred["age", ]), 0)
  expect_identical(unname(pred["age", "age"]), 0)
})

test_that("NO-PREDICTORS-002: a zero row of an imputed variable is logged", {
  pred <- make.predictorMatrix(nhanes)
  pred["age", ] <- 0
  where <- make.where(nhanes, "all")
  expect_warning(
    imp <- mice(
      nhanes,
      where = where,
      predictorMatrix = pred,
      m = 1,
      maxit = 1,
      seed = 1,
      print = FALSE
    ),
    "logged events"
  )
  ev <- imp$loggedEvents
  hit <- ev[ev$dep == "age", ]
  expect_identical(nrow(hit), 1L)
  expect_identical(hit$meth, "pmm")
  expect_identical(hit$it, 0)
  expect_match(hit$out, "intercept-only")
})

test_that("NO-PREDICTORS-003: zero rows of variables that are not imputed are not logged", {
  pred <- quickpred(nhanes)
  pred["age", ] <- 0
  imp <- mice(nhanes, predictorMatrix = pred, m = 1, maxit = 1, print = FALSE)
  expect_null(imp$loggedEvents)
})
