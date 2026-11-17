d <- brandsma[1:200, c("sch", "lpo")]
pred <- make.predictorMatrix(d)
pred["lpo", "sch"] <- -2

test_that("MICE-IMPUTE-2L-LMER-001: mice::mice.impute.2l.lmer() runs empty model", {
  expect_silent(
    imp <- mice(
      d,
      method = "2l.lmer",
      print = FALSE,
      pred = pred,
      m = 1,
      maxit = 1
    )
  )
  expect_false(anyNA(complete(imp)))
})

d <- brandsma[1:200, c("sch", "lpo", "iqv")]
d[c(1, 11, 21), "iqv"] <- NA
pred <- make.predictorMatrix(d)
pred[c("lpo", "iqv"), "sch"] <- -2

test_that("MICE-IMPUTE-2L-LMER-002: 2l.lmer() runs random intercept, one predictor", {
  expect_silent(
    imp <- mice(
      d,
      method = "2l.lmer",
      print = FALSE,
      pred = pred,
      m = 1,
      maxit = 1
    )
  )
  expect_false(anyNA(complete(imp)))
})

test_that("MICE-IMPUTE-2L-LMER-003: global mice() arguments do not break lmer (#792)", {
  set.seed(1)
  n <- 600
  cl <- rep(1:6, each = 100)
  x1 <- rnorm(n)
  y <- 2 + x1 + rnorm(6)[cl] + rnorm(n)
  y[sample(n, 150)] <- NA
  y[cl == 6] <- NA
  d <- data.frame(y, x1, cl)
  pred <- make.predictorMatrix(d)
  pred["y", "cl"] <- -2
  pred[c("x1", "cl"), ] <- 0
  meth <- c(y = "2l.lmer", x1 = "", cl = "")
  expect_silent(
    imp <- mice(
      d,
      method = meth,
      predictorMatrix = pred,
      m = 1,
      maxit = 3,
      seed = 1,
      printFlag = FALSE,
      ridge = 1e-5
    )
  )
  expect_gt(length(unique(imp$chainMean["y", , 1])), 1L)
})

test_that("MICE-IMPUTE-2L-LMER-004: fallback warning names the lmer error", {
  y <- c(1, 2, NA, 4)
  ry <- !is.na(y)
  x <- matrix(1:4, ncol = 1, dimnames = list(NULL, "cl"))
  expect_warning(
    mice.impute.2l.lmer(y, ry, x, type = c(cl = -2)),
    "lmer does not run \\(.+\\)"
  )
})
