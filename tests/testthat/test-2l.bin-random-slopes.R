test_that("2l.bin supports random slopes in all random-effects modes", {
  skip_if_not_installed("lme4")
  set.seed(2026)
  cid <- rep(1:30, each = 30)
  x <- rnorm(length(cid))
  u <- rnorm(30, sd = 1.2)
  v <- rnorm(30, sd = 0.8)
  y <- rbinom(length(cid), 1, plogis(0.2 + 0.4 * x + u[cid] + v[cid] * x))
  y[c(1, 2, 31, 61, 91)] <- NA
  predictors <- data.frame(cid, x)
  ry <- !is.na(y)

  for (mode in c("laplace", "eb", "marginal")) {
    imputed <- mice.impute.2l.bin(
      y, ry, predictors, type = c(cid = -2, x = 2), random.effects = mode
    )
    expect_length(imputed, sum(!ry))
    expect_true(all(imputed %in% 0:1))
  }

  # A single cluster requiring imputation must also remain a row of B.
  wy <- !ry & cid == 1
  imputed <- mice.impute.2l.bin(
    y, ry, predictors, type = c(cid = -2, x = 2), wy = wy
  )
  expect_length(imputed, sum(wy))
  expect_true(all(imputed %in% 0:1))

  # The q = 1 path still works.
  imputed <- mice.impute.2l.bin(y, ry, predictors, type = c(cid = -2, x = 1))
  expect_length(imputed, sum(!ry))
  expect_true(all(imputed %in% 0:1))
})

test_that("mice imputes binary factors with a random slope", {
  skip_if_not_installed("lme4")
  set.seed(2026)
  cid <- rep(1:30, each = 30)
  x <- rnorm(length(cid))
  u <- rnorm(30, sd = 1.2)
  v <- rnorm(30, sd = 0.8)
  y <- factor(rbinom(length(cid), 1, plogis(0.2 + 0.4 * x + u[cid] + v[cid] * x)))
  y[c(1, 2, 31, 61, 91)] <- NA
  d <- data.frame(cid, x, y)
  pred <- make.predictorMatrix(d)
  pred["y", c("cid", "x")] <- c(-2, 2)
  imp <- mice(d, method = c("", "", "2l.bin"), predictorMatrix = pred,
              m = 1, maxit = 1, printFlag = FALSE, seed = 1)
  expect_false(anyNA(complete(imp)))
  expect_identical(levels(complete(imp)$y), levels(y))
})
