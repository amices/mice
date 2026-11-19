skip_if_not_installed("lme4")

# toenail: outcome is factor
data("toenail2")
data_factor <- tidyr::complete(toenail2, patientID, visit) |>
  tidyr::fill(treatment) |>
  dplyr::select(-time) |>
  dplyr::mutate(patientID = as.integer(patientID))
pred_factor <- make.predictorMatrix(data_factor)
pred_factor["outcome", "patientID"] <- -2

test_that("MICE-IMPUTE-2L-BIN-001: mice::mice.impute.2l.bin() accepts factor outcome", {
  expect_silent(
    imp <- mice(
      data_factor,
      method = "2l.bin",
      print = FALSE,
      pred = pred_factor,
      m = 1,
      maxit = 1
    )
  )
  expect_false(anyNA(complete(imp)))
})

# toenail: outcome is 0/1
data("toenail", package = "mice")
data_binary <- tidyr::complete(toenail, ID, visit) |>
  tidyr::fill(treatment) |>
  dplyr::select(-month)
pred_binary <- make.predictorMatrix(data_binary)
pred_binary["outcome", "ID"] <- -2

test_that("MICE-IMPUTE-2L-BIN-002: mice::mice.impute.2l.bin() accepts 0/1 outcome", {
  expect_silent(
    imp <- mice(
      data_binary,
      method = "2l.bin",
      print = FALSE,
      pred = pred_binary,
      m = 1,
      maxit = 1
    )
  )
  expect_false(anyNA(complete(imp)))
})

test_that("MICE-IMPUTE-2L-BIN-003: global mice() arguments do not break glmer (#792)", {
  set.seed(1)
  n <- 600
  cl <- rep(1:6, each = 100)
  x1 <- rnorm(n)
  b <- rbinom(n, 1, plogis(x1 + rnorm(6)[cl]))
  b[sample(n, 150)] <- NA
  d <- data.frame(b, x1, cl)
  pred <- make.predictorMatrix(d)
  pred["b", "cl"] <- -2
  pred[c("x1", "cl"), ] <- 0
  meth <- c(b = "2l.bin", x1 = "", cl = "")
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
  expect_gt(length(unique(imp$chainMean["b", , 1])), 1L)
})
