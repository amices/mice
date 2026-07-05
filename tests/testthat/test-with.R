# See #292: with.mids() using eval_tidy() breaks compatibility with metafor

test_that("WITH-001: does not break metafor package (#292)", {
  skip_if_not_installed("metafor")
  library(metafor)
  dat <- dat.bcg
  dat <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat)
  dat$ablat[c(2, 4, 8)] <- NA

  predMatrix <- make.predictorMatrix(dat)
  predMatrix[, ] <- 0
  predMatrix["ablat", c("yi", "year")] <- 1
  impMethod <- make.method(dat)
  impMethod["ablat"] <- "pmm"

  imp <- mice(dat, print = FALSE, predictorMatrix = predMatrix,
              method = impMethod, seed = 1234)
  expect_silent(fit <- with(imp, rma(yi, vi, mods = ~ ablat + year)))
  expect_true(is.mira(fit))
})
