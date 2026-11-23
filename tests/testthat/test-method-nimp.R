impute <- function(data, ...) {
  mice(data, m = 2, maxit = 2, seed = 1, print = FALSE, ...)
}

test_that("METHOD-NIMP-001: a method for a complete variable is kept, not called (#602)", {
  meth <- make.method(nhanes, where = matrix(TRUE, nrow(nhanes), ncol(nhanes)))
  expect_identical(unname(meth["age"]), "pmm")
  imp <- impute(nhanes, method = meth)
  expect_identical(imp$method, meth)
  expect_identical(nrow(imp$imp$age), 0L)
  expect_identical(imp$imp, impute(nhanes)$imp)
})

test_that("METHOD-NIMP-002: a single method string is kept for complete variables", {
  imp <- impute(nhanes, method = "pmm")
  expect_identical(unname(imp$method), rep("pmm", 4L))
  expect_identical(imp$imp, impute(nhanes)$imp)
})

test_that("METHOD-NIMP-003: methods that fail without cells to impute are not called", {
  meth <- c(age = "polr", bmi = "pmm", hyp = "logreg", chl = "pmm")
  imp <- expect_silent(impute(nhanes2, method = meth))
  expect_identical(imp$method, meth)
  expect_identical(nrow(imp$imp$age), 0L)
  skip_if_not_installed("ranger")
  expect_silent(impute(nhanes2, method = "rf"))
})

test_that("METHOD-NIMP-004: an explicit empty method still skips an incomplete variable", {
  meth <- c(age = "", bmi = "", hyp = "pmm", chl = "pmm")
  imp <- impute(nhanes, method = meth)
  expect_identical(imp$method, meth)
  expect_true(all(is.na(imp$imp$bmi)))
  expect_identical(sum(is.na(complete(imp)$bmi)), sum(is.na(nhanes$bmi)))
})

test_that("METHOD-NIMP-005: a missing cluster outside where does not stop", {
  d <- brandsma[1:400, c("sch", "lpo", "iqv")]
  d$iqv[is.na(d$iqv)] <- mean(d$iqv, na.rm = TRUE)
  d$sch[1:5] <- NA
  pred <- make.predictorMatrix(d)
  pred[, ] <- 0
  pred["lpo", c("sch", "iqv")] <- c(-2, 1)
  where <- make.where(d)
  where[, "sch"] <- FALSE
  meth <- c(sch = "pmm", lpo = "2l.norm", iqv = "")
  imp <- suppressWarnings(
    impute(d, method = meth, predictorMatrix = pred, where = where)
  )
  expect_identical(imp$method, meth)
  expect_true(any(imp$loggedEvents$dep == "sch"))
  expect_error(
    impute(d, method = meth, predictorMatrix = pred),
    "cannot impute cluster membership"
  )
})
