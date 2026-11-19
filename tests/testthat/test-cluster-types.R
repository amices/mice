d0 <- brandsma[1:400, c("sch", "lpo", "iqv")]
d0$iqv[is.na(d0$iqv)] <- mean(d0$iqv, na.rm = TRUE)
d0$sch <- as.integer(factor(d0$sch))
pred <- make.predictorMatrix(d0)
pred[,] <- 0
pred["lpo", c("sch", "iqv")] <- c(-2, 1)

impute <- function(data, method, ...) {
  meth <- c(sch = "", lpo = method, iqv = "")
  mice(
    data,
    method = meth,
    predictorMatrix = pred,
    m = 2,
    maxit = 2,
    seed = 1,
    print = FALSE,
    ...
  )
}

has.lme4 <- requireNamespace("lme4", quietly = TRUE)
has.pan <- requireNamespace("pan", quietly = TRUE)

d1 <- d0
d1$sch <- factor(d1$sch, labels = paste0("S", sort(unique(d0$sch))))

test_that("CLUSTER-TYPES-001: factor clusters give the same imputations as integers (#657)", {
  for (m in c("2l.norm", if (has.lme4) "2l.lmer", if (has.pan) "2l.pan")) {
    i1 <- impute(d0, m)
    i2 <- impute(d1, m)
    # equal, not identical: BLAS (e.g. MKL) may differ in the last bits
    expect_equal(i1$imp$lpo, i2$imp$lpo, info = m)
    expect_s3_class(complete(i2)$sch, "factor")
  }
})

test_that("CLUSTER-TYPES-002: unused factor levels do not matter", {
  d3 <- d1
  d3$sch <- factor(d3$sch, levels = c("S0", levels(d1$sch), "S99"))
  expect_identical(impute(d0, "2l.norm")$imp$lpo, impute(d3, "2l.norm")$imp$lpo)
})

test_that("CLUSTER-TYPES-003: 2l.bin accepts a factor cluster", {
  skip_if_not_installed("lme4")
  d <- d1
  d$b <- factor(as.integer(d$lpo > median(d$lpo, na.rm = TRUE)))
  d$lpo <- NULL
  p <- make.predictorMatrix(d)
  p[,] <- 0
  p["b", c("sch", "iqv")] <- c(-2, 1)
  imp <- mice(
    d,
    method = c(sch = "", iqv = "", b = "2l.bin"),
    predictorMatrix = p,
    m = 1,
    maxit = 1,
    seed = 1,
    print = FALSE
  )
  expect_false(anyNA(complete(imp)$b))
})

test_that("CLUSTER-TYPES-004: a character cluster stops early", {
  d <- d0
  d$sch <- as.character(d$sch)
  expect_error(impute(d, "2l.norm"), "as.integer\\(factor\\(sch\\)\\)")
})

test_that("CLUSTER-TYPES-005: an imputed cluster with missing values stops", {
  d <- d1
  d$sch[1:5] <- NA
  expect_error(
    mice(
      d,
      method = c(sch = "polyreg", lpo = "2l.norm", iqv = ""),
      predictorMatrix = pred,
      m = 1,
      maxit = 1,
      print = FALSE
    ),
    "cannot impute cluster membership"
  )
})

test_that("CLUSTER-TYPES-006: missing cluster values are logged, rows not imputed", {
  for (m in c("2l.norm", if (has.lme4) "2l.lmer")) {
    d <- d1
    d$sch[c(1:5, 200)] <- NA
    d$lpo[1] <- NA
    imp <- suppressWarnings(impute(d, m))
    ev <- imp$loggedEvents
    expect_true(any(ev$dep == "sch" & grepl("6 missing values", ev$out)))
    cd <- complete(imp)
    expect_true(is.na(cd$lpo[1]))
    expect_false(anyNA(cd$lpo[!is.na(d$sch)]))
  }
})
