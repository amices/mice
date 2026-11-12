# texp is a level-2 variable, so missingness must be complete within a school
set.seed(66322)
y <- popmis$texp
x <- popmis[, c("pupil", "school", "sex")]
mis_schools <- sample(unique(x$school), size = 40)
y[x$school %in% mis_schools] <- NA
ry <- !is.na(y)
wy1 <- !ry
wy2 <- rep(TRUE, length(y))
wy3 <- rep(FALSE, length(y))
wy4 <- rep(c(TRUE, FALSE), times = c(1, length(y) - 1))
type <- c(1, -2, 1)

test_that("MICE-IMPUTE-2LONLY-PMM-001: Returns requested length", {
  expect_length(mice.impute.2lonly.pmm(y, ry, x, type, wy1), sum(wy1))
  expect_length(mice.impute.2lonly.pmm(y, ry, x, type, wy2), sum(wy2))
  expect_length(mice.impute.2lonly.pmm(y, ry, x, type, wy3), sum(wy3))
  expect_length(mice.impute.2lonly.pmm(y, ry, x, type, wy4), sum(wy4))
})

test_that("MICE-IMPUTE-2LONLY-PMM-002: Imputations are observed values, constant within cluster", {
  imp <- mice.impute.2lonly.pmm(y, ry, x, type, wy1)
  expect_true(all(imp %in% y[ry]))
  per_cluster <- tapply(imp, x$school[wy1], function(v) length(unique(v)))
  expect_true(all(per_cluster == 1))
})

test_that("MICE-IMPUTE-2LONLY-PMM-003: Rejects clusters with partially missing level-2 data", {
  ypart <- popmis$texp
  ypart[1] <- NA
  expect_error(
    mice.impute.2lonly.pmm(ypart, !is.na(ypart), x, type),
    "partially missing"
  )
})

test_that("MICE-IMPUTE-2LONLY-PMM-004: mice() runs with method 2lonly.pmm", {
  d <- popmis[, c("school", "sex", "texp")]
  d$texp <- y
  pred <- make.predictorMatrix(d)
  pred["texp", "school"] <- -2
  imp <- mice(
    d,
    method = c("", "", "2lonly.pmm"),
    pred = pred,
    m = 1,
    maxit = 1,
    seed = 1,
    print = FALSE
  )
  cmp <- complete(imp)
  expect_false(anyNA(cmp))
  per_cluster <- tapply(cmp$texp, cmp$school, function(v) length(unique(v)))
  expect_true(all(per_cluster == 1))
})
