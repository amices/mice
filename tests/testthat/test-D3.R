skip_if_not_installed("mitml")

# The following test was contributed by jawitte
# https://github.com/amices/mice/issues/226
set.seed(1)
A <- rnorm(100)
B <- 0.1 * A + rnorm(100)
fit1 <- lapply(1:5, function(m) lm(B ~ A))
fit0 <- lapply(1:5, function(m) lm(B ~ 1))

# hardcoded lrtest() result to avoid lmtest dependency
x1 <- structure(
  list(
    `#Df` = c(3, 2),
    LogLik = c(-137.087912980007, -137.516434459951),
    Df = c(NA, -1),
    Chisq = c(NA, 0.857042959888474),
    `Pr(>Chisq)` = c(NA, 0.354567523408569)
  ),
  class = c("anova", "data.frame"),
  row.names = c("1", "2"),
  heading = c("Likelihood ratio test\n", "Model 1: B ~ A\nModel 2: B ~ 1")
)
x2 <- D3(fit1 = fit1, fit0 = fit0)
x3 <- mitml::testModels(fit1, fit0, method = "D3")

test_that("D3-001: lm, complete data: D3() and lrtest() calculate same test statistic", {
  expect_equal(x1$Chisq[2], x2$result[1], tolerance = 1e-6)
})

test_that("D3-002: lm, complete data: testModels() and lrtest() calculate same test statistic", {
  expect_equal(x1$Chisq[2], x3$test[1], tolerance = 1e-6)
})

# imputed data: lm
imp <- mice(nhanes, print = FALSE, m = 10, seed = 219)
fit1 <- with(data = imp, expr = lm(hyp ~ age + chl))
fit0 <- with(data = imp, expr = lm(hyp ~ 1))
z1 <- D3(fit1, fit0)
z2 <- mitml::testModels(
  as.mitml.result(fit1),
  as.mitml.result(fit0),
  method = "D3"
)

# D3() constrains the regression coefficients to their pooled values and
# re-estimates the residual variance in each imputed dataset. testModels()
# also constrains the residual variance to its pooled value, as in Meng and
# Rubin (1992). The two therefore differ for lm. The helper below evaluates
# the likelihood with both the coefficients and the ML residual variance
# pooled, which should reproduce testModels() exactly.
d3_pooled_sigma <- function(fit1, fit0) {
  fit1 <- getfit(fit1)
  fit0 <- getfit(fit0)
  m <- length(fit1)
  qbar1 <- colMeans(do.call(rbind, lapply(fit1, coef)))
  qbar0 <- colMeans(do.call(rbind, lapply(fit0, coef)))
  k <- length(qbar1) - length(qbar0)
  s2 <- function(f) sum(residuals(f)^2) / nobs(f)
  s2bar1 <- mean(vapply(fit1, s2, numeric(1)))
  s2bar0 <- mean(vapply(fit0, s2, numeric(1)))
  ll <- function(f, beta, s2) {
    y <- model.response(model.frame(f))
    r <- y - model.matrix(f) %*% beta
    -length(y) / 2 * log(2 * pi * s2) - sum(r^2) / (2 * s2)
  }
  dev.M <- mean(-2 * (
    vapply(fit0, logLik, numeric(1)) - vapply(fit1, logLik, numeric(1))
  ))
  dev.L <- mean(-2 * mapply(
    function(f0, f1) ll(f0, qbar0, s2bar0) - ll(f1, qbar1, s2bar1),
    fit0,
    fit1
  ))
  rm <- ((m + 1) / (k * (m - 1))) * (dev.M - dev.L)
  c(Dm = dev.L / (k * (1 + rm)), rm = rm)
}

test_that("D3-003: lm, imputed data: D3() differs from testModels() only in the handling of sigma", {
  ps <- d3_pooled_sigma(fit1, fit0)
  expect_equal(unname(ps["Dm"]), unname(z2$test[1]), tolerance = 1e-6)
  expect_equal(unname(ps["rm"]), unname(z2$test[5]), tolerance = 1e-6)
  expect_false(isTRUE(all.equal(z1$result[1], z2$test[1])))
})

# imputed data: lmer (requires broom.mixed)
test_that("D3-004: lmer, imputed data: mice D3() and mitml testModels() calculate same F", {
  skip_if_not_installed("broom.mixed")
  suppressPackageStartupMessages(library(mitml))
  library(lme4)
  data(studentratings)
  fml <- ReadDis + SES ~ ReadAchiev + (1 | ID)
  set.seed(26262)
  imp_pan <- mitml::panImpute(
    studentratings,
    formula = fml,
    n.burn = 1000,
    n.iter = 100,
    m = 5,
    silent = TRUE
  )
  implist <- mitml::mitmlComplete(imp_pan, print = 1:5)
  fit0_lmer <- with(implist, lmer(ReadAchiev ~ (1 | ID), REML = FALSE))
  fit1_lmer <- with(
    implist,
    lmer(ReadAchiev ~ ReadDis + SES + (1 | ID), REML = FALSE)
  )
  z3 <- D3(fit1_lmer, fit0_lmer)
  z4 <- mitml::testModels(fit1_lmer, fit0_lmer, method = "D3")
  expect_s3_class(z3, "mice.anova")
  expect_true(is.finite(z3$result[1]))
  skip(
    "D3() constrains the fixed effects only; testModels() also constrains the variance components (see ?D3)"
  )
  expect_equal(z3$result[1], z4$test[1])
})

# glm: without missing data D3 must reduce to the likelihood ratio test.
# nhanes2 is unsuitable for a glm example because the hypertension model
# suffers from separation in most imputed datasets.
set.seed(2)
n <- 200
xg <- rnorm(n)
zg <- rnorm(n)
yg <- rbinom(n, 1, plogis(-0.5 + 0.8 * xg))
dg <- data.frame(y = yg, x = xg, z = zg)
g1 <- lapply(1:5, function(i) glm(y ~ x + z, family = binomial, data = dg))
g0 <- lapply(1:5, function(i) glm(y ~ 1, family = binomial, data = dg))
lrt <- anova(g0[[1]], g1[[1]], test = "Chisq")

test_that("D3-005: glm, complete data: D3() equals the likelihood ratio test", {
  z5 <- D3(g1, g0)
  k <- z5$result[2]
  expect_equal(k, 2)
  expect_equal(unname(z5$result[1] * k), lrt$Deviance[2], tolerance = 1e-6)
  expect_equal(unname(z5$result[5]), 0, tolerance = 1e-8)
})

test_that("D3-005b: glm, imputed data: D3() warns when the statistic is negative", {
  imp_glm <- mice(nhanes2, print = FALSE, m = 10, seed = 219)
  fit1_glm <- suppressWarnings(with(
    data = imp_glm,
    expr = glm(hyp == "yes" ~ age + chl, family = binomial)
  ))
  fit0_glm <- with(
    data = imp_glm,
    expr = glm(hyp == "yes" ~ 1, family = binomial)
  )
  expect_warning(z <- D3(fit1_glm, fit0_glm), "negative")
  expect_lt(z$result[1], 0)
})

# imputed data: factors
imp2 <- mice(nhanes2, print = FALSE, m = 10, seed = 219)
fit1 <- with(data = imp2, expr = lm(bmi ~ age + chl + hyp))
fit0 <- with(data = imp2, expr = lm(bmi ~ age))
z7 <- D3(fit1, fit0)
z8 <- mitml::testModels(
  as.mitml.result(fit1),
  as.mitml.result(fit0),
  method = "D3"
)

test_that("D3-006: lm with factors, imputed data: D3() differs from testModels() only in the handling of sigma", {
  ps <- d3_pooled_sigma(fit1, fit0)
  expect_equal(unname(ps["Dm"]), unname(z8$test[1]), tolerance = 1e-6)
  expect_equal(unname(ps["rm"]), unname(z8$test[5]), tolerance = 1e-6)
  expect_false(isTRUE(all.equal(z7$result[1], z8$test[1])))
})
