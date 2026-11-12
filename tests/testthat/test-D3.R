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

test_that("D3-003: lm, imputed data: D3() equals mitml::testModels()", {
  expect_equal(unname(z1$result[1]), unname(z2$test[1]), tolerance = 1e-6)
  expect_equal(unname(z1$result[3]), unname(z2$test[3]), tolerance = 1e-6)
  expect_equal(unname(z1$result[4]), unname(z2$test[4]), tolerance = 1e-6)
  expect_equal(unname(z1$result[5]), unname(z2$test[5]), tolerance = 1e-6)
})

test_that("D3-003b: lm, imputed data: gaussian glm gives the same D3 as lm", {
  g1 <- with(data = imp, expr = glm(hyp ~ age + chl))
  g0 <- with(data = imp, expr = glm(hyp ~ 1))
  zg <- D3(g1, g0)
  expect_equal(zg$result, z1$result, tolerance = 1e-6)
})

test_that("D3-003c: lm, imputed data: D3() with fit0 = NULL tests all coefficients", {
  z <- D3(fit1)
  expect_s3_class(z, "mice.anova")
  expect_equal(unname(z$result[2]), 2)
  expect_true(is.finite(z$result[1]))
})

# imputed data: lmer (requires broom.mixed)
test_that("D3-004: lmer, imputed data: D3() equals mitml::testModels() and refits REML", {
  skip_if_not_installed("broom.mixed")
  skip_if_not_installed("lme4")
  skip_if_not_installed("pan")
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
  expect_equal(unname(z3$result[1]), unname(z4$test[1]), tolerance = 1e-6)
  expect_equal(unname(z3$result[5]), unname(z4$test[5]), tolerance = 1e-6)

  # REML fits are refitted with ML and give the same result
  fit1_reml <- with(implist, lmer(ReadAchiev ~ ReadDis + SES + (1 | ID)))
  fit0_reml <- with(implist, lmer(ReadAchiev ~ (1 | ID)))
  expect_message(z3r <- D3(fit1_reml, fit0_reml), "REML")
  expect_equal(z3r$result, z3$result, tolerance = 1e-6)
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

test_that("D3-006: lm with factors, imputed data: D3() equals mitml::testModels()", {
  expect_equal(unname(z7$result[1]), unname(z8$test[1]), tolerance = 1e-6)
  expect_equal(unname(z7$result[3]), unname(z8$test[3]), tolerance = 1e-6)
  expect_equal(unname(z7$result[4]), unname(z8$test[4]), tolerance = 1e-6)
  expect_equal(unname(z7$result[5]), unname(z8$test[5]), tolerance = 1e-6)
})

test_that("D3-007: weighted lm: D3() with equal weights equals the unweighted result", {
  w1 <- with(data = imp2, expr = lm(bmi ~ age + chl + hyp, weights = rep(2, 25)))
  w0 <- with(data = imp2, expr = lm(bmi ~ age, weights = rep(2, 25)))
  zw <- D3(w1, w0)
  # equal weights do not change the estimates, nor the likelihood ratio
  expect_equal(zw$result, z7$result, tolerance = 1e-6)
})
