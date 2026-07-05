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

test_that("lm, complete data: D3() and lrtest() calculate same test statistic", {
  expect_equal(x1$Chisq[2], x2$result[1], tolerance = 1e-6)
})

test_that("lm, complete data: testModels() and lrtest() calculate same test statistic", {
  expect_equal(x1$Chisq[2], x3$test[1], tolerance = 1e-6)
})

# imputed data: lm
imp <- mice(nhanes, print = FALSE, m = 10, seed = 219)
fit1 <- with(data = imp, expr = lm(hyp ~ age + chl))
fit0 <- with(data = imp, expr = lm(hyp ~ 1))
z1 <- D3(fit1, fit0)
z2 <- mitml::testModels(as.mitml.result(fit1), as.mitml.result(fit0), method = "D3")

test_that("lm, imputed data: mice D3() and mitml testModels() calculate same F", {
  skip("FIXME: mice and mitml give discrepant D3 results for imputed data (#226)")
  expect_equal(z1$result[1], z2$test[1])
})

# imputed data: lmer (requires broom.mixed)
test_that("lmer, imputed data: mice D3() and mitml testModels() calculate same F", {
  skip_if_not_installed("broom.mixed")
  suppressPackageStartupMessages(library(mitml))
  library(lme4)
  data(studentratings)
  fml <- ReadDis + SES ~ ReadAchiev + (1 | ID)
  set.seed(26262)
  imp_pan <- mitml::panImpute(studentratings, formula = fml,
                              n.burn = 1000, n.iter = 100, m = 5, silent = TRUE)
  implist <- mitml::mitmlComplete(imp_pan, print = 1:5)
  fit0_lmer <- with(implist, lmer(ReadAchiev ~ (1 | ID), REML = FALSE))
  fit1_lmer <- with(implist, lmer(ReadAchiev ~ ReadDis + SES + (1 | ID), REML = FALSE))
  z3 <- D3(fit1_lmer, fit0_lmer)
  z4 <- mitml::testModels(fit1_lmer, fit0_lmer, method = "D3")
  skip("FIXME: mice and mitml give discrepant D3 results for imputed data (#226)")
  expect_equal(z3$result[1], z4$test[1])
})

# imputed data: glm
test_that("glm, imputed data: mice D3() and mitml testModels() calculate same F", {
  skip("FIXME: D3() with glm produces negative Dm and convergence problems")
  imp_glm <- mice(nhanes2, print = FALSE, m = 10, seed = 219)
  fit1_glm <- with(data = imp_glm, expr = glm(hyp == "yes" ~ age + chl, family = binomial))
  fit0_glm <- with(data = imp_glm, expr = glm(hyp == "yes" ~ 1, family = binomial))
  z5 <- D3(fit1_glm, fit0_glm)
  z6 <- mitml::testModels(as.mitml.result(fit1_glm), as.mitml.result(fit0_glm), method = "D3")
  expect_equal(z5$result[1], z6$test[1])
})

# imputed data: factors
imp2 <- mice(nhanes2, print = FALSE, m = 10, seed = 219)
fit1 <- with(data = imp2, expr = lm(bmi ~ age + chl + hyp))
fit0 <- with(data = imp2, expr = lm(bmi ~ age))
z7 <- D3(fit1, fit0)
z8 <- mitml::testModels(as.mitml.result(fit1), as.mitml.result(fit0), method = "D3")

test_that("lm with factors, imputed data: mice D3() and mitml testModels() calculate same F", {
  skip("FIXME: mice and mitml give discrepant D3 results for imputed data (#226)")
  expect_equal(z7$result[1], z8$test[1])
})
