context("pool")

# set the random generator to V3.5.0 to ensure that this test 
# passes in V3.6.0 and later
# see mail Kurt Hornik, dated 06mar19
# FIXME: consider using the new generator once V3.6.0 is out, 
# at the expense of breaking reproducibility of the examples in 
# https://stefvanbuuren.name/fimd/
suppressWarnings(RNGversion("3.5.0"))

imp <- mice(nhanes2, print = FALSE, maxit = 2, seed = 121)
fit <- with(imp, lm(bmi ~ chl + age + hyp))
est <- pool(fit)
#fitlist <- fit$analyses
#est <- mice:::pool.fitlist(fitlist)

mn <- c(18.76175, 0.05359003, -4.573652, -6.635969, 2.163629)
se <- c(4.002796, 0.02235067, 2.033986, 2.459769, 2.02898)

test_that("retains same numerical result", {
  expect_equal(unname(getqbar(est)), mn, tolerance = 0.00001)
  expect_equal(unname(summary(est)[, "std.error"]), se, tolerance = 0.00001)
})


imp <- mice(nhanes2, print = FALSE, m = 10, seed = 219)
fit0 <- with(data = imp, expr = glm(hyp == "yes" ~ 1, family = binomial))
fit1 <- with(data = imp, expr = glm(hyp == "yes" ~ chl + bmi, family = binomial))
D1(fit1, fit0)
D3(fit1, fit0)

# test_that("retains same numerical result", {
#    expect_equal(round(as.vector(stat1$pvalue), 3), 0.188)
#    expect_equal(stat2$pvalue, 0)
# })
# 

# Venables & Ripley, 2nd, p 235
birthwt <- MASS::birthwt
bwt <- with(birthwt, 
            data.frame(
              low = factor(low),
              age = age,
              lwt = lwt,
              race = factor(race, labels = c("white", "black", "other")),
              smoke = smoke > 0,
              ptd = factor(ptl > 0),
              ht = ht > 0,
              ui = ui > 0,
              ftv = factor(ftv)))
levels(bwt$ftv)[-(1:2)] <- "2"

birthwt.glm <- glm(low ~ ., family = binomial, data = bwt)
summary(birthwt.glm)
birthwt.step <- step(birthwt.glm, trace = FALSE)

LLlogistic <- function(formula, data, coefs) {
  ### Calculates -2 loglikelihood of a model.
  logistic <- function(mu) exp(mu)/(1 + exp(mu))
  Xb <- model.matrix(formula, data) %*% coefs
  y <- model.frame(formula, data)[1][, 1]
  if (is.factor(y)) y <- (0:1)[y]
  p <- logistic(Xb)
  ## in case values of categorical var are other than 0 and 1.
  y <- (y - min(y))/(max(y) - min(y)) 
  term1 <- term2 <- rep(0, length(y))
  term1[y != 0] <- y[y != 0] * log(y[y != 0]/p[y != 0])
  term2[y == 0] <- (1 - y[y == 0]) * log((1 - y[y == 0])/(1 - p[y == 0]))
  2 * sum(term1 + term2)
}
model1 <- glm(low ~ ., family = binomial, data = bwt)
model0 <- update(model1, formula = . ~ . -age - ftv)
model.null <- update(model1, formula = . ~ 1 )

ll1 <- LLlogistic(formula = formula(model1), data = bwt, coefs = coef(model1))
ll0 <- LLlogistic(formula = formula(model0), data = bwt, coefs = coef(model0))
llnull <- LLlogistic(formula = formula(model.null), data = bwt, coefs = coef(model.null))

identical(deviance(model1), ll1, num.eq = FALSE)
identical(deviance(model0), ll0, num.eq = FALSE)
identical(deviance(model.null), llnull, num.eq = FALSE)


# try out coef.fix for binary data

f1 <- fix.coef(model1, beta = coef(model1))
broom::glance(model1)
broom::glance(f1)
identical(broom::glance(f1)$deviance, broom::glance(model1)$deviance)

beta <- coef(model1)
beta["age"] <- 0
beta["smokeTRUE"] <- 0
f2 <- fix.coef(model1, beta)
broom::glance(f2)$deviance

set.seed(123)
bwt.mis <- bwt
bwt.mis$smoke[runif(nrow(bwt)) < 0.001] <- NA
bwt.mis$lwt[runif(nrow(bwt)) < 0.01] <- NA

imp <- mice(bwt.mis, print = FALSE, m = 10)
fit1 <- with(data = imp, expr = glm(low ~ age + lwt + race + smoke + ptd + ht + ui + ftv, family = binomial))
fit0 <- with(data = imp, glm(low ~ lwt + race + smoke + ptd + ht + ui, family = binomial))
D1(fit1, fit0)
D3(fit1, fit0)

# --- test restriction of parameters

# all parameters estimated
fit <- lm(bmi ~ age + hyp + chl, data = nhanes)
coef(fit)
formula(fit)
newformula <- bmi ~ 0 + I(18.26966503 - 5.78652468 * age + 2.10467529 * hyp + 0.08044924 * chl)
newformula <- . ~ 0 + I(18.26966503 * 1L - 5.78652468 * age + 2.10467529 * hyp + 0.08044924 * chl)
fit2 <- update(fit, formula = newformula)
coef(fit2)
summary(fit)
summary(fit2)
cor(predict(fit), predict(fit) + residuals(fit))^2
cor(predict(fit2), predict(fit2) + residuals(fit2))^2
newformula <- bmi ~ 0 + offset(18.26966503 - 5.78652468 * age + 2.10467529 * hyp + 0.08044924 * chl)
fit3 <- update(fit, formula = newformula)
coef(fit3)
summary(fit3)
cor(predict(fit3), predict(fit3) + residuals(fit3))^2

# compare to mitml::anova.mitml.result
suppressPackageStartupMessages(library(mitml, quietly = TRUE))
library(lme4, quietly = TRUE)
data(studentratings)
fml <- ReadDis + SES ~ ReadAchiev + (1|ID)
imp <- mitml::panImpute(studentratings, formula=fml, n.burn=1000, n.iter=100, m=5,
                 silent = TRUE)
implist <- mitml::mitmlComplete(imp, print=1:5)

# * Example 1: multiparameter hypothesis test for 'ReadDis' and 'SES'
# This tests the hypothesis that both effects are zero.
fit0 <- with(implist, lmer(ReadAchiev ~ (1|ID), REML=FALSE))
fit1 <- with(implist, lmer(ReadAchiev ~ ReadDis + (1|ID), REML=FALSE))
# apply Rubin's rules
testEstimates(fit1)

# Wald test
# multiparameter hypothesis test using D1 (default)
mitml::testModels(fit1, fit0)
# stats <- pool.compare(as.mira(fit1), as.mira(fit0), method = "wald")
# Is the same, but probably consequence of single parameter differerence

# Wald test - multiparameter difference - incorrect because now our 
# ubar is vector, not a matrix anymore
fit0 <- with(implist, lmer(ReadAchiev ~ (1|ID), REML=FALSE))
fit1 <- with(implist, lmer(ReadAchiev ~ ReadDis + SES + (1|ID), REML=FALSE))
mitml::testModels(fit1, fit0)
# stats <- pool.compare(as.mira(fit1), as.mira(fit0), method = "wald")
# Is the same, but probably consequence of single parameter differerence

# likelihood test
mitml::testModels(fit1, fit0, method = "D3")
# stats <- pool.compare(as.mira(fit1), as.mira(fit0), method = "likelihood")

# ---

fit1 <- with(implist, lmer(ReadAchiev ~ ReadDis + SES + (1|ID), REML=FALSE))

