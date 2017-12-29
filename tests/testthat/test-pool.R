context("pool")

imp <- mice(nhanes2, print = FALSE, maxit = 2, seed = 121)
fit <- with(imp, lm(bmi ~ chl + age + hyp))
est <- pool(fit)
#fitlist <- fit$analyses
#est <- mice:::pool.fitlist(fitlist)

mn <- c(19.06956625, -5.73559483, -7.56527376, 0.05537215, 2.36656173)
se <- c(3.50493691, 1.94113281, 2.67898025, 0.01927275, 1.74985196)

test_that("retains same numerical result", {
  expect_equal(unname(est$qbar), mn)
  expect_equal(unname(summary(est)[, "se"]), se)
})


imp <- mice(nhanes2, print = FALSE, m = 10, seed = 219)
fit0 <- with(data = imp, expr = glm(hyp == "yes" ~ 1, family = binomial))
fit1 <- with(data = imp, expr = glm(hyp == "yes" ~ chl + bmi, family = binomial))
stat1 <- pool.compare(fit1, fit0, method = "wald")
stat2 <- pool.compare(fit1, fit0, method = "likelihood")

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
stat1 <- pool.compare(fit1, fit0, method = "wald")
stat2 <- pool.compare(fit1, fit0, method = "likelihood")

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

