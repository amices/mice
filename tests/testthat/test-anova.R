context("anova")
imp <- mice(nhanes2, m = 10, print = FALSE, seed = 71242)
m2 <- with(imp, lm(chl ~ age + bmi))
m1 <- with(imp, lm(chl ~ bmi))
m0 <- with(imp, lm(chl ~ 1))

# anova methods
test_that("anova.mira() produces silent D1 and D3", {
  expect_silent(z1 <- anova(m2, m1, m0))
  expect_silent(z3 <- anova(m2, m1, m0, method = "D3"))
})
  
test_that("anova.mira() produces silent with D2", {
  expect_silent(z2a <- anova(m2, m1, m0, method = "D2"))
  expect_silent(z2b <- anova(m2, m1, m0, method = "D2", use = "likelihood"))
})

context("Cox model tests")
library(survival)

set.seed(1)
data <- survival::lung
data$age[rbinom(nrow(data), size = 1, prob = 0.2) == 1] <- NA
data$sex[rbinom(nrow(data), size = 1, prob = 0.2) == 1] <- NA
data$ph.ecog[rbinom(nrow(data), size = 1, prob = 0.2) == 1] <- NA

imp <- mice(data, print = FALSE)
m1 <- with(imp, coxph(Surv(time, status) ~ age))
m2 <- with(imp, coxph(Surv(time, status) ~ age + sex))
m3 <- with(imp, coxph(Surv(time, status) ~ age + sex + ph.ecog))

test_that("runs tests for the Cox model", {
  expect_silent(pool(m1))
  expect_silent(D1(m2, m1))
  expect_silent(D2(m2, m1))
  expect_error(D3(m2, m1))
  expect_silent(anova(m3, m2, m1))
})
