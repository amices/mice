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
  
test_that("anova.mira() produces warnings on D2", {
  expect_warning(z2a <- anova(m2, m1, m0, method = "D2"))
  expect_warning(z2b <- anova(m2, m1, m0, method = "D2", use = "likelihood"))
})
