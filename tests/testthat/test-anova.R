context("anova")
imp <- mice(nhanes2, m = 10, print = FALSE, seed = 71242)
m2 <- with(imp, lm(chl ~ age + bmi))
m1 <- with(imp, lm(chl ~ bmi))
m0 <- with(imp, lm(chl ~ 1))

# anova methods
z1 <- anova(m2, m1, m0)
z2a <- anova(m2, m1, m0, method = "D2")
z2b <- anova(m2, m1, m0, method = "D2", use = "likelihood")
z3 <- anova(m2, m1, m0, method = "D3")

