context("anova")
imp <- mice(nhanes2, m = 10, print = FALSE, seed = 71242)
m2 <- with(imp, lm(chl ~ age + bmi))
m1 <- with(imp, lm(chl ~ bmi))

# method D1
z1 <- anova(m2, m1)
z1
summary(z1)
z2 <- D1(m2, m1)

