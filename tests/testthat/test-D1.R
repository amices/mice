context("D1")
imp <- mice(nhanes2, print = FALSE, m = 10, seed = 219)

fit1 <- with(data = imp, expr = glm(hyp == "yes" ~ age + chl, family = binomial))
fit0 <- with(data = imp, expr = glm(hyp == "yes" ~ 1, family = binomial))
empty <- with(data = imp, expr = glm(hyp == "yes" ~ 0, family = binomial))

stat1 <- pool.compare(fit1, fit0, method = "wald")

z1 <- D1(fit1, fit0)
z2 <- mitml::testModels(as.mitml.result(fit1), as.mitml.result(fit0))

z3 <- D1(fit1)
z4 <- D1(fit1, empty)

test_that("D1 tests are identical", {
   expect_identical(z1$test, z2$test)
   expect_identical(z3, z4)
})

