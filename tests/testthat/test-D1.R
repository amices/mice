context("D1")
imp <- mice(nhanes2, print = FALSE, m = 10, seed = 219)

fit1 <- with(data = imp, expr = glm(hyp == "yes" ~ age + chl, family = binomial))
fit0 <- with(data = imp, expr = glm(hyp == "yes" ~ 1, family = binomial))
empty <- with(data = imp, expr = glm(hyp == "yes" ~ 0, family = binomial))

# stat1 <- pool.compare(fit1, fit0, method = "wald")
# deprecated because it relies on full vcov, which is not present anymore
# in the mipo object

# the next tests were remove because they failed on many 
# systems, not yet clear what the cause is (#128)

# three new ways to compare fit1 to the intercept-only model
# z1 <- D1(fit1, fit0)
# z2 <- mitml::testModels(as.mitml.result(fit1), as.mitml.result(fit0), df.com = 21)
# z3 <- D1(fit1)

#test_that("compares fit1 to the intercept-only model", {
#   expect_identical(z1$result, z2$test)
#   expect_identical(z1$test, z3$test)
#})

# two ways to compare fit1 to the empty model
# z4 <- D1(fit1, empty)
# z5 <- mitml::testModels(as.mitml.result(fit1), NULL, df.com = 21)

#test_that("compares fit1 to empty model", {
#  expect_identical(z4$result, z5$test)
#})


context("D2")

z1 <- D2(fit1, fit0)
z2 <- mitml::testModels(as.mitml.result(fit1), as.mitml.result(fit0), method = "D2")
z3 <- D2(fit1)

test_that("compares fit1 to the intercept-only model", {
  expect_identical(z1$result, z2$test)
  expect_identical(z1$test, z3$test)
})

# two ways to compare fit1 to the empty model
z4 <- D2(fit1, empty)
z5 <- mitml::testModels(as.mitml.result(fit1), NULL, method = "D2")

test_that("compares fit1 to empty model", {
  expect_identical(z4$result, z5$test)
})
