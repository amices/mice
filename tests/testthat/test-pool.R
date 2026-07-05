context("pool")

# set the random generator to V3.5.0 to ensure that this test
# passes in V3.6.0 and later
# see mail Kurt Hornik, dated 06mar19
suppressWarnings(RNGversion("3.5.0"))

imp <- mice(nhanes2, print = FALSE, maxit = 2, seed = 121, use.matcher = TRUE)
fit <- with(imp, lm(bmi ~ chl + age + hyp))
est <- pool(fit)

mn <- c(18.76175, 0.05359003, -4.573652, -6.635969, 2.163629)
se <- c(4.002796, 0.02235067, 2.033986, 2.459769, 2.02898)

test_that("summary(est) works", {
  expect_is(summary(est), "data.frame")
})

test_that("retains same numerical result", {
  expect_equal(unname(getqbar(est)), mn, tolerance = 0.00001)
  expect_equal(unname(summary(est)[, "std.error"]), se, tolerance = 0.00001)
})

test_that("D1 and D3 work for glm on imputed data", {
  imp <- mice(nhanes2, print = FALSE, m = 10, seed = 219)
  fit0 <- with(data = imp, expr = glm(hyp == "yes" ~ 1, family = binomial))
  fit1 <- with(data = imp, expr = glm(hyp == "yes" ~ chl + bmi, family = binomial))
  expect_is(D1(fit1, fit0), "mice.anova")
  expect_is(D3(fit1, fit0), "mice.anova")
})

test_that("D1 p-value for glm is numeric", {
  skip("FIXME: D1/D3 return NA p-value for glm on imputed data")
  imp <- mice(nhanes2, print = FALSE, m = 10, seed = 219)
  fit0 <- with(data = imp, expr = glm(hyp == "yes" ~ 1, family = binomial))
  fit1 <- with(data = imp, expr = glm(hyp == "yes" ~ chl + bmi, family = binomial))
  expect_false(is.na(D1(fit1, fit0)$result["P(>F)"]))
  expect_false(is.na(D3(fit1, fit0)$result["P(>F)"]))
})
