nh <- nhanes
nh$hyp <- as.factor(nh$hyp)
imp <- mice(nh, m = 2, maxit = 2, seed = 1, print = FALSE)

test_that("WITH-DOT-001: glm with a dot formula (#265)", {
  fit <- with(imp, glm(hyp ~ ., family = binomial))
  ref <- with(imp, glm(hyp ~ age + bmi + chl, family = binomial))
  for (i in 1:2) {
    expect_equal(coef(fit$analyses[[i]]), coef(ref$analyses[[i]]))
  }
  expect_identical(fit$analyses[[1]]$call$data, as.name(".mice.data"))
  expect_s3_class(pool(fit), "mipo")
})

test_that("WITH-DOT-002: dot formulas work with D1() and D3()", {
  f1 <- with(imp, lm(bmi ~ .))
  f0 <- with(imp, lm(bmi ~ age))
  expect_s3_class(D1(f1, f0), "mice.anova")
  expect_s3_class(D3(f1, f0), "mice.anova")
})

test_that("WITH-DOT-003: namespaced calls and dot terms", {
  fit <- with(imp, stats::lm(bmi ~ . - chl))
  expect_identical(names(coef(fit$analyses[[1]])), c("(Intercept)", "age", "hyp2"))
})

test_that("WITH-DOT-004: calls without a dot formula are not changed", {
  fit <- with(imp, lm(bmi ~ age))
  expect_null(fit$analyses[[1]]$call$data)
  d <- complete(imp, 1)
  fit <- with(imp, lm(bmi ~ ., data = d))
  expect_identical(fit$analyses[[1]]$call$data, as.name("d"))
})
