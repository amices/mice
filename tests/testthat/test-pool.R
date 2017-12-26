context("pool")

imp <- mice(nhanes2, print = FALSE, maxit = 2, seed = 121)
fit <- with(imp, lm(bmi ~ chl + age + hyp))
est <- pool(fit)

result <- c(19.06956625, 0.05537215, -5.73559483, -7.56527376, 2.36656173)

test_that("retains same numerical result", {
  expect_equal(unname(est$qbar), result)
})
