context("pool")

imp <- mice(nhanes2, print = FALSE, maxit = 2, seed = 121)
fit <- with(imp, lm(bmi ~ chl + age + hyp))
est <- pool(fit)

mn <- c(19.06956625, 0.05537215, -5.73559483, -7.56527376, 2.36656173)
se <- c(3.50493691, 0.01927275, 1.94113281, 2.67898025, 1.74985196)

test_that("retains same numerical result", {
  expect_equal(unname(est$qbar), mn)
  expect_equal(unname(summary(est)[, "se"]), se)
})
