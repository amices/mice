context("tidiers")

test_that("glance.mira: nhanes lm", {

    imp <- mice(nhanes, maxit = 2, m = 2, seed = 1, print = FALSE)
    fit <- with(imp, lm(chl ~ age + bmi))

    tmp <- glance(fit)
    expect_true(inherits(tmp, 'data.frame'))
    expect_equal(dim(tmp), c(1, 4))

})
 
test_that("tidy.mira: nhanes lm", {

    imp <- mice(nhanes, maxit = 2, m = 2, seed = 1, print = FALSE)
    fit <- with(imp, lm(chl ~ age + bmi))

    tmp <- tidy(fit)
    expect_true(inherits(tmp, 'data.frame'))
    expect_equal(dim(tmp), c(3, 13))

    tmp <- tidy(fit, conf.int = TRUE)
    expect_true(inherits(tmp, 'data.frame'))
    expect_equal(dim(tmp), c(3, 15))
    expect_equal(tmp$conf.low, c(-171.676808396086, -12.5277617578218,
                                 3.42203157045941))

    tmp <- tidy(fit, conf.int = TRUE, conf.level = .99)
    expect_true(inherits(tmp, 'data.frame'))
    expect_equal(dim(tmp), c(3, 15))
    expect_equal(tmp$conf.low, c(-216.910255354075, -63.8124944550467,
                                 2.16193377446054))

})
