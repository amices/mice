context("tidiers")

data(nhanes)
imp <- mice::mice(nhanes, maxit = 2, m = 2, seed = 1, print = FALSE)
fit_mira <- with(imp, lm(chl ~ age + bmi))
fit_mipo <- mice::pool(fit_mira)

test_that("glance.mipo: nhanes lm", {
    tmp <- glance(fit_mipo)
    expect_true(inherits(tmp, 'data.frame'))
    expect_equal(tmp$adj.r.squared[1], 0.4966436, tolerance = .00001)
    expect_equal(tmp$r.squared[1], 0.539119, tolerance = .00001)
})

test_that("tidy.mipo: nhanes lm", {
    
    tmp <- tidy(fit_mipo)
    expect_true(inherits(tmp, 'data.frame'))
    expect_equal(dim(tmp), c(3, 13))
    
    tmp <- tidy(fit_mipo, conf.int = TRUE)
    expect_true(inherits(tmp, 'data.frame'))
    expect_equal(dim(tmp), c(3, 15))
    expect_equal(tmp$conf.low, c(-171.676808396086, -12.5277617578218,
                                 3.42203157045941))
    
    tmp <- tidy(fit_mipo, conf.int = TRUE, conf.level = .99)
    expect_true(inherits(tmp, 'data.frame'))
    expect_equal(dim(tmp), c(3, 15))
    expect_equal(tmp$conf.low, c(-216.910255354075, -63.8124944550467,
                                 2.16193377446054))
    
})
