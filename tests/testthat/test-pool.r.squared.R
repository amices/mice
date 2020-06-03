context("pool.r.squared")

data(nhanes)
imp <- mice::mice(nhanes, maxit = 2, m = 2, seed = 10, print = FALSE)
fit_mira <- with(data = imp, exp = lm(chl ~ age + bmi))
fit_mipo <- mice::pool(fit_mira)

test_that("pool.r.squared mira", {
    result <- as.vector(pool.r.squared(fit_mira, adjusted = FALSE)[1, ])
    truth <- c(0.529660990363042, 0.155185078144953, 0.795625721175786, NaN)
    expect_equal(result, truth)
})

test_that("pool.r.squared mira adjusted", {
    result <- as.vector(pool.r.squared(fit_mira, adjusted = TRUE)[1, ])
    truth <- c(0.486574783461685, 0.111808390236973, 0.774997055801878, NaN)
    expect_equal(result, truth)
})

test_that("r.squared mipo", {
    result <- as.vector(pool.r.squared(fit_mipo, adjusted = FALSE)[1, ])
    truth <- c(0.529660990363042, 0.155185078144953, 0.795625721175786, NaN)
    expect_equal(result, truth)
})

test_that("r.squared mipo adjusted", {
    result <- as.vector(pool.r.squared(fit_mipo, adjusted = TRUE)[1, ])
    truth <- c(0.486574783461685, 0.111808390236973, 0.774997055801878, NaN)
    expect_equal(result, truth)
})

