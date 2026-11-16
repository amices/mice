# No mitml dependency: equivalent Gaussian models provide an independent check.
test_that("D3 agrees for weighted lm and Gaussian identity-link glm", {
  imp <- mice(nhanes, m = 2, maxit = 1, seed = 1, printFlag = FALSE)
  for (w in list(rep(1, 25), rep(2, 25), c(rep(1, 15), rep(2, 10)))) {
    lm1 <- with(imp, lm(bmi ~ age + chl, weights = w))
    lm0 <- with(imp, lm(bmi ~ 1, weights = w))
    gl1 <- with(imp, glm(bmi ~ age + chl, family = gaussian, weights = w))
    gl0 <- with(imp, glm(bmi ~ 1, family = gaussian, weights = w))
    expect_equal(D3(gl1, gl0)$result, D3(lm1, lm0)$result, tolerance = 1e-7)
  }
})

test_that("D3 evaluates a weighted Gaussian model at its own MLE correctly", {
  set.seed(42)
  d <- data.frame(x = rnorm(50), y = rnorm(50), w = rep(c(1, 2), 25))
  fits <- list(
    lm(y ~ x, data = d, weights = w),
    glm(y ~ x, data = d, weights = w, family = gaussian)
  )
  for (fit in fits) {
    pars <- mice:::d3.pars(fit)
    expected_sigma2 <- sum(weights(fit, type = "prior") *
      residuals(fit, type = "response")^2) / nobs(fit)
    expect_equal(pars$sigma2, expected_sigma2)
    expect_equal(mice:::d3.loglik(fit, pars), as.numeric(logLik(fit)),
                 tolerance = 1e-8)
  }
})
