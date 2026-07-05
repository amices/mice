# old implementation (with 1e-4 floor on lambda)
barnard.rubin_old <- function(m, b, t, dfcom = Inf) {
  lambda <- (1 + 1 / m) * b / t
  lambda[lambda < 1e-04] <- 1e-04
  dfold <- (m - 1) / lambda^2
  dfobs <- (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lambda)
  ifelse(is.infinite(dfcom), dfold, dfold * dfobs / (dfold + dfobs))
}

test_that("BARNARD-RUBIN-001: new BR matches old for lambda >= 1e-4, dfcom finite", {
  M <- 10; t <- 1; dfcom <- 100
  lambdas <- seq(1, 1e-04, length.out = 100)
  b <- lambdas * t / (1 + 1 / M)
  expect_equal(barnard.rubin(M, b, t, dfcom), barnard.rubin_old(M, b, t, dfcom))
})

test_that("BARNARD-RUBIN-002: new BR equals (m-1)/lambda^2 for lambda >= 1e-4, dfcom infinite", {
  M <- 10; t <- 1
  lambdas <- seq(1, 1e-04, length.out = 100)
  b <- lambdas * t / (1 + 1 / M)
  expect_equal(barnard.rubin(M, b, t, Inf), (M - 1) / lambdas^2)
})

test_that("BARNARD-RUBIN-003: new BR differs from old for lambda < 1e-4 (no floor applied)", {
  M <- 10; t <- 1; dfcom <- 100
  lambdas_small <- seq(1e-04, 0, length.out = 100)[-1]  # exclude 1e-4 boundary
  b <- lambdas_small * t / (1 + 1 / M)
  expect_true(all(barnard.rubin(M, b, t, dfcom) != barnard.rubin_old(M, b, t, dfcom)))
})

test_that("BARNARD-RUBIN-004: new BR handles tiny lambda and approaches correct limit as lambda -> 0", {
  M <- 10; t <- 1; dfcom <- 100
  lambda <- c(0, 1e-8, 1e-6, 5e-5)
  b <- lambda * t / (1 + 1 / M)

  old <- barnard.rubin_old(M, b, t, dfcom)
  new <- barnard.rubin(M, b, t, dfcom)

  limit0 <- dfcom * (dfcom + 1) / (dfcom + 3)  # limit as lambda -> 0

  expect_true(all(is.finite(new)))
  expect_equal(new[1], limit0)  # exactly at lambda = 0
  expect_true(all(abs(new - limit0) <= abs(old - limit0)))
})

test_that("BARNARD-RUBIN-005: dfcom = Inf reduces to (m-1)/lambda^2 for lambda >= 1e-4", {
  M <- 8; t <- 2
  lambdas <- c(1e-4, 1e-3, 0.2)
  b <- lambdas * t / (1 + 1 / M)
  nu_old <- (M - 1) / lambdas^2

  # test each scalar separately (barnard.rubin_old is not vectorised for dfcom = Inf)
  for (i in seq_along(lambdas)) {
    expect_equal(barnard.rubin_old(M, b[i], t, dfcom = Inf), nu_old[i])
    expect_equal(barnard.rubin(M, b[i], t, dfcom = Inf), nu_old[i])
  }
})
