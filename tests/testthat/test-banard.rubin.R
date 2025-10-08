# old implementation
barnard.rubin_old <- function(m, b, t, dfcom = Inf) {
  lambda <- (1 + 1 / m) * b / t
  lambda[lambda < 1e-04] <- 1e-04
  dfold <- (m - 1) / lambda^2
  dfobs <- (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lambda)
  ifelse(is.infinite(dfcom), dfold, dfold * dfobs / (dfold + dfobs))
}

test_that("new BR matches old for lambda >= 1e-4 and dfcom finite", {
  set.seed(1)
  M <- 10
  df_com <- 100
  t <- 1
  lambdas <- seq(1, 1e-04, length.out = 100)
  for (lambda in lambdas) {
    b <- lambda * t / (1 + 1 / M)

    old <- barnard.rubin_old(M, b, t, df_com)
    new <- barnard.rubin(M, b, t, df_com)

    expect_equal(new, old)
  }
  df_com <- Inf
  for (lambda in lambdas) {
    b <- lambda * t / (1 + 1 / M)

    old <- barnard.rubin_old(M, b, t, df_com)
    new <- barnard.rubin(M, b, t, df_com)

    expect_equal(new, old)
  }
})

test_that("new BR differs from old for lambda < 1e-4 and dfcom finite", {
  M <- 10
  df_com <- 100
  t <- 1
  lambdas_small <- seq(1e-04, 0, length.out = 100)
  for (lambda in lambdas_small[-1]) {
    b <- lambda * t / (1 + 1 / M)

    old <- barnard.rubin_old(M, b, t, df_com)
    new <- barnard.rubin(M, b, t, df_com)

    expect_true(new != old)
  }
})

test_that("new BR handles tiny lambda without flooring and approaches correct limit", {
  M <- 10
  df_com <- 100
  t <- 1
  lambda <- c(0, 1e-8, 1e-6, 5e-5) # includes values below the old 1e-4 floor
  b <- lambda * t / (1 + 1 / M)

  old <- barnard.rubin_old(M, b, t, df_com)
  new <- barnard.rubin(M, b, t, df_com)

  # limit at lambda = 0
  limit0 <- df_com * (df_com + 1) / (df_com + 3)

  # new is finite and near the limit; old is biased by flooring
  expect_true(all(is.finite(new)))
  expect_equal(new[1], limit0) # exactly λ = 0
  expect_true(all(abs(new - limit0) <= abs(old - limit0)))
})

test_that("dfcom = Inf reduces to nu_old for both when lambda >= 1e-4", {
  M <- 8
  t <- 2

  for (lambda in c(1e-4, 1e-3, 0.2)) {
    b <- lambda * t / (1 + 1 / M)

    old <- barnard.rubin_old(M, b, t, dfcom = Inf)
    new <- barnard.rubin(M, b, t, dfcom = Inf)
    nu_old <- (M - 1) / (lambda^2)

    # old floors only affects λ < 1e-4, so here they should agree with nu_old
    expect_equal(old, nu_old)
    expect_equal(new, nu_old)
  }
})
