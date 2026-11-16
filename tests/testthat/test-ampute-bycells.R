test_that("cell proportions use the frequency-weighted number of missing cells", {
  patterns <- rbind(c(0, 1, 1, 1), c(1, 0, 0, 0))
  convert <- function(prop, freq = c(0.5, 0.5)) {
    mice:::recalculate.prop(prop, n = 100, k = 4,
                           patterns = patterns, freq = freq)
  }
  expect_equal(convert(0.2), 0.4)
  expect_equal(convert(0.4), 0.8)
  expect_equal(convert(0.2, c(0.25, 0.75)), 0.32)
  expect_equal(convert(0.5), 1)
  expect_error(convert(0.6), "Proportion of missing cells is too large")

  # A single pattern with one missing cell out of four is unchanged.
  expect_equal(mice:::recalculate.prop(
    0.2, n = 100, k = 4, patterns = patterns[1, , drop = FALSE], freq = 1
  ), 0.8)
})

test_that("ampute achieves feasible cell proportions with heterogeneous patterns", {
  set.seed(42)
  X <- matrix(rnorm(5000 * 4), ncol = 4)
  patterns <- rbind(c(0, 1, 1, 1), c(1, 0, 0, 0))
  for (target in c(0.2, 0.4)) {
    result <- ampute(X, prop = target, patterns = patterns,
                     freq = c(0.5, 0.5), mech = "MCAR", bycases = FALSE)
    expect_equal(result$prop, 2 * target)
    expect_lt(abs(mean(is.na(result$amp)) - target), 0.02)
  }
})
