context("matchindex")

test_that("Returns requested length", {
  d <- c(-5, 5, 0, 10, 12)
  t <- c(-6, -4, 0, 2, 4, -2, 6)
  expect_equal(length(matchindex(d, t, k = 1)), length(t))
  expect_equal(length(matchindex(d, t, k = 5)), length(t))
})

test_that("Returns valid indices into d", {
  set.seed(1)
  d <- rnorm(50)
  t <- rnorm(20)
  idx <- matchindex(d, t, k = 5)
  expect_true(all(idx >= 1 & idx <= length(d)))
})

test_that("k = 1 returns the closest donor", {
  d <- c(-5, 5, 0, 10, 12)
  t <- c(-6, -4, 0, 2, 4, -2, 6)
  idx <- matchindex(d, t, k = 1)
  expect_equal(d[idx], c(-5, -5, 0, 0, 5, 0, 5))
})

test_that("Single donor (n1 = 1) does not crash and always matches it", {
  idx <- matchindex(5, c(1, 2, 3), k = 5)
  expect_equal(idx, c(1, 1, 1))
})

test_that("k is clamped to the number of donors", {
  d <- c(1, 2, 3)
  t <- c(1.5)
  idx <- matchindex(d, t, k = 100)
  expect_true(idx >= 1 && idx <= length(d))
})

test_that("Partial tie block smaller than k still finds the true k nearest", {
  # 3 donors tied at 0 (positions 1:3), the rest are far away. With k = 5,
  # the two closest non-tied donors (indices 4 and 5, values -1 and 1) must
  # also be reachable; donors 6:9 (values -2, -3, 3, 2) must never be chosen.
  d <- c(0, 0, 0, -1, 1, -2, -3, 3, 2)
  idx <- matchindex(d, rep(0, 500), k = 5)
  expect_true(all(idx %in% 1:5))
  expect_true(length(unique(idx)) > 3) # not stuck on the tie block alone
})

test_that("Ties are broken independently per target case (issue #236)", {
  # When every donor and every target share the same predicted value (as in
  # an intercept-only imputation model), each target must get an
  # independently random draw among *all* tied donors, not a single fixed
  # subset shared by the whole call. Before the fix, this collapsed to
  # exactly k distinct donors for any number of targets.
  set.seed(1)
  n_donors <- 100
  n_targets <- 200
  idx <- matchindex(rep(0, n_donors), rep(0, n_targets), k = 5)

  expect_true(length(unique(idx)) > 5)
  # Roughly uniform usage across donors: with 200 targets and 100 equally
  # likely donors, no single donor should dominate the draws.
  expect_true(max(table(idx)) < n_targets / 2)
})

test_that("Tie-block fast path is distributionally equivalent to a uniform draw", {
  # When the tie block adjacent to a target has >= k members, matchindex()
  # takes a fast path that samples directly from the block. Its output
  # should still be (approximately) uniform over the block.
  set.seed(1)
  n_donors <- 20
  reps <- 20000
  idx <- matchindex(rep(0, n_donors), rep(0, reps), k = 5)
  tab <- table(factor(idx, levels = 1:n_donors))
  expect_true(chisq.test(tab)$p.value > 0.001)
})

test_that("Large fully-tied donor pools do not crash and stay fast", {
  d <- rep(0, 10000)
  t <- rep(0, 5000)
  expect_silent(idx <- matchindex(d, t, k = 200))
  expect_equal(length(idx), length(t))
  expect_true(all(idx >= 1 & idx <= length(d)))
})
