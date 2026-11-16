test_that("remove.lindep removes multiple dependencies, not independent columns", {
  set.seed(1)
  n <- 100
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  x4 <- x1 + 2 * x2
  x5 <- 2 * x2 - x3
  x6 <- rnorm(n)
  X <- cbind(x1, x2, x3, x4, x5, x6)
  y <- rnorm(n)

  # Explicit local logging state makes this test independent of other files.
  check_design <- function(x) {
    .mice.state <- list(it = 0, im = 0, dep = "y", meth = "test", log = FALSE)
    loggedEvents <- data.frame()
    keep <- mice:::remove.lindep(x, y, rep(TRUE, n), frame = environment())
    expect_true(unname(keep["x6"]))
    expect_equal(qr(x[, keep, drop = FALSE])$rank, sum(keep))
    expect_equal(sum(keep), 4L)
  }

  check_design(X)
  # Also exercise mapping after the initial constant-variable filter and
  # under a different predictor order.
  check_design(cbind(constant = 0, X))
  check_design(X[, rev(seq_len(ncol(X))), drop = FALSE])
})
