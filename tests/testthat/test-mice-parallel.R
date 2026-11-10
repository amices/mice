skip_if_not_installed("future")
skip_if_not_installed("future.apply")

imp_seq <- mice(nhanes, m = 3, maxit = 3, seed = 1, print = FALSE)
imp_par <- mice(
  nhanes,
  m = 3,
  maxit = 3,
  seed = 1,
  print = FALSE,
  parallel = TRUE,
  n.core = 1
)

test_that("MICE-PARALLEL-001: parallel = TRUE returns a complete mids object", {
  expect_s3_class(imp_par, "mids")
  expect_identical(imp_par$m, imp_seq$m)
  expect_identical(imp_par$iteration, 3)
  expect_false(anyNA(complete(imp_par, "long")))
  expect_identical(names(imp_par$imp), names(imp_seq$imp))
  expect_identical(
    lapply(imp_par$imp, dimnames),
    lapply(imp_seq$imp, dimnames)
  )
})

test_that("MICE-PARALLEL-002: chainMean and chainVar have the sequential layout", {
  expect_identical(dimnames(imp_par$chainMean), dimnames(imp_seq$chainMean))
  expect_identical(dimnames(imp_par$chainVar), dimnames(imp_seq$chainVar))
  expect_identical(
    is.na(imp_par$chainMean),
    is.na(imp_seq$chainMean)
  )
})

test_that("MICE-PARALLEL-003: parallel imputations are reproducible with a seed", {
  imp2 <- mice(
    nhanes,
    m = 3,
    maxit = 3,
    seed = 1,
    print = FALSE,
    parallel = TRUE,
    n.core = 1
  )
  expect_identical(complete(imp_par, "long"), complete(imp2, "long"))
  expect_identical(imp_par$chainMean, imp2$chainMean)
})

test_that("MICE-PARALLEL-004: parallel = FALSE is not affected", {
  imp2 <- mice(
    nhanes,
    m = 3,
    maxit = 3,
    seed = 1,
    print = FALSE,
    parallel = FALSE
  )
  expect_identical(complete(imp_seq, "long"), complete(imp2, "long"))
})

test_that("MICE-PARALLEL-005: events logged within chains are kept", {
  set.seed(3)
  d <- data.frame(
    y = c(rnorm(3), rep(NA, 5)),
    x1 = rnorm(8),
    x2 = rnorm(8),
    x3 = rnorm(8),
    x4 = rnorm(8)
  )
  s <- suppressWarnings(mice(d, m = 3, maxit = 2, seed = 1, print = FALSE))
  p <- suppressWarnings(mice(
    d,
    m = 3,
    maxit = 2,
    seed = 1,
    print = FALSE,
    parallel = TRUE,
    n.core = 1
  ))
  expect_identical(
    p$loggedEvents[, c("it", "im", "dep", "meth")],
    s$loggedEvents[, c("it", "im", "dep", "meth")]
  )
})

test_that("MICE-PARALLEL-006: events logged during setup are kept", {
  d <- nhanes
  d$age2 <- d$age
  s <- suppressWarnings(mice(d, m = 2, maxit = 1, seed = 2, print = FALSE))
  p <- suppressWarnings(mice(
    d,
    m = 2,
    maxit = 1,
    seed = 2,
    print = FALSE,
    parallel = TRUE,
    n.core = 1
  ))
  expect_identical(p$loggedEvents, s$loggedEvents)
})

test_that("MICE-PARALLEL-007: printFlag reports the total time", {
  expect_output(
    mice(nhanes, m = 2, maxit = 1, seed = 1, parallel = TRUE, n.core = 1),
    "Imputed 2 chains in parallel, 1 iterations"
  )
})

test_that("MICE-PARALLEL-008: maxit = 0 returns the initial imputations", {
  imp0 <- mice(
    nhanes,
    m = 2,
    maxit = 0,
    seed = 1,
    print = FALSE,
    parallel = TRUE,
    n.core = 1
  )
  imp0_seq <- mice(nhanes, m = 2, maxit = 0, seed = 1, print = FALSE)
  expect_identical(imp0$iteration, 0)
  expect_identical(imp0$imp, imp0_seq$imp)
})

test_that("MICE-PARALLEL-009: invalid n.core is rejected", {
  expect_error(
    mice(nhanes, parallel = TRUE, n.core = 0, print = FALSE),
    "positive integer"
  )
  expect_error(
    mice(nhanes, parallel = TRUE, n.core = c(1, 2), print = FALSE),
    "positive integer"
  )
})

test_that("MICE-PARALLEL-010: the future plan set by mice() is reset", {
  oldplan <- future::plan(future::sequential)
  on.exit(future::plan(oldplan), add = TRUE)
  mice(nhanes, m = 2, maxit = 1, print = FALSE, parallel = TRUE, n.core = 1)
  expect_true(inherits(future::plan(), "sequential"))
})

test_that("MICE-PARALLEL-011: results do not depend on the number of workers", {
  skip_on_cran()
  skip_if_not(future::availableCores() >= 2L)
  # workers load the installed mice, not the one from devtools::load_all()
  skip_if(
    isNamespaceLoaded("pkgload") && pkgload::is_dev_package("mice"),
    "mice is loaded with pkgload"
  )
  imp2 <- mice(
    nhanes,
    m = 3,
    maxit = 3,
    seed = 1,
    print = FALSE,
    parallel = TRUE,
    n.core = 2
  )
  expect_identical(complete(imp_par, "long"), complete(imp2, "long"))
  expect_identical(imp_par$chainMean, imp2$chainMean)
})
