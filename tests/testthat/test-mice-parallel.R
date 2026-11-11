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
  skip_if(mice_is_dev_loaded(), "mice is loaded with devtools::load_all()")
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

test_that("MICE-PARALLEL-012: user-defined methods and their helpers reach the chains", {
  assign("helper.par012", function(y, ry, x, ...) {
    mice.impute.pmm(y, ry, x, ...)
  }, envir = globalenv())
  assign("mice.impute.par012", function(y, ry, x, ...) {
    helper.par012(y, ry, x, ...)
  }, envir = globalenv())
  on.exit(
    rm(list = c("helper.par012", "mice.impute.par012"), envir = globalenv()),
    add = TRUE
  )
  meth <- make.method(nhanes)
  meth["bmi"] <- "par012"
  imp1 <- mice(
    nhanes,
    m = 2,
    maxit = 2,
    method = meth,
    seed = 1,
    print = FALSE,
    parallel = TRUE,
    n.core = 1
  )
  expect_false(anyNA(complete(imp1)))

  skip_on_cran()
  skip_if_not(future::availableCores() >= 2L)
  skip_if(mice_is_dev_loaded(), "mice is loaded with devtools::load_all()")
  imp2 <- mice(
    nhanes,
    m = 2,
    maxit = 2,
    method = meth,
    seed = 1,
    print = FALSE,
    parallel = TRUE,
    n.core = 2
  )
  expect_identical(complete(imp1, "long"), complete(imp2, "long"))
})

test_that("MICE-PARALLEL-013: errors name the iteration, chain and variable", {
  # fails at the fifth call: chain 2, iteration 1, since each chain
  # runs over all iterations before the next chain starts
  assign("calls.par013", 0L, envir = globalenv())
  assign("mice.impute.par013", function(y, ry, x, ...) {
    calls.par013 <<- calls.par013 + 1L
    if (calls.par013 == 5L) {
      stop("failure in par013")
    }
    mice.impute.pmm(y, ry, x, ...)
  }, envir = globalenv())
  on.exit(
    rm(list = c("calls.par013", "mice.impute.par013"), envir = globalenv()),
    add = TRUE
  )
  meth <- make.method(nhanes)
  meth["bmi"] <- "par013"
  # future.apply warns that it cancels the other chains
  e <- suppressWarnings(tryCatch(
    mice(
      nhanes,
      m = 3,
      maxit = 4,
      method = meth,
      seed = 1,
      print = FALSE,
      parallel = TRUE,
      n.core = 1
    ),
    error = identity
  ))
  expect_s3_class(e, "mice_sampler_error")
  expect_identical(e$iteration, 1L)
  expect_identical(e$imputation, 2L)
  expect_identical(e$variable, "bmi")
  expect_identical(e$method, "par013")
  expect_match(conditionMessage(e), "iteration 1, imputation 2, variable 'bmi'")
  expect_match(conditionMessage(e$parent), "failure in par013")
})

test_that("MICE-PARALLEL-014: post can use the imputation number as chain", {
  post <- make.post(nhanes)
  post["bmi"] <- "imp[[j]][, i] <- 100 * chain"
  imp_s <- mice(nhanes, m = 3, maxit = 1, post = post, seed = 1, print = FALSE)
  imp_p <- mice(
    nhanes,
    m = 3,
    maxit = 1,
    post = post,
    seed = 1,
    print = FALSE,
    parallel = TRUE,
    n.core = 1
  )
  expected <- matrix(
    rep(c(100, 200, 300), each = sum(is.na(nhanes$bmi))),
    ncol = 3
  )
  expect_equal(unname(as.matrix(imp_s$imp$bmi)), expected)
  expect_equal(unname(as.matrix(imp_p$imp$bmi)), expected)
})
