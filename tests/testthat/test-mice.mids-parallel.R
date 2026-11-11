skip_if_not_installed("future")
skip_if_not_installed("future.apply")

par_mice <- function(..., n.core = 1) {
  mice(..., print = FALSE, parallel = TRUE, n.core = n.core)
}

test_that("MIDS-PARALLEL-001: mice() stores the parallel setting and chain seeds", {
  imp <- par_mice(nhanes, m = 3, maxit = 2, seed = 1)
  expect_true(imp$parallel)
  expect_length(imp$chainSeed, 3L)
  expect_true(all(vapply(imp$chainSeed, is.integer, logical(1))))

  seq <- mice(nhanes, m = 3, maxit = 2, seed = 1, print = FALSE)
  expect_false(seq$parallel)
  expect_null(seq$chainSeed)
})

test_that("MIDS-PARALLEL-002: continuing in parallel equals a single parallel run", {
  full <- par_mice(nhanes, m = 3, maxit = 4, seed = 1)
  part <- par_mice(nhanes, m = 3, maxit = 2, seed = 1)
  cont <- mice.mids(part, maxit = 2, print = FALSE, n.core = 1)
  expect_true(cont$parallel)
  expect_identical(cont$iteration, 4)
  expect_identical(cont$imp, full$imp)
  expect_identical(cont$chainMean, full$chainMean)
  expect_identical(cont$chainVar, full$chainVar)
  expect_identical(cont$chainSeed, full$chainSeed)
})

test_that("MIDS-PARALLEL-003: maxit = 0 followed by mice.mids() equals a single run", {
  full <- par_mice(nhanes, m = 3, maxit = 3, seed = 1)
  init <- par_mice(nhanes, m = 3, maxit = 0, seed = 1)
  expect_length(init$chainSeed, 3L)
  cont <- mice.mids(init, maxit = 3, print = FALSE, n.core = 1)
  expect_identical(cont$imp, full$imp)
})

test_that("MIDS-PARALLEL-004: exact continuation holds on multisession workers", {
  skip_on_cran()
  skip_if_not(future::availableCores() >= 2L)
  skip_if(mice_is_dev_loaded(), "mice is loaded with devtools::load_all()")
  full <- par_mice(nhanes, m = 3, maxit = 4, seed = 1, n.core = 1)
  part <- par_mice(nhanes, m = 3, maxit = 2, seed = 1, n.core = 2)
  cont <- mice.mids(part, maxit = 2, print = FALSE, n.core = 2)
  expect_identical(cont$imp, full$imp)
})

test_that("MIDS-PARALLEL-005: a sequential object can continue in parallel", {
  seq <- mice(nhanes, m = 3, maxit = 2, seed = 1, print = FALSE)
  expect_false(isTRUE(seq$parallel))
  cont1 <- mice.mids(seq, maxit = 2, print = FALSE, parallel = TRUE, n.core = 1)
  cont2 <- mice.mids(seq, maxit = 2, print = FALSE, parallel = TRUE, n.core = 1)
  expect_true(cont1$parallel)
  expect_length(cont1$chainSeed, 3L)
  expect_identical(cont1$iteration, 4)
  expect_identical(cont1$imp, cont2$imp)
  expect_identical(dim(cont1$chainMean), c(4L, 4L, 3L))
  # the first two iterations are kept
  expect_identical(cont1$chainMean[, 1:2, ], seq$chainMean)
})

test_that("MIDS-PARALLEL-006: a parallel object can continue sequentially", {
  part <- par_mice(nhanes, m = 3, maxit = 2, seed = 1)
  cont1 <- mice.mids(part, maxit = 2, print = FALSE, parallel = FALSE)
  cont2 <- mice.mids(part, maxit = 2, print = FALSE, parallel = FALSE)
  expect_false(cont1$parallel)
  expect_null(cont1$chainSeed)
  expect_identical(cont1$imp, cont2$imp)
  # by default, the sequential object stays sequential
  cont3 <- mice.mids(cont1, maxit = 1, print = FALSE)
  expect_false(cont3$parallel)
})

test_that("MIDS-PARALLEL-007: objects without the new fields continue sequentially", {
  old <- mice(nhanes, m = 2, maxit = 2, seed = 1, print = FALSE)
  old$parallel <- NULL
  old$chainSeed <- NULL
  new <- mice(nhanes, m = 2, maxit = 2, seed = 1, print = FALSE)
  expect_identical(
    mice.mids(old, maxit = 1, print = FALSE)$imp,
    mice.mids(new, maxit = 1, print = FALSE)$imp
  )
})

test_that("MIDS-PARALLEL-008: combining functions keep parallel and chainSeed", {
  imp <- par_mice(nhanes, m = 2, maxit = 1, seed = 1)
  seq <- mice(nhanes, m = 2, maxit = 1, seed = 2, print = FALSE)

  r <- rbind(imp, nhanes[1:3, ])
  expect_true(r$parallel)
  expect_identical(r$chainSeed, imp$chainSeed)

  f <- filter(imp, rep(c(TRUE, FALSE), length.out = nrow(nhanes)))
  expect_true(f$parallel)
  expect_identical(f$chainSeed, imp$chainSeed)

  cb <- cbind(imp, extra = seq_len(nrow(nhanes)))
  expect_true(cb$parallel)
  expect_identical(cb$chainSeed, imp$chainSeed)

  ib <- ibind(imp, par_mice(nhanes, m = 2, maxit = 1, seed = 2))
  expect_true(ib$parallel)
  expect_length(ib$chainSeed, 4L)

  mixed <- ibind(imp, seq)
  expect_false(mixed$parallel)
  expect_null(mixed$chainSeed)
})

test_that("MIDS-PARALLEL-009: newdata works with a parallel object", {
  imp <- par_mice(nhanes[1:20, ], m = 2, maxit = 2, seed = 1)
  out <- mice.mids(imp, newdata = nhanes[21:25, ], maxit = 1, print = FALSE,
                   n.core = 1)
  expect_identical(nrow(complete(out)), 5L)
  expect_false(anyNA(complete(out)))
})
