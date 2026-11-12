skip_if_not_installed("future")
skip_if_not_installed("future.apply")

fut_mice <- function(...) {
  mice(..., print = FALSE, future = TRUE)
}

test_that("MIDS-PARALLEL-001: mice() stores the future info and chain seeds", {
  imp <- fut_mice(nhanes, m = 3, maxit = 2, seed = 1)
  expect_false(is.null(imp$future))
  expect_length(imp$chainSeed, 3L)
  expect_true(all(vapply(imp$chainSeed, is.integer, logical(1))))

  seq <- mice(nhanes, m = 3, maxit = 2, seed = 1, print = FALSE)
  expect_null(seq$future)
  expect_null(seq$chainSeed)
})

test_that("MIDS-PARALLEL-002: continuing as futures equals a single future run", {
  full <- fut_mice(nhanes, m = 3, maxit = 4, seed = 1)
  part <- fut_mice(nhanes, m = 3, maxit = 2, seed = 1)
  cont <- mice.mids(part, maxit = 2, print = FALSE)
  expect_false(is.null(cont$future))
  expect_identical(cont$iteration, 4)
  expect_identical(cont$imp, full$imp)
  expect_identical(cont$chainMean, full$chainMean)
  expect_identical(cont$chainVar, full$chainVar)
  expect_identical(cont$chainSeed, full$chainSeed)
})

test_that("MIDS-PARALLEL-003: maxit = 0 followed by mice.mids() equals a single run", {
  full <- fut_mice(nhanes, m = 3, maxit = 3, seed = 1)
  init <- fut_mice(nhanes, m = 3, maxit = 0, seed = 1)
  expect_length(init$chainSeed, 3L)
  cont <- mice.mids(init, maxit = 3, print = FALSE)
  expect_identical(cont$imp, full$imp)
})

test_that("MIDS-PARALLEL-004: exact continuation holds on multisession workers", {
  skip_on_cran()
  skip_if_not(future::availableCores() >= 2L)
  skip_if(mice_is_dev_loaded(), "mice is loaded with devtools::load_all()")
  full <- fut_mice(nhanes, m = 3, maxit = 4, seed = 1)
  cont <- with_workers({
    part <- fut_mice(nhanes, m = 3, maxit = 2, seed = 1)
    mice.mids(part, maxit = 2, print = FALSE)
  })
  expect_identical(cont$future$workers, 2L)
  expect_identical(cont$imp, full$imp)
})

test_that("MIDS-PARALLEL-005: a sequential object can continue as futures", {
  seq <- mice(nhanes, m = 3, maxit = 2, seed = 1, print = FALSE)
  expect_null(seq$future)
  cont1 <- mice.mids(seq, maxit = 2, print = FALSE, future = TRUE)
  cont2 <- mice.mids(seq, maxit = 2, print = FALSE, future = TRUE)
  expect_false(is.null(cont1$future))
  expect_length(cont1$chainSeed, 3L)
  expect_identical(cont1$iteration, 4)
  expect_identical(cont1$imp, cont2$imp)
  expect_identical(dim(cont1$chainMean), c(4L, 4L, 3L))
  # the first two iterations are kept
  expect_identical(cont1$chainMean[, 1:2, ], seq$chainMean)
})

test_that("MIDS-PARALLEL-006: a future object can continue sequentially", {
  part <- fut_mice(nhanes, m = 3, maxit = 2, seed = 1)
  cont1 <- mice.mids(part, maxit = 2, print = FALSE, future = FALSE)
  cont2 <- mice.mids(part, maxit = 2, print = FALSE, future = FALSE)
  expect_null(cont1$future)
  expect_null(cont1$chainSeed)
  expect_identical(cont1$imp, cont2$imp)
  # by default, the sequential object stays sequential
  cont3 <- mice.mids(cont1, maxit = 1, print = FALSE)
  expect_null(cont3$future)
})

test_that("MIDS-PARALLEL-007: objects without the new fields continue sequentially", {
  old <- mice(nhanes, m = 2, maxit = 2, seed = 1, print = FALSE)
  old$future <- NULL
  old$chainSeed <- NULL
  new <- mice(nhanes, m = 2, maxit = 2, seed = 1, print = FALSE)
  expect_identical(
    mice.mids(old, maxit = 1, print = FALSE)$imp,
    mice.mids(new, maxit = 1, print = FALSE)$imp
  )
})

test_that("MIDS-PARALLEL-008: combining functions keep future and chainSeed", {
  imp <- fut_mice(nhanes, m = 2, maxit = 1, seed = 1)
  seq <- mice(nhanes, m = 2, maxit = 1, seed = 2, print = FALSE)

  r <- rbind(imp, nhanes[1:3, ])
  expect_false(is.null(r$future))
  expect_identical(r$chainSeed, imp$chainSeed)

  f <- filter(imp, rep(c(TRUE, FALSE), length.out = nrow(nhanes)))
  expect_false(is.null(f$future))
  expect_identical(f$chainSeed, imp$chainSeed)

  cb <- cbind(imp, extra = seq_len(nrow(nhanes)))
  expect_false(is.null(cb$future))
  expect_identical(cb$chainSeed, imp$chainSeed)

  ib <- ibind(imp, fut_mice(nhanes, m = 2, maxit = 1, seed = 2))
  expect_false(is.null(ib$future))
  expect_length(ib$chainSeed, 4L)

  mixed <- ibind(imp, seq)
  expect_null(mixed$future)
  expect_null(mixed$chainSeed)
})

test_that("MIDS-PARALLEL-009: newdata works with a future object", {
  imp <- fut_mice(nhanes[1:20, ], m = 2, maxit = 2, seed = 1)
  out <- mice.mids(imp, newdata = nhanes[21:25, ], maxit = 1, print = FALSE)
  expect_identical(nrow(complete(out)), 5L)
  expect_false(anyNA(complete(out)))
})

test_that("MIDS-PARALLEL-010: objects from mice 3.19.13 continue as futures", {
  full <- fut_mice(nhanes, m = 3, maxit = 4, seed = 1)
  old <- fut_mice(nhanes, m = 3, maxit = 2, seed = 1)
  old$future <- NULL
  old$parallel <- TRUE
  cont <- mice.mids(old, maxit = 2, print = FALSE)
  expect_false(is.null(cont$future))
  expect_identical(cont$imp, full$imp)
})

test_that("MIDS-PARALLEL-011: the future field describes the run", {
  imp <- fut_mice(nhanes, m = 3, maxit = 1, seed = 1)
  expect_identical(imp$future$plan, plan.name())
  expect_identical(imp$future$workers, 1L)
  expect_length(imp$future$pid, 3L)
  expect_true(all(imp$future$pid == Sys.getpid()))
})

test_that("MIDS-PARALLEL-012: removed arguments give a warning", {
  imp <- fut_mice(nhanes, m = 2, maxit = 1, seed = 1)
  expect_warning(mice.mids(imp, n.core = 2, print = FALSE), "no longer supported")
  expect_warning(
    mice.mids(imp, parallel = TRUE, print = FALSE),
    "no longer supported"
  )
})
