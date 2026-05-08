capture_progress_updates <- function(expr) {
  conditions <- list()
  withCallingHandlers(
    progressr::with_progress(expr, enable = TRUE),
    progression = function(cond) {
      conditions[[length(conditions) + 1L]] <<- cond
    }
  )
  Filter(function(cond) identical(cond$type, "update"), conditions)
}

test_that("progressr signals correct number of updates", {
  skip_if_not_installed("progressr")

  maxit <- 2
  m <- 3
  n_blocks <- length(make.blocks(nhanes))

  updates <- capture_progress_updates(
    mice(nhanes, maxit = maxit, m = m, printFlag = FALSE)
  )

  expected_steps <- maxit * m * n_blocks
  expect_equal(length(updates), expected_steps)
})

test_that("signal count matches length(blocks) with custom blocks", {
  skip_if_not_installed("progressr")

  blocks <- make.blocks(list(c("bmi", "chl"), "hyp"))
  maxit <- 2
  m <- 2

  updates <- capture_progress_updates(
    mice(nhanes, maxit = maxit, m = m, blocks = blocks, printFlag = FALSE)
  )

  expected_steps <- maxit * m * length(blocks)
  expect_equal(length(updates), expected_steps)
})

test_that("no progress signals when maxit = 0", {
  skip_if_not_installed("progressr")

  updates <- capture_progress_updates(
    mice(nhanes, maxit = 0, m = 1, printFlag = FALSE)
  )

  expect_equal(length(updates), 0)
})

test_that("progressr messages include iter, imp, and block info", {
  skip_if_not_installed("progressr")

  updates <- capture_progress_updates(
    mice(nhanes, maxit = 1, m = 1, printFlag = FALSE)
  )

  messages <- vapply(updates, function(cond) cond$message, character(1))

  expect_true(length(messages) > 0)
  expect_true(all(grepl("iter=", messages)))
  expect_true(all(grepl("imp=", messages)))
  expect_true(all(grepl("block=", messages)))
})

test_that("multi-variable blocks show joined variable names in message", {
  skip_if_not_installed("progressr")

  blocks <- make.blocks(list(c("bmi", "chl"), "hyp"))

  updates <- capture_progress_updates(
    mice(nhanes, maxit = 1, m = 1, blocks = blocks, printFlag = FALSE)
  )

  messages <- vapply(updates, function(cond) cond$message, character(1))

  multi_block_msgs <- grep("block=bmi chl", messages, value = TRUE)
  expect_true(length(multi_block_msgs) > 0)
})

test_that("mice.progress.max_signals throttles updates for large jobs", {
  skip_if_not_installed("progressr")

  withr::local_options(mice.progress.max_signals = 5L)

  maxit <- 5
  m <- 4
  n_blocks <- length(make.blocks(nhanes))
  n_total <- maxit * m * n_blocks

  updates <- capture_progress_updates(
    mice(nhanes, maxit = maxit, m = m, printFlag = FALSE)
  )

  step <- max(1L, n_total %/% 5L)
  expect_equal(length(updates), n_total %/% step)
  expect_lte(length(updates), 5L)
})

test_that("progressr signals fire when parallel = TRUE", {
  skip_if_not_installed("progressr")
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  with(future::plan(future::multisession, workers = 1), local = TRUE)

  maxit <- 1
  m <- 2
  n_blocks <- length(make.blocks(nhanes))

  updates <- suppressWarnings(capture_progress_updates(
    mice(nhanes, maxit = maxit, m = m, printFlag = FALSE, parallel = TRUE)
  ))

  messages <- vapply(updates, function(cond) cond$message, character(1))

  expected_steps <- maxit * m * n_blocks
  expect_equal(length(updates), expected_steps)
  expect_true(all(grepl("iter=", messages)))
  expect_true(all(grepl("imp=", messages)))
  expect_true(all(grepl("block=", messages)))
})

test_that("parallel throttling caps step at per-worker workload", {
  skip_if_not_installed("progressr")
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  with(future::plan(future::multisession, workers = 1), local = TRUE)
  n_blocks <- length(make.blocks(nhanes))

  # --- Case 1: step > n_blocks (snaps to n_blocks) ----------------------------
  # n_total = 3*1*4 = 12, naive step = 12 %/% 2 = 6 > n_blocks (4).
  # No factor of 4 >= 6, so step caps at n_blocks (4).
  # Each worker (maxit * n_blocks = 4 calls) fires 4/4 = 1 signal.
  # Total: m * 1 = 3.
  withr::local_options(mice.progress.max_signals = 2L)

  maxit <- 1
  m <- 3

  updates <- suppressWarnings(capture_progress_updates(
    mice(nhanes, maxit = maxit, m = m, printFlag = FALSE, parallel = TRUE)
  ))

  per_worker <- maxit * n_blocks
  expect_equal(length(updates), m * (per_worker %/% n_blocks))

  # --- Case 2: step == n_blocks ------------------------------------------------
  # n_total = 2*2*4 = 16, naive step = 16 %/% 4 = 4 = n_blocks (4).
  # Step already equals n_blocks, no snapping needed.
  # Each worker (maxit * n_blocks = 8 calls) fires 8/4 = 2 signals.
  # Total: m * 2 = 4.
  withr::local_options(mice.progress.max_signals = 4L)

  maxit <- 2
  m <- 2

  updates <- suppressWarnings(capture_progress_updates(
    mice(nhanes, maxit = maxit, m = m, printFlag = FALSE, parallel = TRUE)
  ))

  per_worker <- maxit * n_blocks
  expect_equal(length(updates), m * (per_worker %/% n_blocks))

  # --- Case 3a: step < n_blocks (snaps to n_blocks) ----------------------------
  # n_blocks = 4, factors: {1, 2, 4}. max_signals = 4:
  # n_total = 3*1*4 = 12, naive step = 12 %/% 4 = 3 < n_blocks.
  # Smallest factor of 4 >= 3 is 4 → step = n_blocks.
  # Each worker (1*4 = 4 calls) fires 4/4 = 1 signal. Total: 3*1 = 3.
  withr::local_options(mice.progress.max_signals = 4L)

  maxit <- 1
  m <- 3

  updates <- suppressWarnings(capture_progress_updates(
    mice(nhanes, maxit = maxit, m = m, printFlag = FALSE, parallel = TRUE)
  ))

  per_worker <- maxit * n_blocks
  expect_equal(length(updates), m * (per_worker %/% n_blocks))

  # --- Case 3b: step < n_blocks (snaps to intermediate factor) ----------------
  # n_blocks = 4, factors: {1, 2, 4}. max_signals = 3:
  # n_total = 2*1*4 = 8, naive step = 8 %/% 3 = 2 < 4.
  # Factor 2 >= 2 → step = 2, an intermediate factor.
  # Each worker (1*4 = 4 calls) fires 4/2 = 2 signals. Total: 2*2 = 4.
  withr::local_options(mice.progress.max_signals = 3L)

  maxit <- 1
  m <- 2

  updates <- suppressWarnings(capture_progress_updates(
    mice(nhanes, maxit = maxit, m = m, printFlag = FALSE, parallel = TRUE)
  ))

  per_worker <- maxit * n_blocks
  step <- 2L
  expect_equal(length(updates), m * (per_worker %/% step))
})
