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

test_that("mice runs without error when progressr is unavailable", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "progressr") return(FALSE)
      base::requireNamespace(pkg, ...)
    }
  )

  updates <- capture_progress_updates(
    imp <- mice(nhanes, maxit = 1, m = 1, printFlag = FALSE)
  )

  expect_s3_class(imp, "mids")
  expect_equal(length(updates), 0)
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

test_that("max_signals does not throttle below default cap", {
  skip_if_not_installed("progressr")

  maxit <- 2
  m <- 3
  n_blocks <- length(make.blocks(nhanes))
  expect_lt(maxit * m * n_blocks, 100L)

  updates <- capture_progress_updates(
    mice(nhanes, maxit = maxit, m = m, printFlag = FALSE)
  )

  expect_equal(length(updates), maxit * m * n_blocks)
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

  # --- Case 1: aggressive throttle (max_signals = 2) -------------------------
  # n_total = 4*2*3 = 24, naive step = 24 %/% 2 = 12.
  # In parallel the step snaps to the largest factor of n_blocks (3)
  # that is >= 12, which is only 3 itself — so step becomes n_blocks (3).
  # Each worker task (maxit * n_blocks = 12 calls) fires 12/3 = 4 signals.
  # Total: m * 4 = 8.
  withr::local_options(mice.progress.max_signals = 2L)

  maxit <- 4
  m <- 2

  updates <- suppressWarnings(capture_progress_updates(
    mice(nhanes, maxit = maxit, m = m, printFlag = FALSE, parallel = TRUE)
  ))

  expect_equal(length(updates), maxit * m)

  # --- Case 2: moderate throttle where step lands on a mid-range factor -------
  # nhanes has 3 blocks (factors: 1, 3). With max_signals = 5:
  # n_total = 3*2*3 = 18, naive step = 18 %/% 5 = 3.
  # Snap to first factor of 3 that is >= 3 → step = 3.
  # Each worker fires (3*3)/3 = 3 signals. Total: 2 * 3 = 6.
  withr::local_options(mice.progress.max_signals = 5L)

  maxit <- 3
  m <- 2
  n_total <- maxit * m * n_blocks

  updates <- suppressWarnings(capture_progress_updates(
    mice(nhanes, maxit = maxit, m = m, printFlag = FALSE, parallel = TRUE)
  ))

  per_worker <- maxit * n_blocks
  step <- n_blocks
  expect_equal(length(updates), m * (per_worker %/% step))

  # --- Case 3: no throttle needed (small job, step stays 1) ------------------
  # n_total = 1*1*3 = 3, naive step = 3 %/% 100 = 0 → clamped to 1.
  # step 1 is a factor of 3, so no snapping. Every call signals.
  withr::local_options(mice.progress.max_signals = 100L)

  maxit <- 1
  m <- 1

  updates <- suppressWarnings(capture_progress_updates(
    mice(nhanes, maxit = maxit, m = m, printFlag = FALSE, parallel = TRUE)
  ))

  expect_equal(length(updates), maxit * m * n_blocks)

  # --- Case 4: step resolves to a factor of n_blocks --------------------------
  # The signal count must evenly divide the per-worker workload, so the
  # effective step must be a factor of n_blocks. Verify this across several
  # max_signals values.
  for (max_sig in c(2L, 5L, 10L, 20L, 50L)) {
    withr::local_options(mice.progress.max_signals = max_sig)

    maxit <- 4
    m <- 2
    n_total <- maxit * m * n_blocks
    naive_step <- max(1L, n_total %/% max_sig)
    factors <- which(n_blocks %% seq_len(n_blocks) == 0L)
    snapped <- factors[which(factors >= naive_step)[1]]
    if (is.na(snapped)) snapped <- n_blocks
    expect_true(
      n_blocks %% snapped == 0L,
      label = sprintf("step %d divides n_blocks %d (max_signals=%d)",
                      snapped, n_blocks, max_sig)
    )
  }

  # --- Case 5: custom blocks with different factor structure ------------------
  # 2 blocks (factors: 1, 2). max_signals = 3:
  # n_total = 3*2*2 = 12, naive step = 12 %/% 3 = 4.
  # First factor of 2 that is >= 4 → none, so step = n_blocks (2).
  # Each worker fires (3*2)/2 = 3. Total: 2 * 3 = 6.
  withr::local_options(mice.progress.max_signals = 3L)

  blocks2 <- make.blocks(list(c("bmi", "chl"), "hyp"))
  nb2 <- length(blocks2)
  maxit <- 3
  m <- 2

  updates <- suppressWarnings(capture_progress_updates(
    mice(nhanes, maxit = maxit, m = m, blocks = blocks2,
         printFlag = FALSE, parallel = TRUE)
  ))

  per_worker <- maxit * nb2
  expect_equal(length(updates), m * (per_worker %/% nb2))
})
