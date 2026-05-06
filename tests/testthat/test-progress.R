test_that("progressr signals correct number of updates", {
  skip_if_not_installed("progressr")

  maxit <- 2
  m <- 3
  n_blocks <- length(make.blocks(nhanes))

  conditions <- list()
  withCallingHandlers(
    progressr::with_progress({
      imp <- mice(nhanes, maxit = maxit, m = m, printFlag = FALSE)
    }, enable = TRUE),
    progression = function(cond) {
      conditions[[length(conditions) + 1L]] <<- cond
    }
  )

  updates <- Filter(
    function(cond) identical(cond$type, "update"),
    conditions
  )

  expected_steps <- maxit * m * n_blocks
  expect_equal(length(updates), expected_steps)
})

test_that("signal count matches length(blocks) with custom blocks", {
  skip_if_not_installed("progressr")

  blocks <- make.blocks(list(c("bmi", "chl"), "hyp"))
  maxit <- 2
  m <- 2

  conditions <- list()
  withCallingHandlers(
    progressr::with_progress({
      imp <- mice(nhanes, maxit = maxit, m = m, blocks = blocks, printFlag = FALSE)
    }, enable = TRUE),
    progression = function(cond) {
      conditions[[length(conditions) + 1L]] <<- cond
    }
  )

  updates <- Filter(
    function(cond) identical(cond$type, "update"),
    conditions
  )

  expected_steps <- maxit * m * length(blocks)
  expect_equal(length(updates), expected_steps)
})

test_that("no progress signals when maxit = 0", {
  skip_if_not_installed("progressr")

  conditions <- list()
  withCallingHandlers(
    progressr::with_progress({
      imp <- mice(nhanes, maxit = 0, m = 1, printFlag = FALSE)
    }, enable = TRUE),
    progression = function(cond) {
      conditions[[length(conditions) + 1L]] <<- cond
    }
  )

  updates <- Filter(
    function(cond) identical(cond$type, "update"),
    conditions
  )

  expect_equal(length(updates), 0)
})

test_that("progressr messages include iter, imp, and block info", {
  skip_if_not_installed("progressr")

  conditions <- list()
  withCallingHandlers(
    progressr::with_progress({
      imp <- mice(nhanes, maxit = 1, m = 1, printFlag = FALSE)
    }, enable = TRUE),
    progression = function(cond) {
      conditions[[length(conditions) + 1L]] <<- cond
    }
  )

  messages <- vapply(
    Filter(function(cond) identical(cond$type, "update"), conditions),
    function(cond) cond$message,
    character(1)
  )

  expect_true(length(messages) > 0)
  expect_true(all(grepl("iter=", messages)))
  expect_true(all(grepl("imp=", messages)))
  expect_true(all(grepl("block=", messages)))
})

test_that("multi-variable blocks show joined variable names in message", {
  skip_if_not_installed("progressr")

  blocks <- make.blocks(list(c("bmi", "chl"), "hyp"))

  conditions <- list()
  withCallingHandlers(
    progressr::with_progress({
      imp <- mice(nhanes, maxit = 1, m = 1, blocks = blocks, printFlag = FALSE)
    }, enable = TRUE),
    progression = function(cond) {
      conditions[[length(conditions) + 1L]] <<- cond
    }
  )

  messages <- vapply(
    Filter(function(cond) identical(cond$type, "update"), conditions),
    function(cond) cond$message,
    character(1)
  )

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

  conditions <- list()
  withCallingHandlers(
    progressr::with_progress({
      imp <- mice(nhanes, maxit = 1, m = 1, printFlag = FALSE)
    }, enable = TRUE),
    progression = function(cond) {
      conditions[[length(conditions) + 1L]] <<- cond
    }
  )

  updates <- Filter(
    function(cond) identical(cond$type, "update"),
    conditions
  )

  expect_s3_class(imp, "mids")
  expect_equal(length(updates), 0)
})

test_that("progressr signals fire with parallel = TRUE", {
  skip_if_not_installed("progressr")
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  with(future::plan(future::multisession, workers = 1), local = TRUE)

  maxit <- 1
  m <- 2
  n_blocks <- length(make.blocks(nhanes))

  conditions <- list()
  # At the time of writing this test, mice(..., paralell = TRUE) always logs two success entries and generates a warning
  suppressWarnings(withCallingHandlers(
    progressr::with_progress({
      imp <- mice(nhanes, maxit = maxit, m = m, printFlag = FALSE, parallel = TRUE)
    }, enable = TRUE),
    progression = function(cond) {
      conditions[[length(conditions) + 1L]] <<- cond
    }
  ))

  updates <- Filter(
    function(cond) identical(cond$type, "update"),
    conditions
  )

  messages <- vapply(updates, function(cond) cond$message, character(1))

  expected_steps <- maxit * m * n_blocks
  expect_equal(length(updates), expected_steps)
  expect_true(all(grepl("iter=", messages)))
  expect_true(all(grepl("imp=", messages)))
  expect_true(all(grepl("block=", messages)))
})
