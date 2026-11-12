skip_if_not_installed("progressr")

# collect the messages of the progressr steps signalled by expr
progress_messages <- function(expr) {
  old <- options(progressr.enable = TRUE)
  on.exit(options(old), add = TRUE)
  msg <- character(0)
  withCallingHandlers(
    expr,
    progression = function(cond) {
      if (identical(cond$type, "update")) {
        msg <<- c(msg, conditionMessage(cond))
      }
    }
  )
  msg
}

test_that("MICE-PROGRESS-001: mice() signals one step per iteration and chain", {
  msg <- progress_messages(
    mice(nhanes, m = 3, maxit = 2, seed = 1, print = FALSE)
  )
  expect_identical(
    msg,
    paste0("iter ", rep(1:2, each = 3), ", imp ", rep(1:3, times = 2))
  )
})

test_that("MICE-PROGRESS-002: mice.mids() continues the iteration count", {
  imp <- mice(nhanes, m = 2, maxit = 1, seed = 1, print = FALSE)
  msg <- progress_messages(mice.mids(imp, maxit = 2, print = FALSE))
  expect_identical(
    msg,
    paste0("iter ", rep(2:3, each = 2), ", imp ", rep(1:2, times = 2))
  )
})

test_that("MICE-PROGRESS-003: parallel chains signal their own steps", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  msg <- progress_messages(
    mice(
      nhanes,
      m = 3,
      maxit = 2,
      seed = 1,
      print = FALSE,
      future = TRUE
    )
  )
  expect_length(msg, 6L)
  expect_setequal(
    msg,
    paste0("iter ", rep(1:2, each = 3), ", imp ", rep(1:3, times = 2))
  )
})

test_that("MICE-PROGRESS-004: progress reporting does not change imputations", {
  imp1 <- mice(nhanes, m = 2, maxit = 2, seed = 1, print = FALSE)
  progress_messages(
    imp2 <- mice(nhanes, m = 2, maxit = 2, seed = 1, print = FALSE)
  )
  expect_identical(complete(imp1, "long"), complete(imp2, "long"))
})

test_that("MICE-PROGRESS-005: maxit = 0 signals no steps", {
  msg <- progress_messages(mice(nhanes, maxit = 0, print = FALSE))
  expect_length(msg, 0L)
})
