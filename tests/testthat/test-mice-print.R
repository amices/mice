with_mice_options <- function(opts, expr) {
  old <- options(opts)
  on.exit(options(old), add = TRUE)
  expr
}

test_that("MICE-PRINT-001: print controls the iteration history", {
  expect_output(mice(nhanes, maxit = 1, seed = 1), "iter imp variable")
  expect_silent(mice(nhanes, maxit = 1, seed = 1, print = FALSE))
})

test_that("MICE-PRINT-002: printFlag still works and takes precedence", {
  expect_silent(mice(nhanes, maxit = 1, seed = 1, printFlag = FALSE))
  expect_silent(
    mice(nhanes, maxit = 1, seed = 1, print = TRUE, printFlag = FALSE)
  )
  expect_output(
    mice(nhanes, maxit = 1, seed = 1, print = FALSE, printFlag = TRUE),
    "iter imp variable"
  )
})

test_that("MICE-PRINT-003: the options mice.print and mice.printFlag set the default", {
  with_mice_options(list(mice.print = FALSE), {
    expect_silent(mice(nhanes, maxit = 1, seed = 1))
  })
  with_mice_options(list(mice.print = NULL, mice.printFlag = FALSE), {
    expect_silent(mice(nhanes, maxit = 1, seed = 1))
  })
  with_mice_options(list(mice.print = TRUE, mice.printFlag = FALSE), {
    expect_output(mice(nhanes, maxit = 1, seed = 1), "iter imp variable")
  })
})

test_that("MICE-PRINT-004: mice.mids() accepts print and printFlag", {
  imp <- mice(nhanes, maxit = 1, seed = 1, print = FALSE)
  expect_output(mice.mids(imp), "iter imp variable")
  expect_silent(mice.mids(imp, print = FALSE))
  expect_silent(mice.mids(imp, printFlag = FALSE))
  expect_silent(mice.mids(imp, print = TRUE, printFlag = FALSE))
})

test_that("MICE-PRINT-005: print does not change the imputations", {
  a <- mice(nhanes, maxit = 2, seed = 1, print = FALSE)
  b <- suppressWarnings(capture.output(
    imp <- mice(nhanes, maxit = 2, seed = 1, print = TRUE)
  ))
  c <- mice(nhanes, maxit = 2, seed = 1, printFlag = FALSE)
  expect_identical(a$imp, imp$imp)
  expect_identical(a$imp, c$imp)
})
