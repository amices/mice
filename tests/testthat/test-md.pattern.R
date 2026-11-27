test_that("MD-PATTERN-001: patterns run as expected", {
  # boys
  expect_silent(md.pattern(boys, plot = FALSE))
  # nhanes
  expect_silent(md.pattern(nhanes, plot = FALSE))
  # one whole column missing (single pattern) - should not produce output
  expect_silent(md.pattern(cbind(na.omit(nhanes), NA), plot = FALSE))
  # no missings (no pattern) - should produce output
  expect_output(md.pattern(na.omit(nhanes), plot = FALSE))
  # feed single column - expect error
  expect_error(md.pattern(nhanes$bmi))
  # feed other than dataframe/matrix - expect error
  expect_error(md.pattern(as.list(nhanes)))
  #
})

test_that("MD-PATTERN-002: one row retains the pattern and margins", {
  complete <- data.frame(a = 1, b = 2)
  expected <- rbind(c(1, 1, 0), c(0, 0, 0))
  dimnames(expected) <- list(c("1", ""), c("a", "b", ""))
  expect_output(
    actual <- md.pattern(complete, plot = FALSE),
    "completely observed"
  )
  expect_equal(actual, expected)

  incomplete <- data.frame(a = NA_real_, b = 2)
  expected <- rbind(c(1, 0, 1), c(0, 1, 1))
  dimnames(expected) <- list(c("1", ""), c("b", "a", ""))
  expect_equal(md.pattern(incomplete, plot = FALSE), expected)
})
