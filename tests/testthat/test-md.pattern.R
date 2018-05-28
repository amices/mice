context("md.pattern")

test_that("patterns run as expected", {
  #boys
  expect_silent(md.pattern(boys, plot = FALSE))
  #nhanes
  expect_silent(md.pattern(nhanes, plot = FALSE))
  #one whole column missing (single pattern) - should not produce output
  expect_silent(md.pattern(cbind(na.omit(nhanes), NA), plot = FALSE))
  #no missings (no pattern) - should produce output
  expect_output(md.pattern(na.omit(nhanes), plot = FALSE))
  #feed single column - expect error
  expect_error(md.pattern(nhanes$bmi))
  #feed other than dataframe/matrix - expect error
  expect_error(md.pattern(as.list(nhanes)))
  #
})
