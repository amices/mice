blocks1 <- name.blocks(list(c("bmi", "chl"), "hyp"))
blots1 <- list(B1 = list(donors = 10), hyp = list(donors = 1))

test_that("BLOTS-001: errors when mixing same global and local argument", {
  expect_error(
    mice(nhanes, blocks = blocks1, blots = blots1, donors = 7, print = FALSE),
    'formal argument "donors" matched by multiple actual arguments'
  )
})
