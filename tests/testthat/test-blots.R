context("blots")

# global change of donors argument
blocks1 <- name.blocks(list(c("bmi", "chl"), "hyp"))
imp0 <- mice(nhanes, blocks = blocks1, donors = 10, m = 1, maxit = 1, print = FALSE)

# vary donors, depending on block
blots1 <- list(B1 = list(donors = 10), hyp = list(donors = 1))
imp1 <- mice(nhanes, blocks = blocks1, blots = blots1, m = 1, maxit = 1, print = FALSE)

test_that("errors when mixing same global and local argument", {
  
  expect_error(mice(nhanes, blocks = blocks1, blots = blots1, donors = 7, print = FALSE),
               'formal argument "donors" matched by multiple actual arguments')
})
