context("blocks")


imp <- mice(nhanes, blocks = make.blocks(list(c("bmi", "chl"), "bmi", "age")), m = 10, print = FALSE)
# plot(imp)

test_that("removes variables from 'where'", {
  expect_identical(sum(imp$where[, "hyp"]), 0L)
})
