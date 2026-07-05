test_that("polr for only gen works", {
  imp1 <- mice(boys, blocks = list("gen"), print = FALSE, m = 1, maxit = 1)
  expect_true(is.mids(imp1))
})
