
context("filter.mids")

imp <- mice(nhanes, m = 2, maxit = 1, print = FALSE, seed = 1)

imp_l <- imp
class(imp_l) <- "list"

test_that("throws error", {
  expect_error(filter.mids("mids"))
  expect_error(filter.mids(complete(imp)))
  expect_error(filter.mids(imp_l))
  expect_error(filter.mids(imp, "TRUE"), "must be logical")
  expect_error(filter.mids(imp, rep(1, nrow(nhanes))), "must be logical")
  expect_error(filter.mids(imp, TRUE), "must be of same length")
})


imp_f <- filter.mids(imp, include = c(rep(TRUE, 13), rep(FALSE, 12)))

test_that("filtered mids is subset", {
  expect_equal(complete(imp_f), complete(imp)[1:13, ])
  expect_equal(imp_f$ignore, imp$ignore[1:13])
  expect_equal(imp_f$where, imp$where[1:13, ])
  expect_equal(imp_f$nmis, colSums(is.na(imp$data[1:13, ])))
  expect_null(imp_f$chainMean)
})


imp_fa <- filter.mids(imp, include = rep(TRUE, nrow(nhanes)))

imp2 <- mice.mids(imp, maxit = 1, printFlag = FALSE)
imp_fa2 <- mice.mids(imp_fa, maxit = 1, printFlag = FALSE)

test_that("other elements of mids are left unchanged", {
  expect_equal(complete(imp2), complete(imp_fa2))
})


