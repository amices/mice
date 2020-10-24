
context("filter.mids")

imp <- mice(nhanes, m = 2, maxit = 1, print = FALSE, seed = 1)

imp_l <- imp
class(imp_l) <- "list"

test_that("throws error", {
  # outcommented first two tests because they also throw a
  # deprecated filter_ warning
  # expect_error(filter("mids"))
  # expect_error(filter(imp_l))
  expect_error(filter(imp, "TRUE"))
  expect_error(filter(imp, rep(1, nrow(nhanes))))
})


imp_f <- filter(imp, c(rep(TRUE, 13), rep(FALSE, 12)))

test_that("filtered mids is subset", {
  expect_equal(complete(imp_f), complete(imp)[1:13, ])
  expect_equal(imp_f$ignore, imp$ignore[1:13])
  expect_equal(imp_f$where, imp$where[1:13, ])
  expect_equal(imp_f$nmis, colSums(is.na(imp$data[1:13, ])))
  expect_null(imp_f$chainMean)
})


imp_fa <- filter(imp, rep(TRUE, nrow(nhanes)))

imp2 <- mice.mids(imp, maxit = 1, printFlag = FALSE)
imp_fa2 <- mice.mids(imp_fa, maxit = 1, printFlag = FALSE)

test_that("other elements of mids are left unchanged", {
  expect_equal(complete(imp2), complete(imp_fa2))
})


