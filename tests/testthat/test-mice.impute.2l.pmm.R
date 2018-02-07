context("miceadds::mice.impute.2l.pmm")

suppressPackageStartupMessages(require(miceadds, warn.conflicts = FALSE, quietly = TRUE))

d <- brandsma[1:200, c("school", "langpost")]
pred <- make.predictorMatrix(d)
pred["langpost", "school"] <- -2

test_that("runs empty model", {
  expect_silent(mice(d, method = "2l.pmm", print = FALSE, 
                            pred = pred, m = 1, maxit = 1, seed = 1))
  expect_false(anyNA(complete(imp1)))
})

imp0 <- mice(d, method = "2l.pmm", print = FALSE, pred = pred, 
             m = 1, maxit = 1, seed = 1)
imp1 <- mice(d, method = "2l.pmm", print = FALSE, pred = pred, 
             m = 1, maxit = 1, seed = 1,
             match_sampled_pars = FALSE)
imp2 <- mice(d, method = "2l.pmm", print = FALSE, pred = pred, 
            m = 1, maxit = 1, seed = 1, 
            match_sampled_pars = TRUE)
test_that("donors and match_sampled_pars produce different imputes", {
  expect_false(identical(complete(imp1), complete(imp2)))
  expect_false(identical(complete(imp0), complete(imp1)))
  expect_false(identical(complete(imp0), complete(imp2)))
})
