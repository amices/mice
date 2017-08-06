
library(testthat)
library(mice)

data("tract2221", package= "imputeMulti")

test_that("extracting imputations works", {
  mice_ch_eq <- mice(tract2221, m=4, method= "polyreg")
  
  imputations1 <- extract_imputations(tract2221, mice_ch_eq$imp, j= 1)
  imputations2 <- extract_imputations(tract2221, mice_ch_eq$imp, j= 2)
  imputations3 <- extract_imputations(tract2221, mice_ch_eq$imp, j= 3)
  imputations4 <- extract_imputations(tract2221, mice_ch_eq$imp, j= 4)
  
  # check attributes; only 1 needed
  expect_equal(nrow(tract2221), nrow(imputations1))
  expect_equal(ncol(tract2221), ncol(imputations1))
  expect_equal(names(tract2221), names(imputations1))
  expect_equal(class(tract2221), class(imputations1))
  
  # check extractions
  expect_false(sum(is.na(tract2221)) == 0)
  expect_equal(sum(is.na(imputations1)), 0)
  expect_true(all.equal(unlist(lapply(tract2221, class)),
                        unlist(lapply(imputations1, class))))
  
  # manually check a few columns missing values 
  idx_t <- which(is.na(tract2221[[5]]))
  imps  <- imputations1[[5]][idx_t]
  
  expect_equal(sum(is.na(imps)), 0)
  
  idx_t <- which(is.na(tract2221[[6]]))
  imps  <- imputations3[[6]][idx_t]
  
  expect_equal(sum(is.na(imps)), 0)
  
})