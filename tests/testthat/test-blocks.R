context("blocks")

# case with two non-standard problems
# 1) a duplicate bmi is acceptable through blocks
# 2) hyp not specified,
#
# The current policy is not satisfying:
# Currently, where[, "hyp"] is set to FALSE, so hyp is not imputed.
# However, it is still is predictor for block B1, bmi and age, thus
# leading to missing data propagation
#

library(mice)   # branch support_blocks
expect_warning(imp <<- mice(nhanes, blocks = make.blocks(list(c("bmi", "chl"), "bmi", "age")), m = 1, print = FALSE))

head(complete(imp))
imp$blocks
imp$formulas
head(imp$where)
imp$method
imp$predictorMatrix

# A better policy might be inactivating any unmentioned variable j by
# 1) set method[j] to "",
# 2) set predictorMatrix[, j] to 0 (take j out as predictor)
# 3) leave predictorMatrix[j, ] untouched
# 4) leave where[, j] untouched
# As a result, j is not imputed and is not a predictor anywhere

test_that("removes variables from 'where'", {
  expect_identical(sum(imp$where[, "hyp"]), 8L)
})



# reprex https://github.com/amices/mice/issues/326
imp1 <- mice(nhanes, seed = 1, m = 1, maxit = 2, print = FALSE)
imp2 <- mice(nhanes, blocks = list(c("bmi", "hyp"), "chl"), m = 1, maxit = 2, seed = 1, print = FALSE)
test_that("expands a univariate method to all variables in the block", {
  expect_identical(complete(imp1, 1), complete(imp2, 1))
})

imp3 <- mice(nhanes, blocks = list(c("hyp", "bmi"), "chl"), m = 1, maxit = 2, seed = 1, print = FALSE)
imp4 <- mice(nhanes, visitSequence = c("hyp", "bmi", "chl"), m = 1, maxit = 2, seed = 1, print = FALSE)
test_that("blocks alter the visit sequence", {
  expect_identical(complete(imp3, 1), complete(imp4, 1))
})


context("parcel")

# model with duplicate bmi cannot be specified with parcel

# EXPECT WARNING: In b2n(name.blocks(x, prefix = prefix)) : Duplicated name(s) removed: bmi
expect_warning(
  parcel1a <<- make.parcel(list(c("bmi", "chl"), "bmi", "age")))
parcel1b <-    setNames(      c("A",   "A",    "bmi", "age"),
                       nm = c("bmi", "chl",  "bmi", "age"))

expect_silent(imp1a <- mice(nhanes, parcel = parcel1a, m = 10, print = FALSE))
# EXPECT ERROR: validate.parcel(parcel, silent = silent) is not TRUE
expect_error(suppressWarnings(imp1b <<- mice(nhanes, parcel = parcel1b, m = 10, print = FALSE)))

# Getting around the error by the visitSequence
# test_that("parcel formulation is equivalent to blocks", {
#   expect_identical(complete(imp1, 1), complete(imp1a, 1))
#   expect_identical(complete(imp1, 1), complete(imp1b, 1))
# })
#


# reprex https://github.com/amices/mice/issues/326
imp1 <- mice(nhanes, seed = 1, m = 1, maxit = 2, print = FALSE)
imp2 <- mice(nhanes, parcel = make.parcel(list(c("bmi", "hyp"), "chl")), m = 1, maxit = 2, seed = 1, print = FALSE)
test_that("expands a univariate method to all variables in the block", {
  expect_identical(complete(imp1, 1), complete(imp2, 1))
})

# neat parcel formulation
parcel2 <- setNames(c("A", "A", "chl"),
                  nm = c("bmi", "hyp", "chl"))
imp2a <- mice(nhanes, parcel = parcel2, m = 1, maxit = 2, seed = 1, print = FALSE)
test_that("setNames parcel formulation yields same solution", {
  expect_identical(complete(imp2, 1), complete(imp2a, 1))
})

# different order
parcel3 <- setNames(c("A", "A", "chl"),
                  nm = c("hyp", "bmi", "chl"))
imp3 <- mice(nhanes, parcel = parcel3, m = 1, maxit = 2, seed = 1, print = FALSE)
imp4 <- mice(nhanes, visitSequence = c("hyp", "bmi", "chl"), m = 1, maxit = 2, seed = 1, print = FALSE)
test_that("parcels alter the visit sequence", {
  expect_identical(complete(imp3, 1), complete(imp4, 1))
})

complete(imp3, 1)

