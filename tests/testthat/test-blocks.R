test_that("BLOCKS-001: removes variables from 'where'", {
  imp <- mice(
    nhanes,
    blocks = make.blocks(list(c("bmi", "chl"), "bmi", "age")),
    m = 10,
    print = FALSE
  )
  expect_identical(sum(imp$where[, "hyp"]), 0L)
})

test_that("BLOCKS-002: expands a univariate method to all variables in the block", {
  # reprex https://github.com/amices/mice/issues/326
  imp1 <- mice(nhanes, seed = 1, m = 1, maxit = 2, print = FALSE)
  imp2 <- mice(
    nhanes,
    blocks = list(c("bmi", "hyp"), "chl"),
    calltype = c("pred", "pred"),
    m = 1,
    maxit = 2,
    seed = 1,
    print = FALSE
  )
  expect_identical(complete(imp1, 1), complete(imp2, 1))
})

test_that("BLOCKS-003: within-block order affects imputations though visitSequence is unchanged", {
  imp3 <- mice(
    nhanes,
    blocks = list(c("hyp", "bmi"), "chl"),
    m = 1,
    maxit = 2,
    seed = 1,
    print = FALSE
  )
  imp3a <- mice(
    nhanes,
    blocks = list(c("bmi", "hyp"), "chl"),
    m = 1,
    maxit = 2,
    seed = 1,
    print = FALSE
  )
  expect_identical(imp3$visitSequence, imp3a$visitSequence)
  expect_failure(expect_identical(complete(imp3, 1), complete(imp3a, 1)))
})
