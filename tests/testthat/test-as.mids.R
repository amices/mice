context("as.mids")

nhanes3 <- nhanes
rownames(nhanes3) <- LETTERS[1:nrow(nhanes3)]
imp <- mice(nhanes3, m = 2, maxit = 1, print = FALSE)

X <- complete(imp, action = "long", include = TRUE)
# create dataset with .imp variable as numeric
X2 <- X

# nhanes example
test1 <- as.mids(X)
# nhanes example
test2 <- as.mids(X2)
# nhanes example, where we explicitly specify .id as column 2
test3 <- as.mids(X, .id = 6)
# nhanes example with .id where .imp is numeric
test4 <- as.mids(X2, .id = 6)
#' # example without an .id variable
#' # variable .id not preserved
test5 <- as.mids(X[, -6])
#' # reverse data order
rev <- ncol(X):1
test6 <- as.mids(X[, rev])

# as() syntax has fewer options
test7 <- as(X, "mids")
test8 <- as(X2, "mids")
test9 <- as(X2[, -2], "mids")
test10 <- as(X[, rev], "mids")

test_that("as.mids() produces a `mids` object", {
  expect_is(test1, "mids")
  expect_is(test2, "mids")
  expect_is(test3, "mids")
  expect_is(test4, "mids")
  expect_is(test5, "mids")
  expect_is(test7, "mids")
  expect_is(test8, "mids")
  expect_is(test9, "mids")
  expect_is(test10, "mids")
  expect_error(
    as(X[-(1:10), ], "mids"),
    "Unequal group sizes in imputation index `.imp`"
  )
  expect_error(
    as(X[, -(5:6)], "mids"),
    "Imputation index `.imp` not found"
  )
})

test_that("complete() reproduces the original data", {
  expect_true(identical(complete(test1, action = "long", include = TRUE), X))
  expect_true(identical(complete(test2, action = "long", include = TRUE), X))
  expect_true(identical(complete(test3, action = "long", include = TRUE), X))
  expect_true(identical(complete(test4, action = "long", include = TRUE), X))
  expect_true(identical(complete(test5, action = "long", include = TRUE)[, -6], X[, -6]))
  expect_true(identical(complete(test6, action = "long", include = TRUE)[, -(5:6)], X[, rev][, -(1:2)]))
})

# works with dplyr

library(dplyr)
X3 <- X %>%
  group_by(hyp) %>%
  mutate(chlm = mean(chl, na.rm = TRUE))
test_that("handles grouped_df", {
  expect_silent(as.mids(X3))
})

