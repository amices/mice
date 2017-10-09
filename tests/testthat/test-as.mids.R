context("as.mids")

nhanes3 <- nhanes
rownames(nhanes3) <- LETTERS[1:nrow(nhanes3)]
imp <- mice(nhanes3, m = 2, print = FALSE)

X <- complete(imp, action = "long", include = TRUE)
# create dataset with .imp variable as numeric
X2 <- X
X2$.imp <- as.numeric(levels(X$.imp))[X$.imp]

# nhanes example
test1 <- as.mids(X)
# nhanes example
test2 <- as.mids(X2)
# nhanes example, where we explicitly specify .id as column 2
test3 <- as.mids(X, .id = 2)
# nhanes example with .id where .imp is numeric
test4 <- as.mids(X2, .id = 2)
#'# example without an .id variable
#'# variable .id not preserved
test5 <- as.mids(X[, -2])
#'# reverse data order
rev <- ncol(X):1
test6 <- as.mids(X[, rev])

test_that("as.mids() produces a `mids` object", {
  expect_is(test1, "mids")
  expect_is(test2, "mids")
  expect_is(test3, "mids")
  expect_is(test4, "mids")
  expect_is(test5, "mids")
})

test_that("complete() reproduces the original data", {
  expect_true(all(complete(test1, action = "long", include = TRUE) == X, na.rm = TRUE))
  expect_true(all(complete(test2, action = "long", include = TRUE) == X, na.rm = TRUE))
  expect_true(all(complete(test3, action = "long", include = TRUE) == X, na.rm = TRUE))
  expect_true(all(complete(test4, action = "long", include = TRUE) == X, na.rm = TRUE))
  expect_true(all(complete(test5, action = "long", include = TRUE)[, -2] == X[, -2], na.rm = TRUE))
  expect_true(all(complete(test6, action = "long", include = TRUE)[,-(1:2)] == X[, rev][, -(5:6)], na.rm = TRUE))
})

