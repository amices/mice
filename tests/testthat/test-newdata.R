context("mice.mids: newdata")

# Check that mice.mids correctly appends the newdata to the
# existing mids object
init0 <- mice(nhanes, maxit = 0, m = 1, print = FALSE, seed = 1)

init1 <- mice(nhanes, maxit = 0, m = 1, print = FALSE)
init1$ignore <- rep(FALSE, nrow(nhanes))

init2 <- mice.mids(init0, newdata = nhanes, maxit = 0, print = FALSE)

test_that("`newdata` works like rbind with ignore", {
  expect_equal(complete(rbind(init0, init1)), complete(init2))
})

imp <- mice(nhanes2, maxit = 0, m = 1, seed = 1)
test_that("`newdata` produces warning `invalid factor level, NA generated`", {
  expect_silent(mice.mids(imp, newdata = nhanes2[1, ], print = FALSE))
})

# Check that rows flagged as ignored are indeed ignored by the
# univariate sampler in mice.mids
artificial <- data.frame(
  age = c(1, 1),
  bmi = c(NA, 40.0),
  hyp = c(1, 1),
  chl = c(200, 200),
  row.names = paste0("a", 1:2)
)

imp1 <- mice(nhanes,
  maxit = 1, m = 1, print = FALSE, seed = 1,
  donors = 1L, matchtype = 0
)

imp2 <- mice.mids(imp1, newdata = artificial, maxit = 1, print = FALSE)
imp2b <- mice.mids(imp1, newdata = artificial, maxit = 1, print = FALSE)


test_that("`newdata` works with pmm", {
  expect_failure(expect_equal(complete(imp2)["a1", "bmi"], 40.0))
})

test_that("`newdata` returns filtered mids object", {
  expect_equal(nrow(complete(imp2)), nrow(artificial))
})

test_that("`newdata` uses a common seed", {
  expect_true(identical(complete(imp2), complete(imp2b)))
})


