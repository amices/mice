# Check that mice.mids correctly appends the newdata to the
# existing mids object
init0 <- mice(nhanes, maxit = 0, m = 1, print = FALSE, seed = 1)

init1 <- mice(nhanes, maxit = 0, m = 1, print = FALSE)
init1$ignore <- rep(FALSE, nrow(nhanes))

init2 <- mice.mids(init0, newdata = nhanes, maxit = 0, print = FALSE)

test_that("NEWDATA-001: `newdata` works like rbind with ignore", {
  expect_equal(complete(rbind(init0, init1)), complete(init2))
})

imp <- mice(nhanes2, maxit = 0, m = 1, seed = 1)
test_that("NEWDATA-002: `newdata` produces warning `invalid factor level, NA generated`", {
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

imp1 <- mice(
  nhanes,
  maxit = 1,
  m = 1,
  print = FALSE,
  seed = 1,
  donors = 1L,
  matchtype = 0
)

imp2 <- mice.mids(imp1, newdata = artificial, maxit = 1, print = FALSE)
imp2b <- mice.mids(imp1, newdata = artificial, maxit = 1, print = FALSE)


test_that("NEWDATA-003: `newdata` works with pmm", {
  expect_failure(expect_equal(complete(imp2)["a1", "bmi"], 40.0))
})

test_that("NEWDATA-004: `newdata` returns filtered mids object", {
  expect_equal(nrow(complete(imp2)), nrow(artificial))
})

test_that("NEWDATA-005: `newdata` uses a common seed", {
  expect_equal(complete(imp2), complete(imp2b))
})

# Methods for variables that were complete in the training data (#753)
imp753 <- mice(nhanes, m = 2, maxit = 2, print = FALSE, seed = 1)

test_that("NEWDATA-006: a variable complete in training gets the default method", {
  nd <- nhanes[2, ]
  nd[1, "age"] <- NA
  expect_message(
    imp <- mice.mids(imp753, newdata = nd, print = FALSE),
    "`age`"
  )
  expect_equal(unname(imp$method["age"]), "pmm")
  expect_false(anyNA(complete(imp, 1)))
  expect_false(anyNA(complete(imp, 2)))
})

test_that("NEWDATA-007: no NA propagation to variables that have a method", {
  nd <- nhanes[2, ]
  nd[1, c("age", "bmi")] <- NA
  imp <- suppressMessages(mice.mids(imp753, newdata = nd, print = FALSE))
  expect_false(anyNA(complete(imp, 1)))
})

test_that("NEWDATA-008: a method set to \"\" by the user is kept", {
  imp0 <- imp753
  imp0$method["bmi"] <- ""
  imp0$predictorMatrix[, "bmi"] <- 0
  nd <- nhanes[2, ]
  nd[1, "bmi"] <- NA
  expect_silent(imp <- mice.mids(imp0, newdata = nd, print = FALSE))
  expect_equal(unname(imp$method["bmi"]), "")
  expect_true(is.na(complete(imp, 1)[nrow(nd), "bmi"]))
})

test_that("NEWDATA-009: a filled block is added to a visitSequence without it", {
  imp0 <- mice(
    nhanes,
    m = 1,
    maxit = 1,
    print = FALSE,
    seed = 1,
    visitSequence = "roman"
  )
  expect_false("age" %in% imp0$visitSequence)
  nd <- nhanes[2, ]
  nd[1, "age"] <- NA
  imp <- suppressMessages(mice.mids(imp0, newdata = nd, print = FALSE))
  expect_true("age" %in% imp$visitSequence)
  expect_false(anyNA(complete(imp, 1)))
})

test_that("NEWDATA-010: the default method follows the type of the variable", {
  imp0 <- mice(nhanes2, m = 1, maxit = 1, print = FALSE, seed = 1)
  nd <- nhanes2[2, ]
  nd[1, "age"] <- NA
  imp <- suppressMessages(mice.mids(imp0, newdata = nd, print = FALSE))
  expect_equal(unname(imp$method["age"]), "polyreg")
  expect_s3_class(complete(imp, 1)$age, "factor")
  expect_false(anyNA(complete(imp, 1)))
})

test_that("NEWDATA-011: `newdata` works when the blocks do not cover all variables", {
  imp0 <- mice(
    nhanes,
    formulas = list(
      bmi = bmi ~ age + chl,
      chl = chl ~ age + bmi,
      hyp = hyp ~ age
    ),
    m = 1,
    maxit = 1,
    print = FALSE,
    seed = 1
  )
  expect_silent(imp <- mice.mids(imp0, newdata = nhanes[2, ], print = FALSE))
  expect_equal(nrow(complete(imp, 1)), 1L)
})
