test_that("SAMPLER-ERROR-001: errors in a method name the iteration, imputation and variable", {
  assign("calls.se001", 0L, envir = globalenv())
  assign(
    "mice.impute.se001",
    function(y, ry, x, ...) {
      calls.se001 <<- calls.se001 + 1L
      if (calls.se001 == 5L) {
        stop("failure in se001")
      }
      mice.impute.pmm(y, ry, x, ...)
    },
    envir = globalenv()
  )
  on.exit(
    rm(list = c("calls.se001", "mice.impute.se001"), envir = globalenv()),
    add = TRUE
  )
  meth <- make.method(nhanes)
  meth["chl"] <- "se001"
  # sequential order: iteration 1 has calls 1-3, the fifth call is
  # iteration 2, imputation 2
  e <- tryCatch(
    mice(nhanes, m = 3, maxit = 3, method = meth, seed = 1, print = FALSE),
    error = identity
  )
  expect_s3_class(e, "mice_sampler_error")
  expect_identical(e$iteration, 2L)
  expect_identical(e$imputation, 2L)
  expect_identical(e$variable, "chl")
  expect_identical(e$method, "se001")
  expect_false(e$post)
  expect_match(
    conditionMessage(e),
    "iteration 2, imputation 2, variable 'chl' (method 'se001')",
    fixed = TRUE
  )
  expect_match(conditionMessage(e), "failure in se001")
})

test_that("SAMPLER-ERROR-002: errors in post-processing are reported as such", {
  post <- make.post(nhanes)
  post["bmi"] <- "if (k == 2) stop('failure in post')"
  e <- tryCatch(
    mice(nhanes, m = 2, maxit = 3, post = post, seed = 1, print = FALSE),
    error = identity
  )
  expect_s3_class(e, "mice_sampler_error")
  expect_identical(e$iteration, 2L)
  expect_identical(e$imputation, 1L)
  expect_identical(e$variable, "bmi")
  expect_true(e$post)
  expect_match(conditionMessage(e), "(post-processing)", fixed = TRUE)
  expect_s3_class(e$parent, "simpleError")
})

test_that("SAMPLER-ERROR-003: mice.mids() reports the continued iteration", {
  imp <- mice(nhanes, m = 2, maxit = 2, seed = 1, print = FALSE)
  post <- make.post(nhanes)
  post["bmi"] <- "if (k == 3) stop('failure in post')"
  imp$post <- post
  e <- tryCatch(mice.mids(imp, maxit = 2, print = FALSE), error = identity)
  expect_s3_class(e, "mice_sampler_error")
  expect_identical(e$iteration, 3L)
})

test_that("SAMPLER-ERROR-004: errors on a parallel chain report its imputation number", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  post <- make.post(nhanes)
  post["bmi"] <- "if (chain == 2) stop('failure in chain 2')"
  e <- suppressWarnings(tryCatch(
    mice(
      nhanes,
      m = 3,
      maxit = 2,
      post = post,
      seed = 1,
      print = FALSE,
      future = TRUE
    ),
    error = identity
  ))
  expect_s3_class(e, "mice_sampler_error")
  expect_identical(e$imputation, 2L)
  expect_identical(e$iteration, 1L)
})

test_that("SAMPLER-ERROR-005: arguments in ... do not shift the sampler arguments", {
  # an argument in ... that partially matched an argument of sampler(),
  # e.g. `from` for `fromto`, used to shift the positional arguments, so
  # that printFlag was passed to the methods (e.g. as ntree of rf)
  a <- mice(nhanes, method = "rf", seed = 1, print = FALSE)
  b <- mice(nhanes, method = "rf", seed = 1, print = FALSE, from = 1)
  expect_identical(a$imp, b$imp)
  c <- mice(nhanes, method = "rf", seed = 1, printFlag = FALSE, print = FALSE)
  expect_identical(a$imp, c$imp)
})
