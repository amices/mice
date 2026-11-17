test_that("CHECK-METHOD-NOTFOUND-001: unknown methods give a clear error (#575)", {
  expect_error(
    mice(nhanes, method = "smcfcsXYZ", m = 1, maxit = 1, print = FALSE),
    "Imputation method\\(s\\) not found: 'smcfcsXYZ'"
  )
  expect_error(
    mice(
      nhanes,
      method = c("", "pmm", "nrom", "pmm"),
      m = 1,
      maxit = 1,
      print = FALSE
    ),
    "mice.impute.nrom\\(\\)"
  )
})

test_that("CHECK-METHOD-NOTFOUND-002: user-defined methods are found", {
  mice.impute.mymethod <- function(y, ry, x, wy = NULL, ...) {
    if (is.null(wy)) wy <- !ry
    rep(mean(y[ry]), sum(wy))
  }
  assign("mice.impute.mymethod", mice.impute.mymethod, envir = globalenv())
  on.exit(rm("mice.impute.mymethod", envir = globalenv()))
  expect_silent(
    mice(nhanes, method = "mymethod", m = 1, maxit = 1, print = FALSE)
  )
})
