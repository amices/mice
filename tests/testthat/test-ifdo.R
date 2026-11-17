d <- airquality[, 1:2]

impute <- function(cmd, data = d, maxit = 2) {
  post <- make.post(data)
  post["Ozone"] <- cmd
  mice(
    data,
    method = "norm.nob",
    m = 2,
    maxit = maxit,
    seed = 1,
    post = post,
    print = FALSE
  )
}

test_that("IFDO-001: the book example matches squeeze() (#43)", {
  imp1 <- impute("ifdo(c(Ozone < 1, Ozone > 200), c(1, 200))")
  imp2 <- impute("imp[[j]][, i] <- squeeze(imp[[j]][, i], c(1, 200))")
  expect_identical(imp1$imp, imp2$imp)
  expect_gte(min(as.matrix(imp1$imp$Ozone)), 1)
})

test_that("IFDO-002: a single condition, and an action using the data", {
  imp <- impute("ifdo(Ozone < 1, 1)")
  expect_gte(min(as.matrix(imp$imp$Ozone)), 1)

  # one iteration, so that both runs draw the same Ozone imputations
  imp <- impute("ifdo(Ozone < 0, abs(Ozone))", maxit = 1)
  ref <- impute("", maxit = 1)
  expect_identical(abs(as.matrix(ref$imp$Ozone)), as.matrix(imp$imp$Ozone))
})

test_that("IFDO-003: conditions may use other variables", {
  imp <- impute("ifdo(Solar.R > 250, 999)")
  cmp <- complete(imp, 1)
  miss <- is.na(d$Ozone)
  # use rows with observed Solar.R, which do not change between iterations
  obs <- miss & !is.na(d$Solar.R)
  hit <- obs & d$Solar.R > 250
  expect_true(any(hit))
  expect_true(all(cmp$Ozone[hit] == 999))
  expect_false(any(cmp$Ozone[obs & !hit] == 999))
})

test_that("IFDO-004: conditions are evaluated before replacement", {
  # without a snapshot, the second condition would catch the first action
  imp <- impute("ifdo(c(Ozone < 1, Ozone == 500), c(500, 0))")
  expect_true(all(as.matrix(imp$imp$Ozone) >= 1))
  expect_true(any(as.matrix(imp$imp$Ozone) == 500))
})

test_that("IFDO-005: informative errors", {
  expect_error(
    impute("ifdo(c(Ozone < 1, Ozone > 200), 1)"),
    "2 condition\\(s\\), but `action` has 1"
  )
  expect_error(ifdo(TRUE, 1), "inside the `post` argument")
})
