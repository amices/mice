test_that("LOGGEDEVENTS-001: loggedEvents is NULL", {
  imp1 <- mice(nhanes, m = 2, print = FALSE)

  # copy of data, different names
  data2 <- cbind(nhanes, nhanes)
  colnames(data2)[5:8] <- c("age2", "bmi2", "hyp2", "chl2")
  imp2 <- suppressWarnings(mice(data2, m = 2, print = FALSE))

  expect_null(imp1$loggedEvents)
  expect_type(imp2$loggedEvents, "list")
})

test_that("LOGGEDEVENTS-002: the ridge penalty entry is one line (#669)", {
  imp <- suppressWarnings(
    mice(boys[boys$gen != "G5", ], m = 1, maxit = 1, print = FALSE, eps = 0)
  )
  out <- imp$loggedEvents$out
  expect_true(any(grepl("ridge penalty applied", out)))
  expect_false(any(grepl("\n", out, fixed = TRUE)))
})
