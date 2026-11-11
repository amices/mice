test_that("FUTUREMICE-001: futuremice() restores the future plan of the user", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("furrr")
  skip_if_not_installed("parallelly")

  oldplan <- future::plan(future::multisession, workers = 2)
  on.exit(future::plan(oldplan), add = TRUE)

  # sequential plan does not take workers, so futuremice() warns
  imp <- suppressWarnings(futuremice(
    nhanes,
    m = 2,
    n.core = 1,
    parallelseed = 1,
    future.plan = "sequential",
    maxit = 1
  ))
  expect_s3_class(imp, "mids")
  expect_true(inherits(future::plan(), "multisession"))
})
