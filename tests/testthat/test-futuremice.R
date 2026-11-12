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

test_that("FUTUREMICE-002: check.cores() never returns zero workers", {
  # one available core, e.g. in a container or a single-core job
  expect_equal(check.cores(NULL, 1L, 5), 1L)
  expect_equal(check.cores(NULL, 3L, 5), 3L)
  expect_equal(check.cores(NULL, 8L, 5), 5L)
  expect_equal(check.cores(2, 3L, 5), 2)
  expect_warning(n <- check.cores(4, 3L, 5), "is set to 3")
  expect_equal(n, 3L)
})
