test_that("flux() returns one row per variable in local", {
  full <- flux(nhanes)

  f2 <- flux(nhanes, local = c("bmi", "hyp"))
  expect_identical(f2, full[c("bmi", "hyp"), ])

  f3 <- flux(nhanes, local = c("bmi", "hyp", "chl"))
  expect_identical(f3, full[c("bmi", "hyp", "chl"), ])
})

test_that("fluxplot() returns the flux statistics for local", {
  pdf(NULL)
  on.exit(dev.off())
  f <- fluxplot(nhanes, local = c("bmi", "chl"))
  expect_identical(f, flux(nhanes)[c("bmi", "chl"), ])
})
