# toenail: outcome is factor
data("toenail2")
data_factor <- tidyr::complete(toenail2, patientID, visit) |>
  tidyr::fill(treatment) |>
  dplyr::select(-time) |>
  dplyr::mutate(patientID = as.integer(patientID))
pred_factor <- make.predictorMatrix(data_factor)
pred_factor["outcome", "patientID"] <- -2

test_that("mice::mice.impute.2l.bin() accepts factor outcome", {
  expect_silent(
    imp <- mice(data_factor, method = "2l.bin", print = FALSE,
                pred = pred_factor, m = 1, maxit = 1)
  )
  expect_false(anyNA(complete(imp)))
})

# toenail: outcome is 0/1
data("toenail", package = "mice")
data_binary <- tidyr::complete(toenail, ID, visit) |>
  tidyr::fill(treatment) |>
  dplyr::select(-month)
pred_binary <- make.predictorMatrix(data_binary)
pred_binary["outcome", "ID"] <- -2

test_that("mice::mice.impute.2l.bin() accepts 0/1 outcome", {
  expect_silent(
    imp <- mice(data_binary, method = "2l.bin", print = FALSE,
                pred = pred_binary, m = 1, maxit = 1)
  )
  expect_false(anyNA(complete(imp)))
})
