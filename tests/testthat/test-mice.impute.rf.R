context("mice.impute.rf")

data <- matrix(
  c(
    1.0, 10.5, 1.5, 13.2, 1.8, 8.0, 1.7, 15.0, 23.0, 40.0,
    2.0, 21.0, 3.3, 38.0, 4.5, -2.3, NA, -2.4
  ),
  nrow = 9, ncol = 2, byrow = TRUE
)
df <- data.frame(data)
par <- list(
  y = df$X1,
  ry = !is.na(df$X1),
  x = df[, "X2", drop = FALSE]
)

test_that("runs with a single missing value using ranger", {
  skip_if_not_installed("ranger")
  expect_visible(do.call(mice.impute.rf, c(par, list(rfPackage = "ranger"))))
})

test_that("runs with a single missing value using randomForest", {
  skip_if_not_installed("randomForest")
  expect_visible(do.call(mice.impute.rf, c(par, list(rfPackage = "randomForest"))))
})

test_that("runs with a single missing value using literanger", {
  skip_if_not_installed("literanger")
  expect_visible(do.call(mice.impute.rf, c(par, list(rfPackage = "literanger"))))
})
