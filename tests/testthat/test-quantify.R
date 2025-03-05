
test_that("quantify() and unquantify() work correctly", {
  set.seed(123)

  # Original factor variable
  y <- factor(sample(c("A", "B", "C"), 10, replace = TRUE), levels = c("A", "B", "C"))

  # Simulated `x` (covariates)
  x <- matrix(rnorm(10 * 3), ncol = 3)

  # Logical `ry` (observed vs missing)
  ry <- sample(c(TRUE), 10, replace = TRUE)

  # Quantify the factor (optimal scaling)
  ynum_quantified <- mice:::quantify(y, ry, x)

  # Convert to integer encoding
  ynum_integer <- as.integer(y)

  # Reverse optimal scaling
  y_reconstructed_quantified <- mice:::unquantify(ynum_quantified, y, quantify = TRUE)

  # Reverse integer encoding
  y_reconstructed_integer <- mice:::unquantify(ynum_integer, y, quantify = FALSE)

  # Test 1: Levels should remain in the original order
  expect_equal(levels(y_reconstructed_quantified), levels(y))
  expect_equal(levels(y_reconstructed_integer), levels(y))

  # Test 2: Factor reconstruction should match original
  expect_equal(y_reconstructed_quantified, y)
  expect_equal(y_reconstructed_integer, y)

  # Test 3: Handle missing values correctly
  y_with_na <- y
  y_with_na[c(2, 5)] <- NA

  ynum_quantified_na <- mice:::quantify(y_with_na, ry, x)
  y_reconstructed_na <- mice:::unquantify(ynum_quantified_na, y_with_na, quantify = TRUE)

  expect_true(is.na(y_reconstructed_na[2]))
  expect_true(is.na(y_reconstructed_na[5]))

  # Test 4: Unquantify should return original y if y is not a factor
  expect_equal(mice:::unquantify(ynum_quantified, as.numeric(y), quantify = TRUE), ynum_quantified)
})


