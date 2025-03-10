test_that("quantify() and unquantify() work correctly for factors", {
  set.seed(123)
  y <- factor(sample(c("A", "B", "C"), 10, replace = TRUE), levels = c("A", "B", "C"))
  x <- matrix(rnorm(10 * 3), ncol = 3)
  ry <- sample(c(TRUE), 10, replace = TRUE)

  # Quantify the factor (optimal scaling)
  f <- mice:::quantify(y, ry, x)
  ynum_quantified <- f$ynum
  y_reconstructed_quantified <- mice:::unquantify(ynum_quantified, quant = f$quant, labels = f$labels)
  expect_equal(y, y_reconstructed_quantified)

  # Integer coding
  f <- mice:::quantify(y, ry, x, quantify = FALSE)
  ynum_quantified <- f$ynum
  y_reconstructed_integer <- mice:::unquantify(ynum_quantified, quant = f$quant, labels = f$labels)
  expect_equal(y, y_reconstructed_integer)

  # Test 1: Levels should remain in the original order
  expect_equal(levels(y_reconstructed_quantified), levels(y))
  expect_equal(levels(y_reconstructed_integer), levels(y))

  # Test 2: Factor reconstruction should match original
  expect_equal(y_reconstructed_quantified, y)
  expect_equal(y_reconstructed_integer, y)

  # Handle missing values, with extra level
  y_with_na <- y
  y_with_na[c(2, 5)] <- NA
  ry[c(2,5)] <- FALSE

  f <- mice:::quantify(y_with_na, ry, x)
  ynum_quantified_na <- f$ynum
  y_reconstructed_na <- mice:::unquantify(ynum_quantified_na, quant = f$quant, labels = f$labels)
  expect_equal(y_with_na, y_reconstructed_na)

  expect_true(is.na(y_reconstructed_na[2]))
  expect_true(is.na(y_reconstructed_na[5]))
})

test_that("quantify() and unquantify() work correctly for numeric variables", {
  set.seed(123)
  y <- rnorm(10)
  x <- matrix(rnorm(10 * 3), ncol = 3)
  ry <- sample(c(TRUE), 10, replace = TRUE)

  # Pass through a numeric variable
  f <- mice:::quantify(y, ry, x)
  ynum_quantified <- f$ynum
  y_reconstructed_quantified <- mice:::unquantify(ynum_quantified, quant = f$quant, labels = f$labels)
  expect_equal(y, y_reconstructed_quantified)

  # Pass through, integer coding
  f <- mice:::quantify(y, ry, x, quantify = FALSE)
  ynum_quantified <- f$ynum
  y_reconstructed_integer <- mice:::unquantify(ynum_quantified, quant = f$quant, labels = f$labels)
  expect_equal(y, y_reconstructed_integer)

  # Handle missing values, with extra level
  y_with_na <- y
  y_with_na[c(2, 5)] <- NA
  ry[c(2,5)] <- FALSE

  f <- mice:::quantify(y_with_na, ry, x)
  ynum_quantified_na <- f$ynum
  y_reconstructed_na <- mice:::unquantify(ynum_quantified_na, quant = f$quant, labels = f$labels)
  expect_equal(y_with_na, y_reconstructed_na)
})



