set.seed(123)
test_that("quantify() and unquantify() work correctly for factors", {
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


test_that("quantify and unquantify handle small sample edge cases", {
  # n = 1 per category (minimum viable case),
  # but not enough to estimate a regression
  y1 <- factor(c("low", "medium", "high"))
  x1 <- matrix(runif(3), nrow = 3)
  ry1 <- rep(TRUE, 3)

  q1 <- quantify(y1, ry1, x1)
  y1_back <- unquantify(q1$ynum, q1$quant, q1$labels)

  expect_s3_class(y1_back, "factor")
  expect_true(all(levels(y1_back) %in% levels(y1)))
  expect_equal(length(y1_back), length(y1))
  # there are duplicate quant values, so the reconstructed factor
  # will not be identical to the original factor
  expect_false(identical(y1_back, y1))

  # n = 2 per category
  y2 <- factor(c("low", "low", "medium", "medium", "high", "high"))
  x2 <- matrix(runif(12), nrow = 6)
  ry2 <- rep(TRUE, 6)

  q2 <- quantify(y2, ry2, x2)
  y2_back <- unquantify(q2$ynum, q2$quant, q2$labels)

  expect_s3_class(y2_back, "factor")
  expect_equal(length(y2_back), length(y2))
  expect_true(all(levels(y2_back) %in% levels(y2)))
  # there are no duplicate quant values, so the reconstructed factor
  # will be identical to the original factor
  expect_true(identical(y2_back, y2))
})

