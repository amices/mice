test_that("categorical imputers use one independent uniform per missing row", {
  skip_if_not_installed("MASS")
  # All classes have the same observed predictor distribution, so their fitted
  # probabilities are 1/3. Compare exact seeded draws, not empirical frequencies.
  for (method in c("polyreg", "polr")) {
    imputer <- get(paste0("mice.impute.", method), envir = asNamespace("mice"))
    for (nmis in c(1L, 2L, 30L)) {
      y <- factor(
        c(rep(letters[1:3], each = 30), rep(NA_character_, nmis)),
        levels = letters[1:3]
      )
      x <- cbind(x = c(rep(seq(-1, 1, length.out = 30), 3), rep(0, nmis)))
      if (method == "polr") {
        y <- ordered(y)
      }
      set.seed(100)
      u <- runif(nmis)
      expected <- letters[1L + (u > 1 / 3) + (u > 2 / 3)]
      set.seed(100)
      actual <- imputer(y, !is.na(y), x)
      expect_identical(actual, expected, info = paste(method, nmis))
    }
  }
})

test_that("lda categorical draws match its posterior row by row", {
  skip_if_not_installed("MASS")
  # LDA rejects exactly equal group means; use distinct means and compare to
  # the actual posterior instead of assuming equal class probabilities.
  for (nmis in c(1L, 2L, 30L)) {
    y <- factor(
      c(rep(letters[1:3], each = 30), rep(NA_character_, nmis)),
      levels = letters[1:3]
    )
    observed <- rep(seq(-1, 1, length.out = 30), 3) +
      rep(c(-0.1, 0, 0.1), each = 30)
    x <- cbind(x = c(observed, rep(0, nmis)))
    ry <- !is.na(y)
    fit <- MASS::lda(x, y, subset = ry)
    post <- predict(fit, x[!ry, , drop = FALSE])$posterior
    set.seed(100)
    u <- runif(nmis)
    expected <- vapply(
      seq_len(nmis),
      function(i) {
        levels(y)[which(u[i] <= cumsum(post[i, ]))[1L]]
      },
      character(1)
    )
    set.seed(100)
    expect_identical(mice.impute.lda(y, ry, x), expected)
  }
})
