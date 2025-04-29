context("check.visitSequence")

data <- nhanes

test_that("mice() takes numerical and character visitSequence", {
  expect_silent(imp <- mice(data, visitSequence = 4:1, m = 1, print = FALSE))
  expect_silent(imp <- mice(data, visitSequence = rev(names(data)), m = 1, print = FALSE))
})

test_that("Passive variable is moved to end of visitSequence when not user-defined", {
  data <- data.frame(
    p = rep(NA_real_, 8),
    x = c(1, NA, 3, 4, NA, 3, 2, NA),
    y = c(2, 3, NA, 5, 6, 3, 4, NA)
  )

  # Problem case: p = y * x. Updated before x and y are imputed
  set.seed(1)
  ini <- mice(data, maxit = 0, remove.constant = FALSE)
  meth <- ini$method
  meth["p"] <- "~ I(y * x)"

  imp1 <- mice(data, method = meth, maxit = 1, print = FALSE)
  expect_setequal(imp1$visitSequence, c("x", "y", "p"))
})