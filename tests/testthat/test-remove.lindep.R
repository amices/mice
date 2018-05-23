context("remove.lindep")

set.seed(1)
td <- matrix(rnorm(20), nrow = 5, ncol = 4)
dimnames(td) <- list(1:5, LETTERS[1:4])

y <- td[, 1]
ry <- rep(TRUE, 5)

# data frame for storing the event log
state <- list(it = 0, im = 0, dep = "y", meth = "test", log = FALSE)
loggedEvents <- data.frame(it = 0, im = 0, dep = "", meth = "", out = "")

fr <- 2

state$meth <- "k1"
x <- td[, 2:4]
k1 <- mice:::remove.lindep(x, y, ry, frame = fr)

state$meth <- "k2"
x[, 2] <- x[, 1]
k2 <- mice:::remove.lindep(x, y, ry, frame = fr)

state$meth <- "k3"
x[, 3] <- 2 * x[, 1]
k3 <- mice:::remove.lindep(x, y, ry, frame = fr)

state$meth <- "k4"
x <- td[, 2:4]
y <- x[, 2]
k4 <- mice:::remove.lindep(x, y, ry, frame = fr)

state$meth <- "k5"
x <- td[, 2:4]
y <- x[, 2]
x[, 3] <- x[, 1] <- x[, 2]
k5 <- mice:::remove.lindep(x, y, ry, frame = fr)

# one column x, same as y --> FALSE
state$meth <- "k6"
x <- td[, 2, drop = FALSE]
y <- x[, 1]
k6 <- mice:::remove.lindep(x, y, ry, frame = fr)

# one column x, different from u --> TRUE
state$meth <- "k7"
x <- td[, 2, drop = FALSE]
y <- td[, 1]
k7 <- mice:::remove.lindep(x, y, ry, frame = fr)

# two columns, same x and y --> FALSE, FALSE
state$meth <- "k8"
x <- td[, 2:3, drop = FALSE]
x[, 2] <- x[, 1]
y <- x[, 1]
k8 <- mice:::remove.lindep(x, y, ry, frame = fr)


loggedEvents

test_that("removes copies", {
  expect_identical(unname(k1), c(TRUE, TRUE, TRUE))
  expect_identical(unname(k2), c(FALSE, TRUE, TRUE))
  # expect_identical(unname(k3), c(FALSE, FALSE, TRUE))
  expect_identical(unname(k4), c(TRUE, FALSE, TRUE))
  expect_identical(unname(k5), c(FALSE, FALSE, FALSE))
  expect_identical(unname(k6), c(FALSE))
  expect_identical(unname(k7), c(TRUE))
  expect_identical(unname(k8), c(FALSE, FALSE))
})
