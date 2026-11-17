set.seed(1)
td <- matrix(rnorm(20), nrow = 5, ncol = 4)
dimnames(td) <- list(1:5, LETTERS[1:4])

y <- td[, 1]
ry <- rep(TRUE, 5)

# data frame for storing the event log
.mice.state <- list(it = 0, im = 0, dep = "y", meth = "test", log = FALSE)
loggedEvents <- data.frame(it = 0, im = 0, dep = "", meth = "", out = "")

fr <- 2

.mice.state$meth <- "k1"
x <- td[, 2:4]
k1 <- mice:::remove.lindep(x, y, ry, frame = fr)

.mice.state$meth <- "k2"
x[, 2] <- x[, 1]
k2 <- mice:::remove.lindep(x, y, ry, frame = fr)

.mice.state$meth <- "k3"
x[, 3] <- 2 * x[, 1]
k3 <- mice:::remove.lindep(x, y, ry, frame = fr)

.mice.state$meth <- "k4"
x <- td[, 2:4]
y <- x[, 2]
k4 <- mice:::remove.lindep(x, y, ry, frame = fr)

.mice.state$meth <- "k5"
x <- td[, 2:4]
y <- x[, 2]
x[, 3] <- x[, 1] <- x[, 2]
k5 <- mice:::remove.lindep(x, y, ry, frame = fr)

# one column x, same as y --> FALSE
.mice.state$meth <- "k6"
x <- td[, 2, drop = FALSE]
y <- x[, 1]
k6 <- mice:::remove.lindep(x, y, ry, frame = fr)

# one column x, different from u --> TRUE
.mice.state$meth <- "k7"
x <- td[, 2, drop = FALSE]
y <- td[, 1]
k7 <- mice:::remove.lindep(x, y, ry, frame = fr)

# two columns, same x and y --> FALSE, FALSE
.mice.state$meth <- "k8"
x <- td[, 2:3, drop = FALSE]
x[, 2] <- x[, 1]
y <- x[, 1]
k8 <- mice:::remove.lindep(x, y, ry, frame = fr)


loggedEvents

test_that("REMOVE-LINDEP-001: removes copies", {
  expect_identical(unname(k1), c(TRUE, TRUE, TRUE))
  expect_identical(unname(k2), c(FALSE, TRUE, TRUE))
  # expect_identical(unname(k3), c(FALSE, FALSE, TRUE))
  expect_identical(unname(k4), c(TRUE, FALSE, TRUE))
  expect_identical(unname(k5), c(FALSE, FALSE, FALSE))
  expect_identical(unname(k6), c(FALSE))
  expect_identical(unname(k7), c(TRUE))
  expect_identical(unname(k8), c(FALSE, FALSE))
})

test_that("REMOVE-LINDEP-002: logs a copy of the outcome as the only removal (#306)", {
  set.seed(123)
  n <- 200
  x <- rnorm(n)
  y <- x + rnorm(n)
  d <- data.frame(x = x, y = y, z = y)
  d$y[sample(n, 50)] <- NA
  expect_warning(
    imp <- mice(
      d,
      method = "norm",
      remove.collinear = FALSE,
      m = 1,
      maxit = 1,
      seed = 1,
      print = FALSE
    ),
    "Number of logged events"
  )
  ev <- imp$loggedEvents
  expect_true(any(ev$dep == "y" & ev$out == "z"))
})

test_that("REMOVE-LINDEP-003: removes negative copies and logs constant outcomes", {
  set.seed(1)
  x <- matrix(
    rnorm(40),
    nrow = 10,
    dimnames = list(NULL, c("a", "b", "c", "d"))
  )
  ry <- rep(TRUE, 10)
  .mice.state <- list(it = 1, im = 1, dep = "y", meth = "test", log = FALSE)
  loggedEvents <- NULL

  y <- -x[, "b"]
  keep <- mice:::remove.lindep(x, y, ry, frame = 1)
  expect_identical(unname(keep), c(TRUE, FALSE, TRUE, TRUE))
  expect_true("b" %in% loggedEvents$out)

  loggedEvents <- NULL
  .mice.state$log <- FALSE
  keep <- mice:::remove.lindep(x, rep(1, 10), ry, frame = 1)
  expect_false(any(keep))
  expect_identical(loggedEvents$out, "a, b, c, d")
})
