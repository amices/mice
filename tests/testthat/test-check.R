context("check")

data <- nhanes
where <- is.na(data)

blocks <- list("bmi", "age", "chl")
setup <- list(blocks = blocks, 
              nimp = nimp(where, blocks),
              visitSequence = NULL, 
              method = vector("character", length(blocks)),
              defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
              predictorMatrix = (1 - diag(1, length(blocks))),
              nmis = apply(is.na(data), 2, sum), 
              nwhere = apply(where, 2, sum))
z1 <- mice:::check.visitSequence(setup, where)
y1 <- mice:::check.method(z1, data)

blocks <- list(c("bmi", "chl"), "age")
setup <- list(blocks = blocks, 
              nimp = nimp(where, blocks),
              visitSequence = NULL, 
              method = rep("", 2),
              defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
              predictorMatrix = (1 - diag(1, length(blocks))),
              nmis = apply(is.na(data), 2, sum), 
              nwhere = apply(where, 2, sum))
z2 <- mice:::check.visitSequence(setup, where)
y2 <- mice:::check.method(z2, data)

blocks <- list(c("bmi", "chl"), "age", "chl")
setup <- list(blocks = blocks, 
              nimp = nimp(where, blocks),
              visitSequence = NULL, 
              defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
              predictorMatrix = (1 - diag(1, length(blocks))),
              nmis = apply(is.na(data), 2, sum), 
              nwhere = apply(where, 2, sum))
z3 <- mice:::check.visitSequence(setup, where)
y3 <- mice:::check.method(z3, data)

blocks <- list("bmi", "chl", "age", "hyp")
setup <- list(blocks = blocks, 
              nimp = nimp(where, blocks),
              visitSequence = "roman", 
              defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
              predictorMatrix = (1 - diag(1, length(blocks))),
              nmis = apply(is.na(data), 2, sum), 
              nwhere = apply(where, 2, sum))
z4 <- mice:::check.visitSequence(setup, where)
y4 <- mice:::check.method(z4, data)

setup <- list(blocks = blocks, 
              nimp = nimp(where, blocks),
              visitSequence = "arab", 
              method = c("logreg", "", "", ""),
              defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
              predictorMatrix = (1 - diag(1, length(blocks))),
              nmis = apply(is.na(data), 2, sum), 
              nwhere = apply(where, 2, sum))
z5 <- mice:::check.visitSequence(setup, where)
# y5 <- mice:::check.method(z5, data)

setup <- list(blocks = blocks, 
              nimp = nimp(where, blocks),
              visitSequence = "mon",
              method = "unknown",
              defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
              predictorMatrix = (1 - diag(1, length(blocks))),
              nmis = apply(is.na(data), 2, sum), 
              nwhere = apply(where, 2, sum))
z6 <- mice:::check.visitSequence(setup, where)
# y6 <- mice:::check.method(z6, data)

setup <- list(blocks = blocks, 
              nimp = nimp(where, blocks),
              visitSequence = "rev", 
              method = c("pmm", "pmm"),
              defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
              predictorMatrix = (1 - diag(1, length(blocks))),
              nmis = apply(is.na(data), 2, sum), 
              nwhere = apply(where, 2, sum))
z7 <- mice:::check.visitSequence(setup, where)
# y7 <- mice:::check.method(z7, data)

blocks <- make.blocks(data, "void")
setup <- list(blocks = blocks, 
              nimp = nimp(where, blocks),
              visitSequence = "rev", 
              defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
              predictorMatrix = (1 - diag(1, length(blocks))),
              nmis = apply(is.na(data), 2, sum), 
              nwhere = apply(where, 2, sum))
z8 <- mice:::check.visitSequence(setup, where)
y8 <- mice:::check.method(z8, data)

test_that("check.visitSequence produces proper results", {
  expect_equal(z1$visitSequence, c(1, 3))
  expect_equal(z2$visitSequence, 1)
  expect_equal(z3$visitSequence, c(1, 3))
  expect_equal(z4$visitSequence, c(1, 2, 4))
  expect_equal(z5$visitSequence, c(4, 2, 1))
  expect_equal(z6$visitSequence, c(4, 1, 2))
  expect_equal(z7$visitSequence, c(2, 1, 4))
  expect_equal(z8$visitSequence, integer(0))
})

test_that("check.method produces proper results", {
  expect_equal(y1$method, c("pmm", "", "pmm"))
  expect_equal(y2$method, c("pmm", ""))
  expect_equal(y3$method, c("pmm", "", "pmm"))
  expect_equal(y4$method, c("pmm", "pmm", "", "pmm"))
  expect_warning(mice:::check.method(z5, data))
  expect_error(mice:::check.method(z6, data))
  expect_error(mice:::check.method(z7, data))
  expect_equal(y8$method, character(0))
})

