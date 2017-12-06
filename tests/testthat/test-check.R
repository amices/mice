context("check")

data <- nhanes
where <- is.na(data)

blocks <- list("bmi", "age", "chl")
setup <- list(blocks = blocks, 
              visitSequence = NULL, 
              defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
              predictorMatrix = (1 - diag(1, length(blocks))),
              nmis = apply(is.na(data), 2, sum), 
              nwhere = apply(where, 2, sum))
z1 <- mice:::check.visitSequence(setup, where)

blocks <- list(c("bmi", "chl"), "age")
setup <- list(blocks = blocks, 
              visitSequence = NULL, 
              defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
              predictorMatrix = (1 - diag(1, length(blocks))),
              nmis = apply(is.na(data), 2, sum), 
              nwhere = apply(where, 2, sum))
z2 <- mice:::check.visitSequence(setup, where)

blocks <- list(c("bmi", "chl"), "age", "chl")
setup <- list(blocks = blocks, 
              visitSequence = NULL, 
              defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
              predictorMatrix = (1 - diag(1, length(blocks))),
              nmis = apply(is.na(data), 2, sum), 
              nwhere = apply(where, 2, sum))
z3 <- mice:::check.visitSequence(setup, where)

blocks <- list("bmi", "chl", "age", "hyp")
setup <- list(blocks = blocks, 
              visitSequence = "roman", 
              defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
              predictorMatrix = (1 - diag(1, length(blocks))),
              nmis = apply(is.na(data), 2, sum), 
              nwhere = apply(where, 2, sum))
z4 <- mice:::check.visitSequence(setup, where)

setup <- list(blocks = blocks, 
              visitSequence = "arab", 
              defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
              predictorMatrix = (1 - diag(1, length(blocks))),
              nmis = apply(is.na(data), 2, sum), 
              nwhere = apply(where, 2, sum))
z5 <- mice:::check.visitSequence(setup, where)

setup <- list(blocks = blocks, 
              visitSequence = "mon", 
              defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
              predictorMatrix = (1 - diag(1, length(blocks))),
              nmis = apply(is.na(data), 2, sum), 
              nwhere = apply(where, 2, sum))
z6 <- mice:::check.visitSequence(setup, where)

setup <- list(blocks = blocks, 
              visitSequence = "rev", 
              defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
              predictorMatrix = (1 - diag(1, length(blocks))),
              nmis = apply(is.na(data), 2, sum), 
              nwhere = apply(where, 2, sum))
z7 <- mice:::check.visitSequence(setup, where)

blocks <- make.blocks(data, "void")
setup <- list(blocks = blocks, 
              visitSequence = "rev", 
              defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
              predictorMatrix = (1 - diag(1, length(blocks))),
              nmis = apply(is.na(data), 2, sum), 
              nwhere = apply(where, 2, sum))
z8 <- mice:::check.visitSequence(setup, where)


test_that("check.visitSequence has proper entries", {
  expect_equal(z1$visitSequence, c(1, 3))
  expect_equal(z2$visitSequence, 1)
  expect_equal(z3$visitSequence, c(1, 3))
  expect_equal(z4$visitSequence, c(1, 2, 4))
  expect_equal(z5$visitSequence, c(4, 2, 1))
  expect_equal(z6$visitSequence, c(4, 1, 2))
  expect_equal(z7$visitSequence, c(2, 1, 4))
  expect_equal(z8$visitSequence, integer(0))
})

