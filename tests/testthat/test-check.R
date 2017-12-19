context("check")

data <- nhanes
where <- is.na(data)

blocks <- name.blocks(list("bmi", "age", "chl"))
setup <- list(blocks = blocks, 
              nimp = nimp(where, blocks),
              visitSequence = NULL, 
              method = vector("character", length(blocks)),
              defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
              predictorMatrix = make.predictorMatrix(data, blocks),
              nmis = apply(is.na(data), 2, sum), 
              nwhere = apply(where, 2, sum),
              nvar = ncol(data),
              varnames = names(data))
z1 <- mice:::check.visitSequence(setup, where)
y1 <- mice:::check.method(z1, data)
x1 <- y1
w1 <- mice:::check.data(x1, data)

blocks <- name.blocks(list(c("bmi", "chl"), "age"))
setup <- list(blocks = blocks, 
              nimp = nimp(where, blocks),
              visitSequence = NULL, 
              method = rep("", 2),
              defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
              predictorMatrix = make.predictorMatrix(data, blocks),
              nmis = apply(is.na(data), 2, sum), 
              nwhere = apply(where, 2, sum),
              nvar = ncol(data),
              varnames = names(data))
z2 <- mice:::check.visitSequence(setup, where)
y2 <- mice:::check.method(z2, data)
x2 <- y2
w2 <- mice:::check.data(x2, data)

blocks <- name.blocks(list(c("bmi", "chl"), "age", "chl"))
setup <- list(blocks = blocks, 
              nimp = nimp(where, blocks),
              visitSequence = NULL, 
              defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
              predictorMatrix = make.predictorMatrix(data, blocks),
              nmis = apply(is.na(data), 2, sum), 
              nwhere = apply(where, 2, sum),
              nvar = ncol(data),
              varnames = names(data))
z3 <- mice:::check.visitSequence(setup, where)
y3 <- mice:::check.method(z3, data)
x3 <- y3
w3 <- mice:::check.data(x3, data)

blocks <- name.blocks(list("bmi", "chl", "age", "hyp"))
setup <- list(blocks = blocks, 
              nimp = nimp(where, blocks),
              visitSequence = "roman", 
              defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
              predictorMatrix = make.predictorMatrix(data, blocks),
              nmis = apply(is.na(data), 2, sum), 
              nwhere = apply(where, 2, sum),
              nvar = ncol(data),
              varnames = names(data))
z4 <- mice:::check.visitSequence(setup, where)
y4 <- mice:::check.method(z4, data)
x4 <- y4
w4 <- mice:::check.data(x4, data)

setup <- list(blocks = blocks, 
              nimp = nimp(where, blocks),
              visitSequence = "arab", 
              method = c("logreg", "", "", ""),
              defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
              predictorMatrix = make.predictorMatrix(data, blocks),
              nmis = apply(is.na(data), 2, sum), 
              nwhere = apply(where, 2, sum),
              nvar = ncol(data),
              varnames = names(data))
z5 <- mice:::check.visitSequence(setup, where)
#y5 <- mice:::check.method(z5, data)
#x5 <- mice:::check.predictorMatrix(y5)

setup <- list(blocks = blocks, 
              nimp = nimp(where, blocks),
              visitSequence = "mon",
              method = "unknown",
              defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
              predictorMatrix = make.predictorMatrix(data, blocks),
              nmis = apply(is.na(data), 2, sum), 
              nwhere = apply(where, 2, sum),
              nvar = ncol(data),
              varnames = names(data))
z6 <- mice:::check.visitSequence(setup, where)
# y6 <- mice:::check.method(z6, data)

setup <- list(blocks = blocks, 
              nimp = nimp(where, blocks),
              visitSequence = "rev", 
              method = c("pmm", "pmm"),
              defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
              predictorMatrix = make.predictorMatrix(data, blocks),
              nmis = apply(is.na(data), 2, sum), 
              nwhere = apply(where, 2, sum),
              nvar = ncol(data),
              varnames = names(data))
z7 <- mice:::check.visitSequence(setup, where)
# y7 <- mice:::check.method(z7, data)

blocks <- make.blocks(data, "void")
setup <- list(blocks = blocks, 
              nimp = nimp(where, blocks),
              visitSequence = "rev", 
              defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
              predictorMatrix = make.predictorMatrix(data, blocks),
              nmis = apply(is.na(data), 2, sum), 
              nwhere = apply(where, 2, sum),
              nvar = ncol(data),
              varnames = names(data))
z8 <- mice:::check.visitSequence(setup, where)
y8 <- mice:::check.method(z8, data)
x8 <- y8
w8 <- mice:::check.data(x8, data)

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
  expect_equal(unname(y1$method), c("pmm", "", "pmm"))
  expect_equal(unname(y2$method), c("pmm", ""))
  expect_equal(unname(y3$method), c("pmm", "", "pmm"))
  expect_equal(unname(y4$method), c("pmm", "pmm", "", "pmm"))
  expect_warning(mice:::check.method(z5, data))
  expect_error(mice:::check.method(z6, data))
  expect_error(mice:::check.method(z7, data))
  expect_length(y8$method, 0)
})

test_that("check.predictorMatrix produces proper results", {
  expect_equal(dim(x1$predictorMatrix), c(3, 4))
  expect_equal(dim(x2$predictorMatrix), c(2, 4))
  expect_equal(dim(x3$predictorMatrix), c(3, 4))
  expect_equal(dim(x4$predictorMatrix), c(4, 4))
  expect_equal(dim(x8$predictorMatrix), c(0, 4))
})


context("check.blocks")

test_that("Unknown variables in blocks not accepted", {
  expect_error(
    mice(nhanes, blocks = list(c("bmi", "chl", "hey"), "weird"), 
         print = FALSE, m = 1, maxit = 1))
})
