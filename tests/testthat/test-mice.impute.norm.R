#########################
#TEST 1: Simple problem #
#########################
set.seed(123)

#generate data
y <- rnorm(10)
x <- y * .3 + rnorm(10, 0, .25)
x2 <- x + rnorm(10, 2, 3)
x <- cbind(1, x, x2)

#make missingness
y[5:6] <- NA
ry <- !is.na(y)

set.seed(123)
svd <- .norm.draw(y, ry, x, ls.meth = "svd")
set.seed(123)
ridge <- .norm.draw(y, ry, x, ls.meth = "ridge")
set.seed(123)
qr <- .norm.draw(y, ry, x, ls.meth = "qr")

#tests for test1
expect_equal(svd$coef, matrix(qr$coef))
expect_equal(svd$beta, matrix(qr$beta))
expect_equal(svd$sigma, qr$sigma)
expect_equal(svd$estimation, "svd")
expect_equal(qr$estimation, "qr")
expect_equal(ridge$estimation, "ridge")
#svd and qr deliver same estimates; ridge should be different!

#####################################
#TEST 2: extremely high correlation #
#####################################
x <- matrix(c(1:1000, seq(from = 2, to = 2000, by=2)) + rnorm(1000), nrow = 1000, ncol = 2)
y <- t(c(5, 3) %*% t(x))
y[5:6] <- NA
ry <- !is.na(y)

svd <- .norm.draw(y, ry, x, ls.meth = "svd")
ridge <- .norm.draw(y, ry, x, ls.meth = "ridge")
qr <- .norm.draw(y, ry, x, ls.meth = "qr")

#tests for test2
expect_equal(svd$coef, matrix(qr$coef))
expect_equal(svd$beta, matrix(qr$beta))
expect_equal(svd$sigma, qr$sigma)
expect_equal(svd$estimation, "svd")
expect_equal(qr$estimation, "qr")
expect_equal(ridge$estimation, "ridge")
#svd and qr deliver same estimates; ridge should be different!

# test on rank deficient (perfect correlation) - BREAKS!!!!!!!!!!!!!!!!!!!!!!
x <- matrix(c(1:1000, seq(from = 2, to = 2000, by=2)), nrow = 1000, ncol = 2)
y <- t(c(5, 3) %*% t(x))
y[5:6] <- NA
ry <- !is.na(y)

svd <- .norm.draw(y, ry, x, ls.meth = "svd")
ridge <- .norm.draw(y, ry, x, ls.meth = "ridge")
qr <- .norm.draw(y, ry, x, ls.meth = "qr")

#tests for test3
expect_equal(svd$estimation, "svd")
expect_equal(qr$estimation, "qr")
expect_equal(ridge$estimation, "ridge")
expect_warning(.norm.draw(y, ry, x, ls.meth = "svd"), NA)
expect_error(.norm.draw(y, ry, x, ls.meth = "svd"), NA)
expect_warning(.norm.draw(y, ry, x, ls.meth = "qr"), NA)
expect_error(.norm.draw(y, ry, x, ls.meth = "qr"), NA)
expect_warning(.norm.draw(y, ry, x, ls.meth = "ridge"), NA)
expect_error(.norm.draw(y, ry, x, ls.meth = "ridge"), NA)
#Even though x is rank deficient, the methods should default to adding a penalty
#to matrix v in order for LAPACK to circumvent singularity.