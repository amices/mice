context("rbind.mids")

imp1 <- mice(nhanes[1:13, ], m = 2, maxit = 1, print = FALSE)
imp2 <- mice(nhanes[14:25, ], m = 2, maxit = 1, print = FALSE)
imp3 <- mice(nhanes2, m = 2, maxit = 1, print = FALSE)
imp4 <- mice(nhanes2, m = 1, maxit = 1, print = FALSE)
imp5 <- mice(nhanes[1:13, ], m = 2, maxit = 2, print = FALSE)
imp6 <- mice(nhanes[1:13, 2:3], m = 2, maxit = 2, print = FALSE)
nh3 <- nhanes
colnames(nh3) <- c("AGE", "bmi", "hyp", "chl")
imp7 <- mice(nh3[14:25, ], m = 2, maxit = 2, print = FALSE)
imp8 <- mice(nhanes[1:13, ], m = 2, maxit = 2, print = FALSE)

mylist <- list(age = NA, bmi = NA, hyp = NA, chl = NA)
nhalf <- nhanes[13:25, ]

test_that("Expands number of rows and imputes", {
  expect_equal(nrow(complete(rbind(imp1, imp2))), 25L)
  expect_equal(nrow(rbind(imp1, imp2)$imp$bmi), 9L)
})

test_that("throws error", {
  expect_error(rbind(imp1, imp3), "datasets have different factor variables")
  expect_error(rbind(imp3, imp4), "number of imputations differ")
  expect_error(rbind(imp1, imp6), "datasets have different number of columns")
  expect_error(rbind(imp1, imp7), "datasets have different variable names")
})
test_that("throws warning", {
  expect_warning(r1 <- rbind(imp1, imp5), 
                 "iterations differ, so no convergence statistics calculated")
})

r1 <- rbind(imp8, imp5)
r2 <- rbind(imp1, mylist)
r3 <- rbind(imp1, nhalf)

test_that("Produces longer imputed data", {
  expect_identical(nrow(complete(r1)), 26L)
  expect_identical(nrow(complete(r2)), 14L)
  expect_equal(sum(is.na(complete(r3))), sum(is.na(nhalf)))
})

r11 <- mice.mids(rbind(imp1, imp5))
plot(r11)
#r21 <- mice.mids(r2)
#r31 <- mice.mids(r3)

# issue #59
set.seed <- 818
x <- rnorm(10)
D <- data.frame(x=x, y=2*x+rnorm(10))
D[c(2:4, 7), 1] <- NA
D_mids <- mice(D[1:5,], print = FALSE)
D_rbind <- mice:::rbind.mids(D_mids, D[6:10,])
cmp <- complete(D_rbind, 1)
test_that("Solves issue #59, rbind", expect_identical(cmp[6:10, ], D[6:10, ]))

# calculate chainMean and chainVar
# imp1 <- mice(nhanes[1:13, ], m = 5, maxit = 25, print = FALSE, seed = 123)
# imp2 <- mice(nhanes[14:25, ], m = 5, maxit = 25, print = FALSE, seed = 456)
# z <- rbind(imp1, imp2)
# plot(z)
# 
# imp3 <- mice(nhanes, m = 5, maxit = 25, print = FALSE, seed = 123)
# plot(imp3)
#
# An interesting observation is that the SD(hyp, a) < SD(hyp, imp3). This is 
# because SD(hyp, imp1) = 0.
