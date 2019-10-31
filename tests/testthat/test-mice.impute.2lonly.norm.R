context("mice.impute.2lonly.norm")

## https://stackoverflow.com/questions/58266785/mice-2l-pan-multilevel-multiple-imputation-error-missing-values-in-pred-not-all?sem=2

library("pan")

#Not multilevel to illustrate need

set.seed(100)

data <- data.frame(patid = rep(1:4, each = 5),
                   sex = rep(c(1, 2, 1, 2), each = 5),
                   crp = c(68, 78, 93, NA, 143, 
                           5,  7,  9, 13,  NA, 
                           97, NA, 56, 52,  34,
                           22, 30, NA, NA, 45))
pred <- make.predictorMatrix(data)
pred[, "patid"] <- -2

# only missing value (out of five) for patid == 1
data[3, "sex"] <- NA

test_that("2lonly.norm stops with partially missing level-2 data", {
  expect_error(mice(data, method = c("","2lonly.norm", "2l.pan"), 
                    predictorMatrix = pred, maxit = 1, m = 2, print = FALSE),
               "Method 2lonly.norm found the following clusters with partially missing\n  level-2 data: 1\n  Method 2lonly.mean can fix such inconsistencies.")
})


set.seed(66322)
y <- popmis[1:200, "texp"]
x <- popmis[1:200, c("pupil", "school", "sex")]
y[x$school %in% 1:3] <- NA
ry <- !is.na(y)
wy1 <- !ry
wy2 <- rep(TRUE, length(y))
wy3 <- rep(FALSE, length(y))
wy4 <- rep(c(TRUE, FALSE), times = c(1, length(y) - 1))
type <- c(1, -2, 1)
yn <- y

y <- as.numeric(y)

set.seed(1)
z1 <- mice.impute.2lonly.norm(y, ry = ry, x, type)
z2 <- mice.impute.2lonly.pmm(y, ry = ry, x, type)
