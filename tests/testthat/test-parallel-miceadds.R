library(mice)
library(miceadds)

#-- simulate data
set.seed(1)
N <- 100
x <- stats::rnorm(N)
z <- 0.5*x + stats::rnorm(N, sd=.7)
y <- stats::rnorm(N, mean=.3*x - .2*z, sd=1 )
dat <- data.frame(x,z,y)
dat[ seq(1,N,3), c("x","y") ] <- NA
dat[ seq(1,N,4), "z" ] <- NA

#-- use imputation methods from miceadds
method <- c("x" = "rlm", "z" = "lm", "y" = "lqs")

#-- impute data - single threaded
set.seed(1)
expect_silent(imps <- mice::mice(dat, method = method, maxit = 2, print = FALSE))

#-- impute data - parallel
set.seed(1)
expect_silent(impp <- mice::mice(dat, method = method, maxit = 2, parallel = TRUE, future.packages = "miceadds", print = FALSE))
