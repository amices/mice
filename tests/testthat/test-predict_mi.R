context("predict_mi")

library(dplyr)
library(mice)
library(testthat)

suppressWarnings(RNGversion("3.5.0"))

# load data
dat <- mice::nhanes

# add indicator training and test
# first 20 for training, last 5 for testing
dat$set <- c(rep("train", 20), rep("test", 5))

# Make prediction matrix and ensure that set is not used as a predictor
predmat <- mice::make.predictorMatrix(dat)
predmat[,"set"] <- 0

# train imputation model
imp <- mice(dat, m = 5, maxit = 5 ,seed = 1, 
            predictorMatrix = predmat,
            ignore = ifelse(dat$set == "test", TRUE, FALSE))

# extract the training and test data sets
impdats <- mice::complete(imp, "all")
traindats <- lapply(impdats, function(dat) subset(dat, set == "train", select = -set))
testdats <- lapply(impdats, function(dat) subset(dat, set == "test", select = -c(set)))

# predict age with other variables with training datasets
fits <- lapply(traindats, function(dat) lm(age ~ bmi + hyp + chl, data = dat))

# pool the predictions with function
pool_preds <- predict_mi(object = fits, 
                         newdata = testdats, 
                         pool = TRUE, 
                         interval = "prediction",
                         level = 0.95)

# obtain the imputed value and the width of the prediction interval
predfunc <- function(model, data, level) {
  summfit <- summary(model)
  sigma <- summfit$sigma
  xmat <- model.matrix(summfit$terms, data = as.data.frame(data))
  se <- sqrt(1 + rowSums((xmat %*% summfit$cov.unscaled) * xmat))
  xfit <- xmat %*% model$coefficients
  
  # variance
  var_pred = se^2 * sigma^2
  
  return(cbind(model = xfit, var_pred = var_pred))
}

# obtain predictions for the different imputations
preds_all <- Map(predfunc, model = fits, data = testdats, level = 0.95)

# change the list to a df
preds <- unlist(preds_all) %>% 
  array(dim = c(nrow(testdats[[1]]), 2, 5))

# our estimand is the predicted value
# so Q bar is the mean over the predictions
Q_bar <- apply(preds[, 1, ], 1, mean)

# The uncertainty estimate is the width of the prediction interval
U_bar <- apply(preds[, 2, ], 1, mean)

# The between imputation variance is the variance of the predictions
B <- apply(preds[, 1, ], 1, var)

# The total variance is T + U_bar + B + B/M (with m the number of imputations)
T_var <- U_bar + B + B / 5

# calculate the degrees of freedom for all observations
df_vector <- numeric(length(Q_bar))
t_vector <- numeric(length(Q_bar))

for (i in 1:length(Q_bar)) {
  df_vector[i] <- mice:::barnard.rubin(5, B[i], T_var[i])
  t_vector[i] <- qt(1 - (1-0.95)/2, df_vector[i])
}

# Calculate bounds using individual t-values
lwr <- Q_bar - t_vector * sqrt(T_var)
upr <- Q_bar + t_vector * sqrt(T_var)

# check if the output structure is as expected
test_that("Output class is correct", {
  expect_type(pool_preds, "double")     # checks storage type
  expect_true(is.matrix(pool_preds))    # checks it's a matrix
})

# check if result is the same, for by hand or in function
# Note pool_preds function depends on mice::pool.scalar
test_that("retains same numerical result", {
  expect_equal(unname(pool_preds[, 1]), Q_bar, tolerance = 0.00001)
  expect_equal(unname(pool_preds[, 2]), lwr, tolerance = 0.00001)
  expect_equal(unname(pool_preds[, 3]), upr, tolerance = 0.00001)
})

