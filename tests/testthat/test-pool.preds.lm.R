context("pool.preds.lm")

suppressWarnings(RNGversion("3.5.0"))

# Obtain dataset, and impute the data
dat <- mice::nhanes 
train <-dat[1:20,]
test <- dat[21:25,]
imp_train <- mice::mice(train, m = 5, maxit = 5, task = "train", seed = 6)
fits = with(imp_train, lm(age ~ bmi + hyp + chl))
test_imp <- mice::mice(test, tasks = "fill", models = imp_train$models, seed = 6)
test_imp <- complete(test_imp, "all")

# use function
pool_preds <- pool.preds.lm(fits, test_imp)

# calc by hand, no internal mice functions 

# obtain the imputed value and the width of the prediction interval
pred_int = function(model, data, level = 0.95) {
  preds <- predict(model,
                   newdata = data,
                   interval = "prediction",
                   level = level)
  return(data.frame(pred = preds[, 1], PI_width = preds[, 3] - preds[, 2]))
}

# obtain analysis models from fits object
fits <- fits %>%
  .$analyses

# obtain predictions for the different imputations
preds_all <- Map(pred_int, model = fits, data = test_imp, level = 0.95)

# change the list to a df
preds <- unlist(preds_all) |>
  array(dim = c(nrow(test), 2, 5))

# our estimand is the predicted value
# so Q bar is the mean over the predictions
Q_bar <- apply(preds[, 1, ], 1, mean)

# The uncertainty estimate is the width of the prediction interval
U_bar <- apply(preds[, 2, ], 1, mean)

# The between imputation variance is the variance of the predictions
B <- apply(preds[, 1, ], 1, var) 

# The total variance is T + U_bar + B + B/M (with m the number of imputations)
T_var = U_bar + B + B/5

# lower bound and upper bound
lwr = Q_bar - T_var 
upr = Q_bar + T_var 

# check if the output structure is as expected
test_that("Output class is correct", {
  expect_is(preds_all[[1]], "data.frame")
})

test_that("Output class is correct", {
  expect_is(pool_preds, "data.frame")
})

# check if result is the same, for by hand or in function
# Note pool_preds function depends on mice::pool.scalar 
test_that("retains same numerical result", {
  expect_equal(pool_preds[,1], Q_bar, tolerance = 0.00001)
  expect_equal(pool_preds[,2], lwr, tolerance = 0.00001)
  expect_equal(pool_preds[,3], upr, tolerance = 0.00001)
})
