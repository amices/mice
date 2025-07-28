context("pool.preds.lm")

suppressWarnings(RNGversion("3.5.0"))

# Obtain dataset, and impute the data
df <- mice::nhanes
train <- df[1:20, ]
test <- df[21:25, ]
imp_train <- mice::mice(train, m = 5, maxit = 5, tasks = "train")
fits <- with(imp_train, lm(age ~ bmi + hyp + chl))
test_imp <- mice::mice(test, tasks = "fill", models = imp_train$models)
test_imp <- complete(test_imp, "all")

# use function
level = 0.95
pool_preds <- pool.preds.lm(fits, test_imp, level = level)

# obtain the imputed value and the width of the prediction interval
predfunc <- function(model, data,level) {
  summfit <- summary(model)
  sigma <- summfit$sigma
  xmat <- model.matrix(summfit$terms, data = as.data.frame(data))
  se <- sqrt(1 + rowSums((xmat %*% summfit$cov.unscaled) * xmat))
  xfit <- xmat %*% model$coefficients
  
  # variance
  var_pred = se^2 * sigma^2
  
  return(cbind(model = xfit, var_pred = var_pred))
}

# obtain analysis models from fits object
fits <- fits %>%
  .$analyses

# obtain predictions for the different imputations
preds_all <- Map(predfunc, model = fits, data = test_imp, level = 0.95)

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
T_var <- U_bar + B + B / 5

# calculate the degrees of freedom for all observations
df_vector <- numeric(length(Q_bar))
t_vector <- numeric(length(Q_bar))

for (i in 1:length(Q_bar)) {
  df_vector[i] <- mice:::barnard.rubin(5, B[i], T_var[i])
  t_vector[i] <- qt(1 - (1-level)/2, df_vector[i])
}

# Calculate bounds using individual t-values
lwr <- Q_bar - t_vector * sqrt(T_var)
upr <- Q_bar + t_vector * sqrt(T_var)

# check if the output structure is as expected
test_that("Output class is correct", {
  expect_is(pool_preds, "data.frame")
})

# check if result is the same, for by hand or in function
# Note pool_preds function depends on mice::pool.scalar
test_that("retains same numerical result", {
  expect_equal(pool_preds[, 1], Q_bar, tolerance = 0.00001)
  expect_equal(pool_preds[, 2], lwr, tolerance = 0.00001)
  expect_equal(pool_preds[, 3], upr, tolerance = 0.00001)
})

