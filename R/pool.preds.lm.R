# roxygen

#' Pool the predictions, and the prediction interval, for a linear regression model
#'
#' @param object An prediction model, either a single lm object or a multiple imputed object of class 'mira' or 'mipo'.
#' @param new_data 	An optional data frame, or list with data frames, in which to look for variables with which to predict. If omitted, the values from the prediction models are used.
#' @param interval 	Type of interval calculation. Can be either "prediction", or "none". If "prediction", the pooled prediction interval is calculated, otherwise only the pooled predictions are returned.
#' @param level Tolerance/confidence level. The default is set to 0.95, giving a 95% prediction interval. The level should be between 0 and 1.
#'
#' @returns A data frame with the pooled predictions and the prediction interval, or only the pooled predictions if specified.  The data frame contains three columns: 'fit' for the predicted values, 'lwr' for the lower bound of the prediction interval, and 'upr' for the upper bound of the prediction interval.
#'
#' @examples
#' # Dataframe with missings in X
#' # Create Imp and Lm object
#' dat <- mice::nhanes
#'
#' # Split the data in training and test set
#' train <- dat[1:20, ]
#' test <- dat[21:25, ]
#'
#' # Impute missing values in train, and create a imputation model
#' imp_train <- mice::mice(train, m = 5, maxit = 5, task = "train")
#'
#' # Fit the prediction models, based on the imputed training data
#' fits <- with(imp_train, lm(age ~ bmi + hyp + chl))
#'
#' # To ensure the data types match between train and test use mice::coerce
#' # Fit the imputation model on the test set
#' test_imp <- mice::mice(test, tasks = "fill", models = imp_train$models)
#'
#' # Retrieve the imputed datasets
#' test_imp <- complete(test_imp, "all")
#'
#' # Pool the predictions for the test set
#' pool.preds.lm(fits, test_imp)
pool.preds.lm <- function(object,
                          new_data = NULL,
                          interval = "prediction",
                          level = 0.95) {
  #  Make sure that we have multiple pred models or datasets, and that the prediction model is lm
  call <- match.call()
  if (!is.mira(object) & !is.mipo(object)) {
    if (is.data.frame(new_data)) {
      stop(
        "The prediction model must have class 'mira' or 'mipo' if there are no multiple imputed new data."
      )
    }
    if (class(object) != "lm") {
      stop(
        "The pooled predictions can only be calculated for results of the 'lm' modeling function"
      )
    }
  }

  # Check to see if we have more than one imputation, and the same number of pred models and imp
  # Make sure that we have an lm model
  if (is.mira(object)) {
    if ((m <- length(object$analyses)) < 2) {
      stop("At least two prediction models are needed for pooling.\n")
    }
    if ((m <- length(object$analyses)) > 2 &
      !is.data.frame(new_data)) {
      if (m != length(new_data) & !is.null(new_data)) {
        stop(
          "The number of prediction models whould be the same as the number of imputed samples for the new data.\n"
        )
      }
    }
    if (class((object$analyses[[1]]))[1] != "lm") {
      stop(
        "The pooled predictions can only be calculated for results of the 'lm' modeling function"
      )
    }
  }

  # check what type of interval is requested
  if (interval[1] != "prediction" & interval[1] != "none") {
    stop("The interval argument should be either 'prediction' or 'none'.")
  }

  # make prediction on the training set if no new data is specified
  if (is.null(new_data)) {
    if (is.mira(object)) {
      print("No new data provided, using the original data from the model.")
      new_data <- lapply(object$analyses, function(x) {
        x$model
      })
    } else {
      stop("With only a single prediction model and no new data, pooling is not possible.")
    }
  }

  # check the level that should be used

  if (level < 0 | level > 1) {
    stop("The level should be between 0 and 1.")
  }

  # obtain the fitted models
  if (is.mira(object)) {
    fits <- object %>%
      .$analyses # extract the fitted analyses
  } else {
    fits <- list(object)
  }

  # check how many test sets there are
  if (is.data.frame(new_data)) {
    new_data <- list(new_data)
  }

  # obtain the predictions with the interval
  preds <- Map(pred_int, model = fits, data = new_data, level = level)

  # make sure we have the correct length
  len <- max(length(new_data), length(fits))

  # pool the predictions,
  # see test-pool.preds for the calculation without pool.scalar
  out <- unlist(preds) |>
    array(dim = c(nrow(new_data[[1]]), 2, len)) |>
    apply(1, \(x) {
      pooled <- mice::pool.scalar(Q = x[1, ], U = x[2, ])
      if (is.na(pooled$b)) {
        pooled$t <- pooled$ubar
      }
      pooled$qbar + c(0, -1, 1) * pooled$t
    }) |>
    t()

  # tidy output
  # # only return the predictions if the PI is not needed
  if (interval == "prediction") {
    return(data.frame(
      fit = out[, 1],
      lwr = out[, 2],
      upr = out[, 3]
    ))
  } else {
    return(data.frame(fit = out[, 1]))
  }
}

#' Title
#'
#' @param model lm objeact
#' @param data data frame with the new data to predict
#' @param level confidence level for the prediction interval, default is 0.95
#'
#' @returns predictions and the width of the prediction interval
#' @keywords internal
#' @noRd

pred_int <- function(model, data, level = 0.95) {
  preds <- predict(model,
    newdata = data,
    interval = "prediction",
    level = level
  )
  return(data.frame(pred = preds[, 1], PI_width = preds[, 3] - preds[, 2]))
}
