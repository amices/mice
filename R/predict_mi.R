#' Predict method for linear models with multiply imputed data
#'
#' @param object A prediction model, either a single lm object or a list of lm
#' objects obtained from multiply imputed data (object can also be of class mira).
#' @param newdata An optional data frame in which to look for variables 
#' with which to predict. Can be a data.frame, list, or mids object. 
#' If omitted, the fitted values are used.
#' @param pool Logical indicating whether to pool the predictions (and potentially
#' obtain pooled prediction intervals).
#' @inheritParams stats::predict.lm
#' @inheritDotParams stats::predict.lm
#'
#' @returns If `pool = TRUE`, predict_mi produces a vector of predictions or a
#' matrix of predictions and bounds with column names `fit`, `lwr`, `upr` if
#' additionally `interval` is set. For `type = "terms"` this is a matrix with a
#' column per term and may have an attribute `"constant"`. If `se.fit = TRUE`, a
#' list is returned with the following components:
#'
#' * `fit`: vector or matrix as above
#' * `se.fit`: standard error of __pooled__ predicted means
#' * `residual scale`: average residual standard deviations
#' * `df`: degrees of freedom for residual (per observation) according to Barnard-
#' Rubin (1999).
#'
#' If `pool = FALSE`, the function produces a list with predictions with the same
#' structure as `predict.lm`, with one list-element per imputed dataset.
#'
#' @seealso [stats::lm()], [stats::predict.lm()], [mice::mice()], [mice::with.mids()]
#'
#' @examples
#' # Dataframe with missings in X
#' # Create Imp and Lm object
#' dat <- mice::nhanes
#'
#' # add indicator training and test
#' # first 20 for training, last 5 for testing
#' dat$set <- c(rep("train", 20), rep("test", 5))
#'
#' # Make prediction matrix and ensure that set is not used as a predictor
#' predmat <- mice::make.predictorMatrix(dat)
#' predmat[,"set"] <- 0
#' 
#' # Impute missing values based on the train set
#' imp <- mice(dat, m = 5, maxit = 5 , seed = 1, predictorMatrix = predmat, 
#'   ignore = ifelse(dat$set == "test", TRUE, FALSE), print = FALSE)
#' impdats <- complete(imp, "all")
#' 
#' # extract the training and test data sets
#' traindats <- lapply(impdats, \(dat) subset(dat, set == "train", select = -set))
#' testdats <- lapply(impdats, \(dat) subset(dat, set == "test", select = -c(set)))
#'
#' # Fit the prediction models, based on the imputed training data
#' fits <- lapply(traindats, \(dat) lm(age ~ bmi + hyp + chl, data = dat))
#'
#' # pool the predictions with function
#' pool_preds <- mice::predict_mi(object = fits, newdata = testdats, 
#'   pool = TRUE, interval = "prediction", level = 0.95)
#' @export
predict_mi <- function(object,
                       newdata,
                       pool = TRUE,
                       se.fit = FALSE,
                       interval = c("none", "confidence", "prediction"),
                       level = 0.95,
                       ...) {
  UseMethod("predict_mi")
}

#' @export
predict_mi.mira <- function(object,
                            newdata,
                            pool = TRUE,
                            se.fit = FALSE,
                            interval = c("none", "confidence", "prediction"),
                            level = 0.95,
                            ...) {
  cl <- match.call()
  args <- list(...)
  interval <- match.arg(interval, choices = c("none", "confidence", "prediction"))
  if (missing(newdata)) {
    newdata <- NULL
  }
  if (!inherits(object$analyses[[1]], "lm")) {
    stop("`predict_mi()` currently only works with the linear model.")
  }
  
  if (!inherits(newdata, c("list", "mids"))) {
    fit <- lapply(
      object$analyses,
      predict,
      newdata = newdata,
      se.fit = TRUE,
      interval = interval,
      level = level,
      ...
    )
  } else {
    if (inherits(newdata, "mids")) {
      newdata <- mice::complete(newdata, action = "all")
    }
    if (length(object$analyses) != length(newdata)) {
      stop(
        "`object` and `newdata` are the result of multiple imputation, but have\ndifferent lengths. Both should stem from the same imputation model."
      )
    }
    fit <- Map(
      predict,
      newdata = newdata,
      object = object$analyses,
      se.fit = TRUE,
      interval = interval,
      level = level,
      ...
    )
  }
  if (pool) {
    preds <- pool_predictions(fit,
                              interval = interval,
                              level = level,
                              args = args)
    if (!se.fit) {
      preds <- remove_sefit(preds)
    }
  } else {
    preds <- fit
    if (!se.fit) {
      preds <- lapply(preds, remove_sefit)
    } else {
      warning(
        "`se.fit` does not reflect uncertainty from missing data,\nwhich requires `pool = TRUE`."
      )
    }
  }
  preds
}

#' @export
predict_mi.list <- function(object,
                            newdata,
                            pool = TRUE,
                            se.fit = FALSE,
                            interval = c("none", "confidence", "prediction"),
                            level = 0.95,
                            ...) {
  cl <- match.call()
  args <- list(...)
  if (missing(newdata)) {
    newdata <- NULL
  }
  
  object <- as.mira(object)
  
  predict_mi.mira(
    object,
    newdata = newdata,
    pool = pool,
    se.fit = se.fit,
    interval = interval,
    level = level,
    ...
  )
}

#' @export
predict_mi.lm <- function(object,
                          newdata,
                          pool = TRUE,
                          se.fit = FALSE,
                          interval = c("none", "confidence", "prediction"),
                          level = 0.95,
                          ...) {
  cl <- match.call()
  args <- list(...)
  interval <- match.arg(interval, choices = c("none", "confidence", "prediction"))
  if (missing(newdata)) {
    newdata <- NULL
  }
  
  
  if (!inherits(newdata, c("list", "mids"))) {
    warning(
      "The model `object` nor the `newdata` are the result of multiple\nimputation. Returning regular predictions instead."
    )
    preds <- predict(object, newdata = newdata, ...)
  } else {
    if (inherits(newdata, "mids")) {
      newdata <- mice::complete(newdata, action = "all")
    }
    fit <- lapply(
      newdata,
      predict,
      object = object,
      se.fit = TRUE,
      interval = interval,
      level = level,
      ...
    )
    if (pool) {
      preds <- pool_predictions(fit,
                                interval = interval,
                                level = level,
                                args = args)
      if (!se.fit) {
        # TODO: maybe only calculate se.fit if necessary (se.fit = TRUE / interval \in confidence or prediction)
        preds <- remove_sefit(preds)
      }
    } else {
      preds <- fit
      if (!se.fit) {
        preds <- lapply(preds, remove_sefit)
      } else {
        warning(
          "`se.fit` does not reflect uncertainty from missing data,\nwhich requires `pool = TRUE`."
        )
      }
    }
  }
  preds
}

pool_predictions <- function(predlist, interval, level, args) {
  m <- length(predlist)
  argsnames <- names(args)
  
  type <- return_type(args)
  
  pooled <- predlist[[1]]
  
  n <- nrow(as.matrix(pooled$fit))
  nterms <- if (type == "terms")
    ncol(pooled$fit)
  else
    1
  df <- pooled$df
  
  params <- sapply(
    # stack predictions in array: 1st dim observations
    predlist,
    #                             2nd dim terms
    function(pred) {
      #                             3rd dim predicted value or standard error
      array(
        #                             4th dim imputations
        c(as.matrix(pred$fit)[, seq_len(nterms)], pred$se.fit),
        dim = c(n, nterms, 2)
      )
    },
    simplify = "array"
  )
  
  ubar_pred <- mean(sapply(predlist, function(x)
    x$residual.scale^2))
  
  pooled_preds <- apply(
    params,
    c(1, 2),
    # separate observations and terms extract fitted values and variances
    function(x) {
      # to pool over imputations for each observation and each term.
      pooled_obs <- mice::pool.scalar(
        Q = x[1, ],
        U = x[2, ]^2,
        #TODO: check whether n and k are necessary
        # n = n,
        # k = n - df
      )
      c(
        pooled_obs$qbar,
        pooled_obs$t,
        pooled_obs$df,
        pooled_obs$t + ubar_pred,
        barnard.rubin(m, pooled_obs$b, pooled_obs$t + ubar_pred) # TODO: check whether df is necessary
      )
    }
  )
  
  if (!is.matrix(pooled$fit)) {
    pooled$fit <- pooled_preds[1, , ]
  } else {
    pooled$fit[, seq_len(nterms)] <- pooled_preds[1, , ]
  }
  
  if (type == "terms") {
    attr(pooled$fit, "constant") <- mean(sapply(predlist, function(x)
      attr(x$fit, "constant")))
  }
  
  pooled$se.fit <- sqrt(pooled_preds[2, , ])
  pooled$df <- pooled_preds[3, , ]
  pooled$residual.scale <- mean(sapply(predlist, function(x)
    x$residual.scale))
  
  if (interval == "confidence") {
    tscale <- pooled_preds[2, , ]
    df_pred <- pooled_preds[3, , ]
  } else if (interval == "prediction") {
    tscale <- pooled_preds[4, , ]
    df_pred <- pooled_preds[5, , ]
  }
  
  if (interval %in% c("confidence", "prediction")) {
    lwr <- pooled_preds[1, , ] - sqrt(tscale) * qt(1 - (1 - level) / 2, df_pred)
    upr <- pooled_preds[1, , ] + sqrt(tscale) * qt(1 - (1 - level) / 2, df_pred)
    
    if (type == "terms") {
      pooled$lwr <- lwr
      pooled$upr <- upr
    } else {
      pooled$fit[, "lwr"] <- lwr
      pooled$fit[, "upr"] <- upr
    }
  }
  pooled
}

return_type <- function(args) {
  if ("type" %in% names(args)) {
    type <- args$type
  } else {
    type <- "reponse"
  }
}

remove_sefit <- function(fit) {
  fit <- within(fit, rm(se.fit, df, residual.scale))
  if (length(fit) == 1) {
    fit <- fit$fit
  }
  fit
}

# pool.preds.lm <- function(object,
#                           new_data = NULL,
#                           interval = "prediction",
#                           level = 0.95) {
#   #  Make sure that we have multiple pred models or datasets, and that the prediction model is lm
#   call <- match.call()
#   if (!is.mira(object) & !is.mipo(object)) {
#     if (is.data.frame(new_data)) {
#       stop(
#         "The prediction model must have class 'mira' or 'mipo' if there are no multiple imputed new data."
#       )
#     }
#     if (class(object) != "lm") {
#       stop(
#         "The pooled predictions can only be calculated for results of the 'lm' modeling function"
#       )
#     }
#   }
#   
#   # Check to see if we have more than one imputation, and the same number of pred models and imp
#   # Make sure that we have an lm model
#   if (is.mira(object)) {
#     if ((m <- length(object$analyses)) < 2) {
#       stop("At least two prediction models are needed for pooling.\n")
#     }
#     if ((m <- length(object$analyses)) > 2 &
#         !is.data.frame(new_data)) {
#       if (m != length(new_data) & !is.null(new_data)) {
#         stop(
#           "The number of prediction models whould be the same as the number of imputed samples for the new data.\n"
#         )
#       }
#     }
#     if (class((object$analyses[[1]]))[1] != "lm") {
#       stop(
#         "The pooled predictions can only be calculated for results of the 'lm' modeling function"
#       )
#     }
#   }
#   
#   # check what type of interval is requested
#   if (interval[1] != "prediction" & interval[1] != "none") {
#     stop("The interval argument should be either 'prediction' or 'none'.")
#   }
#   
#   # make prediction on the training set if no new data is specified
#   if (is.null(new_data)) {
#     if (is.mira(object)) {
#       print("No new data provided, using the original data from the model.")
#       new_data <- lapply(object$analyses, function(x) {
#         x$model
#       })
#     } else {
#       stop("With only a single prediction model and no new data, pooling is not possible.")
#     }
#   }
#   
#   # check the level that should be used
#   
#   if (level < 0 | level > 1) {
#     stop("The level should be between 0 and 1.")
#   }
#   
#   # obtain the fitted models
#   if (is.mira(object)) {
#     fits <- object$analyses # extract the fitted analyses
#   } else {
#     fits <- list(object)
#   }
#   
#   # check how many test sets there are
#   if (is.data.frame(new_data)) {
#     new_data <- list(new_data)
#   }
#   
#   # obtain the predictions with the interval
#   preds <- Map(pred_int,
#                model = fits,
#                data = new_data,
#                level = level)
#   
#   # make sure we have the correct length
#   len <- max(length(new_data), length(fits))
#   
#   # pool the predictions,
#   # see test-pool.preds for the calculation without pool.scalar
#   out <- unlist(preds) |>
#     array(dim = c(nrow(new_data[[1]]), 2, len)) |>
#     apply(1, \(x) {
#       pooled <- mice::pool.scalar(Q = x[1, ], U = x[2, ])
#       pooled$qbar + c(0, -1, 1) * qt(1 - (1 - level) / 2, pooled$df) * sqrt(pooled$t)
#     }) |>
#     t()
#   
#   # tidy output
#   # # only return the predictions if the PI is not needed
#   if (interval == "prediction") {
#     return(data.frame(
#       fit = out[, 1],
#       lwr = out[, 2],
#       upr = out[, 3]
#     ))
#   } else {
#     return(data.frame(fit = out[, 1]))
#   }
# }

#' Title
#' 
#' @param model lm objeact
#' @param data data frame with the new data to predict
#' @param level confidence level for the prediction interval, default is 0.95
#' 
#' @returns predictions and the width of the prediction interval
#' @keywords internal
#' @noRd
# pred_int <- function(model, data, level = 0.95) {
#   preds <- predict(model,
#                    newdata = data,
#                    interval = "prediction",
#                    level = level)
#   
#   # obtain the predictions with with the prediction variance
#   # by removing the t value that is used based on df
#   t =  qt(1 - (1 - level) / 2, model$df.residual)
#   var_pred = ((preds[, 3] - preds[, 2]) / (2 * t))^2
#   return(data.frame(pred = preds[, 1], var_pred = var_pred))
# }
