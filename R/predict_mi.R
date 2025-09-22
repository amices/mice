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
#' traindats <- lapply(impdats, function(dat) subset(dat, set == "train", select = -set))
#' testdats <- lapply(impdats, function(dat) subset(dat, set == "test", select = -c(set)))
#'
#' # Fit the prediction models, based on the imputed training data
#' fits <- lapply(traindats, function(dat) lm(age ~ bmi + hyp + chl, data = dat))
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