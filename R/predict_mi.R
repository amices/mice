#' Predict method for linear models with multiply imputed data
#'
#' @param object A prediction model, either a single lm object or a list of lm
#' objects obtained from multiply imputed data (object can also be of class mira).
#' @param newdata An optional data frame in which to look for variables
#' with which to predict. Can be a data.frame, list, or mids object.
#' If omitted, the fitted values are used.
#' @param pool Logical indicating whether to pool the predictions (and potentially
#' obtain pooled prediction intervals).
#' @param iterations Number of iterations for the bisection algorithm that finds the 
#' mixture quantiles
#' @param probs Quantiles from the posterior predictive distribution corresponding
#' to the given probabilities will be returned (overwrites `level`)
#' @param nsamples Number of samples from the posterior predictive distribution for
#' each imputed dataset. Defaults to `NULL`, in which case only the quantiles 
#' specified by probs of the posterior predictive distribution are provided .
#' @inheritParams stats::predict.lm
#' @inheritDotParams stats::predict.lm
#'
#' @returns If `pool = TRUE`, predict_mi produces a vector of predictions or a
#' matrix of predictions and confidence/prediction bounds. The bounds depend on
#' the probabilities specified by the user. The default returns a prediction matrix
#'  with column names `fit`, `lwr`, `upr` if `interval` is set. For custom probabilities,
#' names are set to the respective probabilities used to calculate the prediction quantiles.
#' For `type = "terms"`, a matrix is returned with a column per term and potentially an 
#' attribute `"constant"`. If `se.fit = TRUE`, a list is returned with the following 
#' components:
#' * `fit`: vector or matrix as above
#' * `se.fit`: standard error of __pooled__ predicted means
#' * `residual scale`: average residual standard deviations
#' * `df`: degrees of freedom for residual (per observation) according to Barnard-
#' Rubin (1999).
#'
#' If `pool = FALSE`, the function produces a list with predictions with the same
#' structure as `predict.lm`, with one list-element per imputed dataset.
#' If `nsamples` is set, this many samples are drawn from the posterior predictive
#' distribution per imputed dataset. The samples are returned in an array with 
#' dimensions n * m * nsamples. 
#' @references Van Leeuwen, F., Volker, T. B., & Van Buuren, S. (n.d.). Prediction
#' intervals with missing data.
#'
#' @seealso [stats::lm()], [stats::predict.lm()], [mice::mice()], [mice::with.mids()]
#'
#' @examples
#' # Dataframe with missings in X
#' # Create imputations and linear model object
#' dat <- mice::nhanes
#'
#' # add indicator training and test
#' # first 20 for training, last 5 for testing
#' dat$set <- c(rep("train", 20), rep("test", 5))
#' dat$age[dat$set == "test"] <- NA # set test cases outcome to missing
#'
#' # Make prediction matrix and ensure that set is not used as a predictor
#' predmat <- mice::make.predictorMatrix(dat)
#' predmat[,"set"] <- 0
#'
#' # Impute missing values based on the train set
#' imp <- mice(dat, m = 20, maxit = 5 , seed = 1, predictorMatrix = predmat,
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
predict_mi <- function(
  object,
  newdata,
  pool = TRUE,
  se.fit = FALSE,
  interval = c("none", "confidence", "prediction"),
  level = 0.95,
  iterations = 20,
  probs = NULL,
  nsamples = NULL,
  ...
) {
  UseMethod("predict_mi")
}

#' @export
predict_mi.mira <- function(
  object,
  newdata,
  pool = TRUE,
  se.fit = FALSE,
  interval = c("none", "confidence", "prediction"),
  level = 0.95,
  iterations = 20,
  probs = NULL,
  nsamples = NULL,
  ...
) {
  args <- list(...)
  interval <- match.arg(
    interval,
    choices = c("none", "confidence", "prediction")
  )
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
    preds <- pool_predictions(
      fit,
      interval = interval,
      level = level,
      args = args,
      iterations = iterations, 
      probs = probs,
      nsamples = nsamples
    )
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
predict_mi.list <- function(
  object,
  newdata,
  pool = TRUE,
  se.fit = FALSE,
  interval = c("none", "confidence", "prediction"),
  level = 0.95,
  iterations = 20,
  probs = NULL,
  nsamples = NULL,
  ...
) {
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
    iterations = iterations,
    probs = probs,
    nsamples = nsamples,
    ...
  )
}

#' @export
predict_mi.lm <- function(
  object,
  newdata,
  pool = TRUE,
  se.fit = FALSE,
  interval = c("none", "confidence", "prediction"),
  level = 0.95,
  iterations = 20,
  probs = NULL,
  nsamples = NULL,
  ...
) {
  args <- list(...)
  interval <- match.arg(
    interval,
    choices = c("none", "confidence", "prediction")
  )
  if (missing(newdata)) {
    newdata <- NULL
  }

  if (!inherits(newdata, c("list", "mids"))) {
    warning(
      "The model `object` nor the `newdata` are the result of multiple\nimputation. Returning regular predictions instead."
    )
    preds <- predict(object, newdata = newdata, se.fit = se.fit, interval = interval, level = level, ...)
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
      preds <- pool_predictions(
        fit,
        interval = interval,
        level = level,
        args = args,
        iterations = iterations,
        probs = probs,
        nsamples = nsamples
      )
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

pool_predictions <- function(predlist, interval, level, iterations, probs, nsamples, args) {
  m <- length(predlist)
  argsnames <- names(args)

  type <- return_type(args)
  if (!isTRUE(is.numeric(iterations)) | !isTRUE(iterations > 0)) stop("iterations must be a positive integer")
  if (!is.null(nsamples)) {
    if (!isTRUE(is.numeric(nsamples)) | !isTRUE(nsamples > 0)) stop("nsamples must be a positive integer")
  }
  if (!is.null(probs)) {
    if (any(0 > probs | probs > 1) | any(!is.numeric(probs))) stop("probs must lie between 0 and 1")
    p <- probs
  } else {
    p <- c((1-level)/2, 1 - (1-level)/2)
  }
  smallest_probdiff <- ifelse(length(p) > 1, min(diff(sort(p))), 1)
  names(p) <- round(p, max(4, -floor(log10(smallest_probdiff))))
    

  pooled <- predlist[[1]]

  n <- nrow(as.matrix(pooled$fit))
  nterms <- if (type == "terms") {
    ncol(pooled$fit)
  } else {
    1
  }
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
        dim = c(n, nterms, 2), 
        dimnames = list(NULL, if (type == "terms") colnames(pooled$fit), c("fit", "se.fit"))
      )
    },
    simplify = "array"
  )
  res.var <- sapply(predlist, \(x) x$residual.scale^2)

  pooled_res <- apply(
    params,
    c(1, 2),
    \(x) {
      res <- pool.scalar(x[1,], x[2,]^2, n = n, k = n - df)
      c(fit = res$qbar, t = res$t, b = res$b, df = res$df, fmi = res$fmi)
    }
  )

  if (!is.matrix(pooled$fit)) {
    pooled$fit <- pooled_res["fit", , ]
  } else {
    pooled$fit[, seq_len(nterms)] <- pooled_res["fit", , ]
  }

  if (type == "terms") {
    attr(pooled$fit, "constant") <- mean(sapply(predlist, function(x) {
      attr(x$fit, "constant")
    }))
  }

  pooled$se.fit <- sqrt(pooled_res["t", , ])
  pooled$residual.scale <- sqrt(mean(res.var))
  pooled$df <- pooled_res["df", , ]

  if (interval %in% c("confidence", "prediction")) {
    if (interval == "confidence") {
      quantiles <- apply(pooled_res, 3, \(x) {
        matrix(x["fit", ], n, length(p)) + 
          matrix(sqrt(x["t", ]), n, length(p)) * 
          sapply(p, \(prob) stats::qt(prob, x["df", ])) |>
          matrix(n, length(p), dimnames = list(NULL, names(p)))
      }, simplify = FALSE)
    } else if (interval == "prediction") {
      b <- pooled_res["b", , ]
      Tpred <- pooled_res["t", , ] + mean(res.var)
      d <- quantile((b/m) / Tpred, 0.95)
      if (d > 0.02 & type != "terms") {
        warning(
          "The MC error of the prediction interval is relatively large. ",
          "At least 5% of the entries have above 2% MC error. Consider",
          " increasing the number of imputations to about m = ", 
          ceiling(m * d / 0.02), "."
        )
      }
      quantiles <- apply(params, 2, \(x) {
        bisection_mixture_interval(
          matrix(x[, 1, ], nrow = n), 
          sqrt(matrix(x[, 2, ]^2, nrow = n) + t(res.var) %x% rep(1, n)), 
          df, 
          p, 
          iterations
        )
      }, simplify = FALSE)
    }
    if (type == "terms") {
      if (is.null(probs)) {
        pooled$lwr <- sapply(quantiles, \(x) x[,1])
        pooled$upr <- sapply(quantiles, \(x) x[,2])
      } else {
        pooled$lwr <- NULL
        pooled$upr <- NULL
        for (x in names(p)) pooled[[x]] <- sapply(quantiles, \(q) q[,x])
      }
    } else {
      if (is.null(probs)) {
        pooled$fit[, "lwr"] <- quantiles[[1]][,1]
        pooled$fit[, "upr"] <- quantiles[[1]][,2]
      } else {
        pooled$fit <- cbind(
          fit = pooled$fit[, "fit"], quantiles[[1]]
        )
      }
    }
  }
  if (!is.null(nsamples)) {
    samples <- apply(params, c(2), \(x) {
      scale <- if (interval == "confidence") x[,2,] else sqrt(x[,2,]^2 + t(res.var) %x% rep(1, n))
      dim <- c(n, m, nsamples)
      array(x[,1,], dim) + array(scale, dim) * array(stats::rt(prod(dim), df), dim)
    }, simplify = FALSE)
    pooled$samples <- samples
  }

  pooled
}

return_type <- function(args) {
  if ("type" %in% names(args)) {
    type <- args$type
  } else {
    type <- "response"
  }
}

remove_sefit <- function(fit) {
  fit <- within(fit, rm(se.fit, df, residual.scale))
  if (length(fit) == 1) {
    fit <- fit$fit
  }
  fit
}

bisection_mixture_interval <- function(preds, scale, df, probs, iterations = 20) {

  sapply(probs, function(prob) {
    lo <- apply(preds + scale * stats::qt(prob, df), 1, min)
    hi <- apply(preds + scale * stats::qt(prob, df), 1, max)
    for (i in seq_len(iterations)) {
      mid <- (lo + hi)/2 # set midpoint of bisection
      decrease_hi <- rowMeans(pt((mid-preds)/scale, df)) > prob # check whether the midpoint exceeds
      lo[!decrease_hi] <- mid[!decrease_hi]
      hi[decrease_hi] <- mid[decrease_hi]
    }
    (lo+hi)/2
  }) |> matrix(nrow = nrow(preds), ncol = length(probs), dimnames = list(NULL, names(probs)))
}