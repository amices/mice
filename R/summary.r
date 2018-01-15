# # ------------------------------summary.mira-------------------------------
# setMethod("summary", signature(object = "mira"), function(object) {
#     summary.mira(object)
# })


#'Summary of a \code{mira} object
#'
#'@rdname summary
#'@param object A \code{mira} object
#'@param ... Other parameters passed down to \code{print()} and \code{summary()}
#'@return \code{NULL}
#'@seealso \code{\link[=mira-class]{mira}}
#'@method summary mira
#'@export
summary.mira <- function(object, 
                         type = c("tidy", "glance", "summary"), 
                         ...) {
  type <- match.arg(type)
  fitlist <- getfit(object)
  if (type == "tidy")
    v <- lapply(fitlist, tidy, effects = "fixed", ...) %>% bind_rows()
  if (type == "glance")
    v <- lapply(fitlist, glance, ...) %>% bind_rows()
  if (type == "summary")
    v <- lapply(fitlist, summary, ...)
  v
}

# # ------------------------------summary.mipo-------------------------------
# setMethod("summary", signature(object = "mipo"), function(object, ...) {
#     summary.mipo(object, ...)
# })
# 

#'Summary of a \code{mipo} object
#'
#'@rdname summary
#'@inheritParams broom::lm_tidiers
#'@return A table containing summary statistis of the pooled analysis
#'@seealso \code{\link[=mipo-class]{mipo}}
#'@method summary mipo
#'@export
# summary.mipo <- function(object, ...) {
#     # summary method for the pooled analysis results
#     # 
#     # object: object of class mipo
#     x <- object
#     table <- array(x$qbar, dim = c(length(x$qbar), 9))
#     dimnames(table) <- list(labels(x$qbar), c("est", "se", "t", "df", "Pr(>|t|)", "lo 95", "hi 95", "nmis", "fmi"))
#     table[, 2] <- sqrt(x$t)
#     table[, 3] <- table[, 1]/table[, 2]
#     table[, 4] <- x$df
#     table[, 5] <- if (all(x$df > 0)) 
#         2 * (1 - pt(abs(table[, 3]), x$df)) else NA
#     table[, 6] <- table[, 1] - qt(0.975, x$df) * table[, 2]
#     table[, 7] <- table[, 1] + qt(0.975, x$df) * table[, 2]
#     if (is.null(x$nmis) || is.null(names(x$qbar)))
#         table[, 8] <- NA else table[, 8] <- x$nmis[names(x$qbar)]
#     table[, 9] <- x$fmi
#     rownames(table) <- x$term
#     table
# }

summary.mipo <- function(object, conf.int = FALSE, conf.level = .95,
                         exponentiate = FALSE, ...) {
  z <- data.frame(
    estimate  = object$qbar,
    std.error = sqrt(object$t),
    statistic = object$qbar / sqrt(object$t),
    p.value   = if (all(object$df > 0)) 
      2 * (1 - pt(abs(object$qbar / sqrt(object$t)), object$df)) else NA,
    riv       = object$r,
    lambda    = object$lambda,
    fmi       = object$fmi,
    stringsAsFactors = FALSE,
    row.names = object$term)
  z <- process_mipo(z, object, conf.int = conf.int, conf.level = conf.level,
                    exponentiate = exponentiate)
  class(z) <- c("mipo.summary", "data.frame")
  z
}


#'Summary of a \code{mids} object
#'
#'@rdname summary
#'@return \code{NULL}
#'@seealso \code{\link[=mids-class]{mids}}
#'@method summary mids
#'@export
summary.mids <- function(object, ...) {
  print(object, ...)
  invisible()
}


#
# ------------------------------summary.mads-------------------------------
#
#'Summary of a \code{mads} object
#'
#'@rdname summary
#'@return \code{NULL}
#'@seealso \code{\link[=mads-class]{mads}}
#'@export
summary.mads <- function(object, ...) {
  print(object, ...)
  invisible()
}

#' helper function to process a tidied mipo object
#' 
#' Adds a confidence interval, and possibly exponentiates, a tidied
#' object.
#' 
#' @param ret data frame with a tidied version of a coefficient matrix
#' @param x an "mipo" object
#' @param conf.int whether to include a confidence interval
#' @param conf.level confidence level of the interval, used only if
#' \code{conf.int=TRUE}
#' @param exponentiate whether to exponentiate the coefficient estimates
#' and confidence intervals (typical for logistic regression)
process_mipo <- function(ret, x, conf.int = FALSE, conf.level = .95,
                         exponentiate = FALSE) {
  if (exponentiate) {
    # save transformation function for use on confidence interval
    if (is.null(x$family) ||
        (x$family$link != "logit" && x$family$link != "log")) {
      warning(paste("Exponentiating coefficients, but model did not use",
                    "a log or logit link function"))
    }
    trans <- exp
  } else {
    trans <- identity
  }
  
  if (conf.int) {
    # avoid "Waiting for profiling to be done..." message
    CI <- suppressMessages(confint(x, level = conf.level))
    # Handle case if regression is rank deficient
    p <- x$rank
    if (!is.null(p) && !is.null(x$qr)) {
      piv <- x$qr$pivot[seq_len(p)]
      CI <- CI[piv, , drop = FALSE]
    }
    colnames(CI) = c("conf.low", "conf.high")
    ret <- cbind(ret, trans(unrowname(CI)))
  }
  ret$estimate <- trans(ret$estimate)
  
  ret
}

vcov.mipo <- function(object, ...) {
  so <- diag(object$t)
  dimnames(so) <- list(object$term, object$term)
  so
}

confint.mipo <- function(object, parm, level = 0.95, ...) {
  cf <- coef(object)
  df <- object$df
  se <- sqrt(object$t)
  pnames <- names(df) <- names(se) <- names(cf)
  if (missing(parm)) 
    parm <- pnames
  else if (is.numeric(parm)) 
    parm <- pnames[parm]
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  fac <- qt(a, object$df)
  pct <- format.perc(a, 3)
  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, 
                                                             pct))
  ci[, 1] <- cf[parm] + qt(1 - a, df[parm]) * se[parm]
  ci[, 2] <- cf[parm] + qt(a, df[parm]) * se[parm]
  ci
}

coef.mipo <- function(object, ...) {
  object$qbar
}

unrowname <- function (x) 
{
  rownames(x) <- NULL
  x
}

format.perc <- function (probs, digits) 
  paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits), 
        "%")

#'Print a \code{mice.anova} object
#'
#'@rdname summary
#'@return \code{NULL}
#'@seealso \code{\link[=mipo-class]{mipo}}
#'@method summary mice.anova
#'@export
summary.mice.anova <- function(object,...) {
  
  # handle objects from anova
  out <- object$out
  # handle objects from D1, D2 and D3
  if (is.null(out))
    out <- list(`1 ~~ 2` = list(result = object$result, 
                                df.com = object$df.com))
  
  test <- names(out)
  df.com <- vapply(out, function(x) x$df.com, numeric(1))
  results <- t(vapply(out, function(x) x$result, numeric(5)))
  rf <- data.frame(test = test, 
                   statistic = results[, 1],
                   df1 = results[, 2], 
                   df2 = results[, 3],
                   df.com = df.com, 
                   p.value = results[, 4],
                   riv = results[, 5],
                   row.names = NULL)
  
  formulas <- object$formulas
  ff <- data.frame(model = names(formulas),
                   formula = as.character(formulas))
  
  structure(list(models = ff, comparisons = rf,
                 m = object$m, method = object$method, use = object$use),
            class = c("mice.anova.summary", class(object)))
}


