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
summary.mira <- function(object, ...) {
  # This summary function is for a mira object.  Then the seperate analyses are of class lm (glm), it calls sequentially
  # summary.lm (summary.glm) for all analyses.  KO, 4/2/00
  
  for (i in seq_along(object$analyses)) {
    cat("\n", "## summary of imputation", i, ":\n")
    print(summary(object$analyses[[i]], ...), ...)
  }
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
  # summary method for the pooled analysis results
  # 
  # object: object of class mipo
  x <- object
  ret <- data.frame(
    term      = x$term,
    estimate  = x$qbar,
    std.error = sqrt(x$t),
    statistic = x$qbar / sqrt(x$t),
    p.value   = if (all(x$df > 0)) 2 * (1 - pt(abs(sqrt(x$t)), x$df)) else NA,
    riv       = x$r,
    fmi       = x$fmi,
    stringsAsFactors = FALSE,
    row.names = NULL)
  process_mipo(ret, x, conf.int = conf.int, conf.level = conf.level,
               exponentiate = exponentiate)
}


# # --------------------------------SUMMARY.MIDS--------------------------------------
# setMethod("summary", signature(object = "mids"), function(object, ...) {
#     summary.mids(object, ...)
# })

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
  rl <- object$result
  rf <- data.frame(test = names(rl),
                   statistic = vapply(rl, function(x) x$test[1], numeric(length(rl))),
                   df1 = sapply(rl, function(x) x$test[2]),
                   df2 = sapply(rl, function(x) x$test[3]),
                   df.com = sapply(rl, function(x) x$df.com),
                   p.value = sapply(rl, function(x) x$test[4]),
                   riv = sapply(rl, function(x) x$test[5]),
                   row.names = NULL)
  
  formulas <- object$formulas
  ff <- data.frame(model = names(formulas),
                   formula = as.character(formulas))
  
  structure(list(models = ff, comparisons = rf,
                 m = object$m, method = object$method, use = object$use),
            class = c("mice.anova.summary", class(object)))
}


