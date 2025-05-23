#' \code{mipo}: Multiple imputation pooled object
#'
#' The \code{mipo} object contains the results of the pooling step.
#' The function \code{\link{pool}} generates an object of class \code{mipo}.
#'
#' @param x An object of class \code{mipo}
#' @param object An object of class \code{mipo}
#' @param mira.obj An object of class \code{mira}
#' @inheritParams broom::lm_tidiers
#' @param z Data frame with a tidied version of a coefficient matrix
#' @param conf.int Logical indicating whether to include
#' a confidence interval.
#' @param conf.level Confidence level of the interval, used only if
#' \code{conf.int = TRUE}. Number between 0 and 1.
#' @param exponentiate Flag indicating whether to exponentiate the
#' coefficient estimates and confidence intervals (typical for
#' logistic regression).
#' @param \dots Arguments passed down
#' @details An object class \code{mipo} is a \code{list} with
#' elements: \code{call}, \code{m}, \code{pooled} and \code{glanced}.
#'
#' The \code{pooled} elements is a data frame with columns:
#' \tabular{ll}{
#' \code{estimate}\tab Pooled complete data estimate\cr
#' \code{ubar}    \tab Within-imputation variance of \code{estimate}\cr
#' \code{b}       \tab Between-imputation variance of \code{estimate}\cr
#' \code{t}       \tab Total variance, of \code{estimate}\cr
#' \code{dfcom}   \tab Degrees of freedom in complete data\cr
#' \code{df}      \tab Degrees of freedom of $t$-statistic\cr
#' \code{riv}     \tab Relative increase in variance\cr
#' \code{lambda}  \tab Proportion attributable to the missingness\cr
#' \code{fmi}     \tab Fraction of missing information\cr
#' }
#' The names of the terms are stored as \code{row.names(pooled)}.
#'
#' The \code{glanced} elements is a \code{data.frame} with \code{m} rows.
#' The precise composition depends on the class of the complete-data analysis.
#' At least field \code{nobs} is expected to be present.
#'
#' The \code{process_mipo} is a helper function to process a
#' tidied mipo object, and is normally not called directly.
#' It adds a confidence interval, and optionally exponentiates, the result.
#' @seealso \code{\link{pool}},
#' \code{\link{mids}}, \code{\link{mira}}
#' @references van Buuren S and Groothuis-Oudshoorn K (2011). \code{mice}:
#' Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#' Statistical Software}, \bold{45}(3), 1-67.
#' \doi{10.18637/jss.v045.i03}
#' @keywords classes
#' @name mipo
NULL

#' @rdname mipo
#' @export
mipo <- function(mira.obj, ...) {
  if (!is.mira(mira.obj)) stop("`mira.obj` not of class `mira`")
  structure(pool(mira.obj, ...), class = c("mipo"))
}

#' @return The \code{summary} method returns a data frame with summary statistics of the pooled analysis.
#' @rdname mipo
#' @export
summary.mipo <- function(object, type = c("tests", "all"),
                         conf.int = FALSE, conf.level = .95,
                         exponentiate = FALSE, ...) {
  type <- match.arg(type)
  x <- object$pooled
  z <- summary_mipo.workhorse(x = x, type = type,
                              conf.int = conf.int, conf.level = conf.level,
                              exponentiate = exponentiate, ...)
  class(z) <- c("mipo.summary", "data.frame")
  z
}

summary_mipo.workhorse <- function(x, type,
                                   conf.int, conf.level,
                                   exponentiate, ...) {
  m <- x$m[1L]
  std.error <- sqrt(x$t)
  statistic <- x$estimate / std.error
  p.value <- 2 * (pt(abs(statistic), pmax(x$df, 0.001), lower.tail = FALSE))

  z <- data.frame(x,
                  std.error = std.error,
                  statistic = statistic,
                  p.value = p.value
  )
  z <- process_mipo(z, x,
                    conf.int = conf.int,
                    conf.level = conf.level,
                    exponentiate = exponentiate)

  if (type == "tests") {
    out <- c("m", "riv", "lambda", "fmi", "ubar", "b", "t", "dfcom")
    keep <- base::setdiff(names(z), out)
    z <- z[, keep]
  }

  return(z)
}

#' @rdname mipo
#' @export
print.mipo <- function(x, ...) {
  cat("Class: mipo    m =", x$m, "\n")
  print.data.frame(x$pooled, ...)
  invisible(x)
}

#' @rdname mipo
#' @export
print.mipo.summary <- function(x, ...) {
  print.data.frame(x, ...)
  invisible(x)
}

#' @rdname mipo
#' @keywords internal
process_mipo <- function(z, x, conf.int = FALSE, conf.level = .95,
                         exponentiate = FALSE) {
  if (exponentiate) {
    # save transformation function for use on confidence interval
    trans <- exp
  } else {
    trans <- identity
  }

  CI <- NULL
  if (conf.int) {
    CI <- confidence(x, level = conf.level)
  }
  z$estimate <- trans(z$estimate)

  # combine and place columns in desired order
  parnames <- z[["term"]]
  if (!is.null(CI)) {
    z <- cbind(
      term = parnames,
      z[, c("m", "estimate", "std.error", "statistic", "df", "p.value")],
      trans(unrowname(CI)),
      conf.low = trans(unrowname(CI[, 1L])),
      conf.high = trans(unrowname(CI[, 2L])),
      z[, c("riv", "lambda", "fmi", "ubar", "b", "t", "dfcom")]
    )
  } else {
    z <- cbind(
      term = parnames,
      z[, c("m", "estimate", "std.error", "statistic", "df", "p.value")],
      z[, c("riv", "lambda", "fmi", "ubar", "b", "t", "dfcom")]
    )
  }
  z
}

confidence <- function(pooled, parm, level = 0.95, ...) {
  cf <- pooled$estimate
  df <- pooled$df
  se <- sqrt(pooled$t)
  pnames <- names(df) <- names(se) <- names(cf) <- row.names(pooled)
  if (missing(parm)) {
    parm <- pnames
  } else if (is.numeric(parm)) {
    parm <- pnames[parm]
  }
  a <- (1 - level) / 2
  a <- c(a, 1 - a)
  fac <- qt(a, df)
  pct <- fmt.perc(a, 3)
  ci <- array(NA,
              dim = c(length(parm), 2L),
              dimnames = list(parm, pct)
  )
  ci[, 1] <- cf[parm] + qt(a[1], df[parm]) * se[parm]
  ci[, 2] <- cf[parm] + qt(a[2], df[parm]) * se[parm]
  return(ci)
}

unrowname <- function(x) {
  rownames(x) <- NULL
  x
}

fmt.perc <- function(probs, digits) {
  paste(
    format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits),
    "%"
  )
}
