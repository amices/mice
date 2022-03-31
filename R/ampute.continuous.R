#' Multivariate amputation based on continuous probability functions
#'
#' This function creates a missing data indicator for each pattern. The continuous
#' probability distributions (Van Buuren, 2012, pp. 63, 64) will be induced on the
#' weighted sum scores, calculated earlier in the multivariate amputation function
#' \code{\link{ampute}}.
#'
#' @param P A vector containing the pattern numbers of the cases's candidacies.
#' For each case, a value between 1 and #patterns is given. For example, a
#' case with value 2 is candidate for missing data pattern 2.
#' @param scores A list containing vectors with the candidates's weighted sum scores,
#' the result of an underlying function in \code{\link{ampute}}.
#' @param prop A scalar specifying the proportion of missingness. Should be a value
#' between 0 and 1. Default is a missingness proportion of 0.5.
#' @param type A vector of strings containing the type of missingness for each
#' pattern. Either \code{"LEFT"}, \code{"MID"}, \code{"TAIL"} or '\code{"RIGHT"}.
#' If a single missingness type is entered, all patterns will be created by the same
#' type. If missingness types should differ over patterns, a vector of missingness
#' types should be entered. Default is RIGHT for all patterns and is the result of
#' \code{\link{ampute.default.type}}.
#' @return A list containing vectors with \code{0} if a case should be made missing
#' and \code{1} if a case should remain complete. The first vector refers to the
#' first pattern, the second vector to the second pattern, etcetera.
#' @author Rianne Schouten [aut, cre], Gerko Vink [aut], Peter Lugtig [ctb], 2016
#' @seealso \code{\link{ampute}}, \code{\link{ampute.default.type}}
#' @references
#' #'Van Buuren, S. (2018).
#' \href{https://stefvanbuuren.name/fimd/sec-linearnormal.html#sec:generateuni}{\emph{Flexible Imputation of Missing Data. Second Edition.}}
#' Chapman & Hall/CRC. Boca Raton, FL.
#' @keywords internal
#' @export
ampute.continuous <- function(P, scores, prop, type) {
  # For a test data set, the shift of the logit function is calculated
  # in order to obtain the right proportion of missingness (area beneath the curve)
  # The set-up for this is created in subsequent lines, it is executed within
  # the for loop over i.
  testset <- scale(rnorm(n = 10000, mean = 0, sd = 1))
  logit <- function(x) exp(x) / (1 + exp(x))
  # An empty list is created, type argument is given the right length
  R <- vector(mode = "list", length = length(scores))
  if (length(type) == 1) {
    type <- rep.int(type, length(scores))
  }
  for (i in seq_along(scores)) {
    # The desired function is chosen
    formula <- switch(type[i],
      LEFT = function(x, b) logit(mean(x) - x + b),
      MID = function(x, b) logit(-abs(x - mean(x)) + 0.75 + b),
      TAIL = function(x, b) logit(abs(x - mean(x)) - 0.75 + b),
      function(x, b) logit(-mean(x) + x + b)
    )
    shift <- bin.search(
      fun = function(shift) {
        sum(formula(x = testset, b = shift)) / length(testset)
      },
      target = prop
    )$where
    if (length(shift) > 1) {
      shift <- shift[1]
    }
    scores.temp <- scores[[i]]
    # empty candidate group
    if (length(scores.temp) == 1 && scores.temp == 0) {
      R[[i]] <- 0 
    } else {
      if (length(scores.temp) == 1) {
        warning(paste("There is only 1 candidate for pattern", i, ",it will be amputed with probability", prop), call. = FALSE)
        probs <- prop
      } else if (length(unique(scores.temp)) == 1) {
        warning(paste("The weighted sum scores of all candidates in pattern", i, "are the same, they will be amputed with probability", prop), call. = FALSE)
        probs <- prop
      } else {
        probs <- formula(x = scores.temp, b = shift)
      }

      # Based on the probabilities, each candidate will receive a missing data
      # indicator 0, meaning it will be made missing or missing data indicator 1,
      # meaning the candidate will remain complete.

      R.temp <- 1 - rbinom(n = length(scores.temp), size = 1, prob = probs)
      R[[i]] <- replace(P, P == (i + 1), R.temp)
      R[[i]] <- replace(R[[i]], P != (i + 1), 1)
    }
  }
  R
}

# This is a custom adaptation of function binsearch from package gtools
# (version 3.5.0) that returns the adjustment of the probability curves used
# in the function ampute.continuous in ampute.
bin.search <- function(fun, range = c(-8, 8), ..., target = 0,
                       lower = ceiling(min(range)),
                       upper = floor(max(range)),
                       maxiter = 100, showiter = FALSE) {
  lo <- lower
  hi <- upper
  counter <- 0
  val.lo <- round(fun(lo, ...), 3)
  val.hi <- round(fun(hi, ...), 3)
  sign <- if (val.lo > val.hi) -1 else 1
  if (target * sign < val.lo * sign) {
    outside.range <- TRUE
  } else if (target * sign > val.hi * sign) {
    outside.range <- TRUE
  } else {
    outside.range <- FALSE
  }
  while (counter < maxiter && !outside.range) {
    counter <- counter + 1
    if (hi - lo <= (1 / (10^3)) || lo < lower || hi > upper) {
      break
    }
    center <- round((hi - lo) / 2 + lo, 3)
    val <- round(fun(center, ...), 3)
    if (showiter) {
      cat("--------------\n")
      cat("Iteration #", counter, "\n")
      cat("lo=", lo, "\n")
      cat("hi=", hi, "\n")
      cat("center=", center, "\n")
      cat("fun(lo)=", val.lo, "\n")
      cat("fun(hi)=", val.hi, "\n")
      cat("fun(center)=", val, "\n")
    }
    if (val == target) {
      val.lo <- val.hi <- val
      lo <- hi <- center
      break
    }
    else if (sign * val < sign * target) {
      lo <- center
      val.lo <- val
    }
    else {
      hi <- center
      val.hi <- val
    }
    if (showiter) {
      cat("new lo=", lo, "\n")
      cat("new hi=", hi, "\n")
      cat("--------------\n")
    }
  }
  retval <- list(call = match.call(), numiter = counter)
  if (outside.range) {
    if (target * sign < val.lo * sign) {
      warning("The desired proportion of ", target, " is too small; ", val.lo, " is used instead.")
      retval$flag <- "Lower Boundary"
      retval$where <- lo
      retval$value <- val.lo
    }
    else {
      warning("The desired proportion of ", target, " is too large; ", val.hi, " is used instead.")
      retval$flag <- "Upper Boundary"
      retval$where <- hi
      retval$value <- val.hi
    }
  }
  else if (counter >= maxiter) {
    retval$flag <- "Maximum number of iterations reached"
    retval$where <- (lo + hi) / 2
    retval$value <- (val.lo + val.hi) / 2
  }
  else if (val.lo == target) {
    retval$flag <- "Found"
    retval$where <- lo
    retval$value <- val.lo
  }
  else if (val.hi == target) {
    retval$flag <- "Found"
    retval$where <- hi
    retval$value <- val.hi
  }
  else {
    retval$flag <- "Between Elements"
    retval$where <- (lo + hi) / 2
    retval$value <- (val.lo + val.hi) / 2
  }
  retval
}
