#'MAR Amputation based on a Continuous probability function
#'
#'This function creates a missing data indicator for each pattern. MAR missingness 
#'is induced by using the probability distributions as described by Van Buuren, 
#'2012. The function is used in the multivariate amputation function 
#'\code{\link{ampute}}.
#'
#'@param P A vector containing the values of the patterns the cases are candidate
#'for. For each case, a value between 1 and #patterns 1 is given. For example, a 
#'case with value 2 is candidate for missing data pattern 2. 
#'@param scores A list containing vectors with weighted sum scores of the 
#'candidates, the result of an underlying function in \code{\link{ampute}}.
#'@param prop A scalar specifying the proportion of missingness. Should be value 
#'between 0 and 1 with 3 decimal places at most. Default is 0.5.
#'@param type A vector of strings containing the type of MAR missingness for each 
#'pattern. Either \code{"MARLEFT"}, \code{"MARMID"}, \code{"MARTAIL"} or 
#'\code{"MARRIGHT"}. If a single missingness type is entered, all patterns will 
#'be created by the same type. If missingness types should differ over patterns, 
#'a vector of missingness types should be entered. Default is MARRIGHT for all 
#'patterns.   
#'@return A list containing vectors with \code{0} if a case should be made missing 
#'and \code{1} if a case should be kept complete. The first vector refers to the 
#'first pattern, the second vector to the second pattern, etc. 
#'@author Rianne Schouten, 2016
#'@seealso \code{\link{ampute}}
#'@references Van Buuren, S. (2012). \emph{Flexible imputation of missing data} 
#'(pp. 63-64). Boca Raton, FL.: Chapman & Hall/CRC Press.
#'@export
ampute.mar.cont <- function(P, scores, prop, type) {
  # MAR Amputation based on a Continuous probability function
  #
  # This function creates a missing data indicator for each pattern. MAR missingness 
  # is induced by using the probability distributions as described by Van Buuren, 
  # 2012. The function is used in the multivariate amputation function ampute().
  #
  # ------------------------ bin.search --------------------------------------
  #
  bin.search <- function (fun, range = c(-8, 8), ..., target = 0, 
                          lower = ceiling(min(range)), 
                          upper = floor(max(range)), 
                          maxiter = 100, showiter = FALSE) {
    # This is a custom adaptation of function binsearch from package gtools 
    # (version 3.5.0) that returns the adjustment of the probability curves used 
    # in the function ampute.mar.cont in ampute, up to 3 decimal places. 
    nd <- min(which(target * 10 ^ (0:20) == floor(target * 10 ^ (0:20)))) - 1
    lo <- lower
    hi <- upper
    counter <- 0
    val.lo <- round(fun(lo, ...), nd)
    val.hi <- round(fun(hi, ...), nd)
    if (val.lo > val.hi) 
      sign <- -1
    else sign <- 1
    if (target * sign < val.lo * sign) 
      outside.range <- TRUE
    else if (target * sign > val.hi * sign) 
      outside.range <- TRUE
    else outside.range <- FALSE
    while (counter < maxiter && !outside.range) {
      counter <- counter + 1
      if (hi - lo <= (1 / (10 ^ nd)) || lo < lower || hi > upper) 
        break
      center <- round((hi - lo)/2 + lo, nd)
      val <- round(fun(center, ...), nd)
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
    retval <- list()
    retval$call <- match.call()
    retval$numiter <- counter
    if (outside.range) {
      if (target * sign < val.lo * sign) {
        warning("Reached lower boundary")
        retval$flag = "Lower Boundary"
        retval$where = lo
        retval$value = val.lo
      }
      else {
        warning("Reached upper boundary")
        retval$flag = "Upper Boundary"
        retval$where = hi
        retval$value = val.hi
      }
    }
    else if (counter >= maxiter) {
      warning("Maximum number of iterations reached")
      retval$flag = "Maximum number of iterations reached"
      retval$where = c(lo, hi)
      retval$value = c(val.lo, val.hi)
    }
    else if (val.lo == target) {
      retval$flag = "Found"
      retval$where = lo
      retval$value = val.lo
    }
    else if (val.hi == target) {
      retval$flag = "Found"
      retval$where = hi
      retval$value = val.hi
    }
    else {
      retval$flag = "Between Elements"
      retval$where = c(lo, hi)
      retval$value = c(val.lo, val.hi)
    }
    return(retval)
  }
  
  # ----------------------- ampute.mar.cont ----------------------------------
  
  testset <- scale(rnorm(n = 10000, mean = 0, sd = 1))
  logit <- function(x) exp(x) / (1 + exp(x))
  shift <- bin.search(fun = function(shift) 
    sum(logit(-mean(testset) + testset[] + shift)) / length(testset), 
    target = prop)$where
  R <- list()
  if (length(type) == 1) {
    type <- rep(type[1], length(scores))
  }
  for (i in 1:length(scores)) {
    scores.temp <- scale(scores[[i]])
    if (type[i] == "MARLEFT") { 
      formula <- function(x, b) logit(mean(x) - x[] + b)
    } else if (type[i] == "MARMID") { 
      formula <- function(x, b) logit(-abs(x[] - mean(x)) + 0.75 + b)
    } else if (type[i] == "MARTAIL") { 
      formula <- function(x, b) logit(abs(x[] - mean(x)) -0.75 + b)
    } else {
      formula <- function(x, b) logit(-mean(x) + x[] + b)
    }
    probs <- formula(x = scores.temp, b = shift)
    R.temp <- 1 - rbinom(n = length(scores.temp), size = 1, prob = probs)
    R[[i]] <- replace(P, P == (i + 1), R.temp)
    R[[i]] <- replace(R[[i]], P != (i + 1), 1)
  }
  return(R)
}
