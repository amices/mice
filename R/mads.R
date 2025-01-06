#' Multivariate amputed data set (\code{mads})
#'
#' The \code{mads} object is an S3 class that contains an amputed dataset,
#' i.e., a dataset with simulated missing values. The  \code{\link{ampute}()}
#' function produces a \code{mads} object.
#' The \code{mads()} function is the S3 constructor.
#' The S3 class \code{mads} has the following methods:
#' \code{\link{bwplot.mads}()},\code{print()}, \code{summary()},
#' and \code{\link{xyplot.mads}()}.
#'
#' @param data A data frame representing the original data.
#' @param call The function call that created the object.
#' @param prop A numeric vector of proportions.
#' @param patterns A matrix of missing data patterns.
#' @param freq A numeric vector of frequencies for each pattern.
#' @param mech A character string describing the missing data mechanism.
#' @param weights A matrix of weights.
#' @param cont A logical vector indicating continuous variables.
#' @param type A character vector describing variable types.
#' @param odds A matrix of odds ratios.
#' @param amp A data frame for amplitude information.
#' @param cand An integer vector of candidate variables.
#' @param scores A list of scores.
#'
#' @return An object of class \code{"mads"}.
#'
#' @section Contents:
#' \describe{
#' \item{\code{call}:}{The function call.}
#' \item{\code{prop}:}{Proportion of cases with missing values. Note: even when
#' the proportion is entered as the proportion of missing cells (when
#' \code{bycases == TRUE}), this object contains the proportion of missing cases.}
#' \item{\code{patterns}:}{A data frame of size #patterns by #variables where \code{0}
#' indicates a variable has missing values and \code{1} indicates a variable remains
#' complete.}
#' \item{\code{freq}:}{A vector of length #patterns containing the relative
#' frequency with which the patterns occur. For example, if the vector is
#' \code{c(0.4, 0.4, 0.2)}, this means that of all cases with missing values,
#' 40 percent is candidate for pattern 1, 40 percent for pattern 2 and 20
#' percent for pattern 3. The vector sums to 1.}
#' \item{\code{mech}:}{A string specifying the missingness mechanism, either
#' \code{"MCAR"}, \code{"MAR"} or \code{"MNAR"}.}
#' \item{\code{weights}:}{A data frame of size #patterns by #variables. It contains
#' the weights that were used to calculate the weighted sum scores. The weights
#' may differ between patterns and between variables.}
#' \item{\code{cont}:}{Logical, whether probabilities are based on continuous logit
#' functions or on discrete odds distributions.}
#' \item{\code{type}:}{A vector of strings containing the type of missingness
#' for each pattern. Either \code{"LEFT"}, \code{"MID"}, \code{"TAIL"} or
#' \code{"RIGHT"}. The first type refers to the first pattern, the second type
#' to the second pattern, etc.}
#' \item{\code{odds}:}{A matrix where #patterns defines the #rows. Each row contains
#' the odds of being missing for the corresponding pattern. The amount of odds values
#' defines in how many quantiles the sum scores were divided. The values are
#' relative probabilities: a quantile with odds value 4 will have a probability of
#' being missing that is four times higher than a quantile with odds 1. The
#' #quantiles may differ between patterns, NA is used for cells remaining empty.}
#' \item{\code{amp}:}{A data frame containing the input data with NAs for the
#' amputed values.}
#' \item{\code{cand}:}{A vector that contains the pattern number for each case.
#' A value between 1 and #patterns is given. For example, a case with value 2 is
#' candidate for missing data pattern 2.}
#' \item{\code{scores}:}{A list containing vectors with weighted sum scores of the
#' candidates. The first vector refers to the candidates of the first pattern, the
#' second vector refers to the candidates of the second pattern, etc. The length
#' of the vectors differ because the number of candidates is different for each
#' pattern.}
#' \item{\code{data}:}{The complete data set that was entered in \code{ampute}.}
#' }
#' @name mads
#' @aliases mads
#' @author Rianne Schouten, 2016
#' @seealso \code{\link{ampute}}, Vignette titled "Multivariate Amputation using
#' Ampute".
#' @keywords classes
#' @export
mads <- function(
    call, prop, patterns, freq, mech, weights, cont, type,
    odds, amp, cand, scores, data) {
  # Validate inputs
  # if (!is.call(call)) stop("Argument 'call' must be a call.")
  # if (!is.numeric(prop)) stop("Argument 'prop' must be numeric.")
  # if (!is.matrix(patterns)) stop("Argument 'patterns' must be a matrix.")
  # if (!is.numeric(freq)) stop("Argument 'freq' must be numeric.")
  # if (!is.character(mech)) stop("Argument 'mech' must be character.")
  # if (!is.matrix(weights)) stop("Argument 'weights' must be a matrix.")
  # if (!is.logical(cont)) stop("Argument 'cont' must be logical.")
  # if (!is.character(type)) stop("Argument 'type' must be character.")
  # if (!is.matrix(odds)) stop("Argument 'odds' must be a matrix.")
  # if (!is.data.frame(amp)) stop("Argument 'amp' must be a data frame.")
  # if (!is.integer(cand)) stop("Argument 'cand' must be integer.")
  # if (!is.list(scores)) stop("Argument 'scores' must be a list.")
  # if (!is.data.frame(data)) stop("Argument 'data' must be a data frame.")

  # Create the object
  obj <- list(
    call = call,
    prop = prop,
    patterns = patterns,
    freq = freq,
    mech = mech,
    weights = weights,
    cont = cont,
    type = type,
    odds = odds,
    amp = amp,
    cand = cand,
    scores = scores,
    data = data
  )

  # Assign the class
  class(obj) <- "mads"
  return(obj)
}

#' Print a \code{mads} object
#'
#' @rdname mads
#' @param x Object of class \code{mads}
#' @param \dots Other parameters
#' @return \code{print()} returns the input object invisibly.
#' @method print mads
#' @export
print.mads <- function(x, ...) {
  if (is.mads(x)) {
    cat("Multivariate Amputed Data Set")
    cat("\nCall: ")
    print(x$call)
    cat("Class:", class(x))
    cat("\nProportion of Missingness: ", x$prop)
    cat("\nFrequency of Patterns: ", x$freq)
    cat("\nPattern Matrix:\n")
    print(x$patterns)
    cat("Mechanism:")
    print(x$mech)
    cat("Weight Matrix:\n")
    print(x$weights)
    cat("Type Vector:\n")
    print(x$type)
    cat("Odds Matrix:\n")
    print(x$odds)
    cat("Head of Amputed Data Set\n")
    print(head(x$amp))
  } else {
    print(x, ...)
  }
  invisible(x)
}

#' @rdname mads
#' @param object Object of class \code{mads}
#' @return \code{summary()} returns the input object invisibly.
#' @method summary mads
#' @export
summary.mads <- function(object, ...) {
  print(object, ...)
  invisible(object)
}

