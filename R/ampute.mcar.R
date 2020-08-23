#' Multivariate amputation under a MCAR mechanism
#'
#' This function creates a missing data indicator for each pattern, based on a MCAR
#' missingness mechanism. The function is used in the multivariate amputation function
#' \code{\link{ampute}}.
#'
#' @param P A vector containing the pattern numbers of the cases' candidates.
#' For each case, a value between 1 and #patterns is given. For example, a
#' case with value 2 is candidate for missing data pattern 2.
#' @param patterns A matrix of size #patterns by #variables where \code{0} indicates
#' a variable should have missing values and \code{1} indicates a variable should
#' remain complete. The user may specify as many patterns as desired. One pattern
#' (a vector) is also possible. Could be the result of \code{\link{ampute.default.patterns}},
#' default will be a square matrix of size #variables where each pattern has missingness
#' on one variable only.
#' @param prop A scalar specifying the proportion of missingness. Should be a value
#' between 0 and 1. Default is a missingness proportion of 0.5.
#' @return A list containing vectors with \code{0} if a case should be made missing
#' and \code{1} if a case should remain complete. The first vector refers to the
#' first pattern, the second vector to the second pattern, etcetera.
#' @author Rianne Schouten, 2016
#' @seealso \code{\link{ampute}}
#' @keywords internal
#' @export
ampute.mcar <- function(P, patterns, prop) {
  f <- function(i) {
    # If there are no candidates in a certain pattern, the list will receive a 0
    if (length(P[P == (i + 1)]) == 0) {
      return(0)
    } else {
      # Otherwise, for all candidates in the pattern, the total proportion of
      # missingness is used to define the probabilities to be missing.
      nf <- length(P[P == (i + 1)])
      R.temp <- 1 - rbinom(n = nf, size = 1, prob = prop)
      # Based on the probabilities, each candidate will receive a missing data
      # indicator 0, meaning he will be made missing or missing data indicator 1,
      # meaning the candidate will remain complete.
      R.temp <- replace(P, P == (i + 1), R.temp)
      R.temp <- replace(R.temp, P != (i + 1), 1)
      return(R.temp)
    }
  }

  lapply(seq_len(nrow(patterns)), f)
}
