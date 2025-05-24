#' Default \code{patterns} in \code{ampute}
#'
#' This function creates a default pattern matrix for the multivariate
#' amputation function \code{ampute()}.
#'
#' @param n A scalar specifying the number of variables in the data.
#' @return A square matrix of size \code{n} where \code{0} indicates a variable
#  should have missing values and \code{1} indicates a variable should remain
#  complete. Each pattern has missingness on one variable only.
#' @seealso \code{\link{ampute}}, \code{\link{md.pattern}}
#' @author Rianne Schouten, 2016
#' @keywords internal
ampute.default.patterns <- function(n) {
  patterns.list <- lapply(
    seq_len(n),
    function(i) c(rep.int(1, i - 1), 0, rep.int(1, n - i))
  )
  do.call(rbind, patterns.list)
}

#' Default \code{freq} in \code{ampute}
#'
#' Defines the default relative frequency vector for the multivariate
#' amputation function \code{ampute}.
#'
#' @param patterns A matrix of size #patterns by #variables where \code{0} indicates
#' a variable should have missing values and \code{1} indicates a variable should
#' remain complete. Could be the result of \code{\link{ampute.default.patterns}}.
#' @return A vector of length #patterns containing the relative frequencies with
#' which the patterns should occur. An equal probability is given to each pattern.
#' @seealso \code{\link{ampute}}, \code{\link{ampute.default.patterns}}
#' @author Rianne Schouten, 2016
#' @keywords internal
#' @export
ampute.default.freq <- function(patterns) {
  rep.int(1 / nrow(patterns), nrow(patterns))
}

#' Default \code{weights} in \code{ampute}
#'
#' Defines the default weights matrix for the multivariate amputation function
#' \code{ampute}.
#'
#' @param patterns A matrix of size #patterns by #variables where \code{0} indicates
#' a variable should have missing values and \code{1} indicates a variable should
#' remain complete. Could be the result of \code{\link{ampute.default.patterns}}.
#' @param mech A string specifying the missingness mechanism.
#' @return A matrix of size #patterns by #variables containing the weights that
#' will be used to calculate the weighted sum scores. Equal weights are given to
#' all variables. When mechanism is MAR, variables that will be amputed will be
#' weighted with \code{0}. If it is MNAR, variables that will be observed
#' will be weighted with \code{0}. If mechanism is MCAR, the weights matrix will
#' not be used. A default MAR matrix will be returned.
#' @seealso \code{\link{ampute}}, \code{\link{ampute.default.patterns}}
#' @author Rianne Schouten, 2016
#' @keywords internal
#' @export
ampute.default.weights <- function(patterns, mech) {
  weights <- matrix(data = 1, nrow = nrow(patterns), ncol = ncol(patterns))
  if (mech != "MNAR") {
    weights <- matrix(data = 1, nrow = nrow(patterns), ncol = ncol(patterns))
    weights[patterns == 0] <- 0
  } else {
    weights <- matrix(data = 0, nrow = nrow(patterns), ncol = ncol(patterns))
    weights[patterns == 0] <- 1
  }
  weights
}

#' Default \code{type} in \code{ampute()}
#'
#' Defines the default type vector for the multivariate amputation function
#' \code{ampute}.
#'
#' @param patterns A matrix of size #patterns by #variables where 0 indicates a
#' variable should have missing values and 1 indicates a variable should remain
#' complete. Could be the result of \code{\link{ampute.default.patterns}}.
#' @return A string vector of length #patterns containing the missingness types.
#' Each pattern will be amputed with a "RIGHT" missingness.
#' @seealso \code{\link{ampute}}, \code{\link{ampute.default.patterns}}
#' @author Rianne Schouten, 2016
#' @keywords internal
#' @export
ampute.default.type <- function(patterns) {
  rep.int("RIGHT", nrow(patterns))
}

#' Default \code{odds} in \code{ampute()}
#'
#' Defines the default odds matrix for the multivariate amputation function
#' \code{ampute}.
#'
#' @param patterns A matrix of size #patterns by #variables where 0 indicates a
#' variable should have missing values and 1 indicates a variable should remain
#' complete. Could be the result of \code{\link{ampute.default.patterns}}.
#' @return A matrix where #rows equals #patterns. Default is 4 quantiles with odds
#' values 1, 2, 3 and 4, for each pattern, imitating a RIGHT type of missingness.
#' @seealso \code{\link{ampute}}, \code{\link{ampute.default.patterns}}
#' @author Rianne Schouten, 2016
#' @keywords internal
#' @export
ampute.default.odds <- function(patterns) {
  matrix(seq_len(4), nrow = nrow(patterns), ncol = 4, byrow = TRUE)
}
