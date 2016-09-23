#
# ------------------------- ampute.default.patterns --------------------------
#

#'Default \code{patterns} in \code{ampute}
#'
#'This function creates a default pattern matrix for the multivariate 
#'amputation function \code{ampute()}. 
#'
#'@param n A scalar specifying the #variables in the data.
#'@return A square matrix of size #variables where 0 indicates a variable 
#  should have missing values and 1 indicates a variable should remain 
#  complete. Each pattern has missingness on one variable only.
#'@seealso \code{\link{ampute}}
#'@author Rianne Schouten, 2016
#'@export
ampute.default.patterns <- function(n) {
  patterns <- matrix(data = NA, nrow = n, ncol = n)
  for (i in 1:n) {
    patterns[i, ] <- c(rep(1, i - 1), 0, rep(1, n - i))
  }
  return(patterns)
}

#
# ------------------------- ampute.default.freq ------------------------------
#

#'Default \code{freq} in \code{ampute}
#'
#'Defines the default relative frequencies vector for the multivariate 
#'amputation function \code{ampute}. 
#'
#'@param patterns A matrix of size #patterns by #variables where 0 indicates a 
#'variable should have missing values and 1 indicates a variable should remain 
#'complete.
#'@return A vector of length #patterns containing the relative frequency with 
#'which the patterns should occur. An equal probability is given to each pattern.
#'@seealso \code{\link{ampute}}
#'@author Rianne Schouten, 2016
#'@export
ampute.default.freq <- function(patterns) {
  freq <- rep((1 / nrow(patterns)), nrow(patterns))
  return(freq)
}

#
# --------------------------- ampute.default.mechanism -----------------------
#

#'
#'@export
ampute.default.mechanism <- function(len) {
  mechanism <- rep("MAR", len)
  return(mechanism)
}

#
# --------------------------- ampute.default.weights -------------------------
#

#'Default \code{weights} in \code{ampute}
#'
#'Defines the default weights matrix for the multivariate amputation function 
#'\code{ampute}. 
#'
#'@param patterns A matrix of size #patterns by #variables where 0 indicates a 
#'variable should have missing values and 1 indicates a variable should remain 
#'complete.
#'@return A matrix of size #patterns by #variables containing the weights that 
#'will be used to calculate the weighted sum scores. Equal weights are given to 
#'all variables. 
#'@seealso \code{\link{ampute}}
#'@author Rianne Schouten, 2016
#'@export
ampute.default.weights <- function(patterns, mechanism) {
  weights <- matrix(data = 1, 
                    nrow = length(mechanism[mechanism == "MAR"]), 
                    ncol = ncol(patterns))
  return(weights)
}

#
# -------------------------- ampute.default.continuous -----------------------
#

#'
#'
#'@export
ampute.default.continuous <- function(len) {
  continuous <- rep(TRUE, len)
  return(continuous)
}

#
# -------------------------- ampute.default.type -----------------------------
#

#'Default \code{type} in \code{ampute()}
#'
#'Defines the default type vector for the multivariate amputation function 
#'\code{ampute}. 
#'
#'@param patterns A matrix of size #patterns by #variables where 0 indicates a 
#'variable should have missing values and 1 indicates a variable should remain 
#'complete.
#'@return A string vector of length #patterns containing the missingness types.  
#'Each pattern will be amputed with "MARRIGHT" missingness. 
#'@seealso \code{\link{ampute}}
#'@author Rianne Schouten, 2016
#'@export
ampute.default.type <- function(len) {
  type <- rep("MARRIGHT", len)
  return(type)
}

#
# ---------------------------- ampute.default.odds ---------------------------
#

#'Default \code{odds} in \code{ampute()}
#'
#'Defines the default odds matrix for the multivariate amputation function 
#'\code{ampute}. 
#'
#'@param patterns A matrix of size #patterns by #variables where 0 indicates a 
#'variable should have missing values and 1 indicates a variable should remain 
#'complete.
#'@return A matrix where #rows equals #patterns. Default is 4 quantiles with odds 
#'values 1, 2, 3 and 4, for each pattern, imitating a MARRIGHT type of missingness.
#'@seealso \code{\link{ampute}}
#'@author Rianne Schouten, 2016
#'@export
ampute.default.odds <- function(len) {
  odds <- matrix(c(1, 2, 3, 4), 
                 nrow = len, ncol = 4, byrow = TRUE)
  return(odds)
}

