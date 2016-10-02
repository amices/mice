#'MCAR Amputation
#'
#'This function creates a missing data indicator for each pattern, based on MCAR 
#'missingness. The function is used in the multivariate amputation function 
#'\code{\link{ampute}}.
#'
#'@param P A vector containing the values of the patterns the cases are candidate
#'for. For each case, a value between 1 and #patterns 1 is given. For example, a 
#'case with value 2 is candidate for missing data pattern 2. 
#'@param patterns A matrix of size #patterns by #variables where 0 indicates a variable
#'should have missing values and 1 indicates a variable should remain complete. 
#'The user may specify as many patterns as desired. One pattern (a vector) is 
#'also possible. Default is a square matrix of size #variables where each pattern 
#'has missingness on one variable only. 
#'@param prop A scalar specifying the proportion of missingness. Should be value 
#'between 0 and 1 with 3 decimal places at most. Default is 0.5.
#'@return A list containing vectors with \code{0} if a case should be made missing 
#'and \code{1} if a case should be kept complete. The first vector refers to the 
#'first pattern, the second vector to the second pattern, etc. 
#'@author Rianne Schouten, 2016 
#'@seealso \code{\link{ampute}} 
#'@export
ampute.mcar <- function(i, P, prop) {
  # MCAR Amputation
  #
  # This function creates a missing data indicator for each pattern, based on 
  # MCAR missingness. The function is used in the multivariate amputation 
  # function ampute().
  nf <- length(P[P == (i + 1)])
  R.temp <- 1 - rbinom(n = nf, size = 1, prob = prop)
  R <- replace(P, P == (i + 1), R.temp)
  R <- replace(R, P != (i + 1), 1)
  return(R)
}