#'MAR Amputation based on a Discrete probability function
#'
#'This function creates a missing data indicator for each pattern. MAR missingness 
#'is induced by using odds probabilities as described by Brand, 1999. The function 
#'is used in the multivariate amputation function \code{\link{ampute}}.
#'
#'@param P A vector containing the values of the patterns the cases are candidate
#'for. For each case, a value between 1 and #patterns 1 is given. For example, a 
#'case with value 2 is candidate for missing data pattern 2. 
#'@param scores A list containing vectors with weighted sum scores of the 
#'candidates, the result of an underlying function in \code{\link{ampute}}.
#'@param prop A scalar specifying the proportion of missingness. Should be value 
#'between 0 and 1 with 3 decimal places at most. Default is 0.5.
#'@param odds A matrix where #patterns defines the #rows. Each row should contain 
#'the odds of being missing for the concurrent pattern. The amount of odds values 
#'defines in how many quantiles the sum scores will be divided. The values are 
#'relative probabilities: a quantile with odds value 4 will have a probability of 
#'being missing that is four times higher than a quantile with odds 1. The 
#'#quantiles may differ between patterns, specify NA for cells remaining empty. 
#'Default is 4 quantiles with odds values 1, 2, 3 and 4, for each pattern, 
#'imitating a MARRIGHT type of missingness.
#'@return A list containing vectors with \code{0} if a case should be made missing 
#'and \code{1} if a case should be kept complete. The first vector refers to the 
#'first pattern, the second vector to the second pattern, etc.
#'@author Rianne Schouten, 2016
#'@seealso \code{\link{ampute}}
#'@references Brand, J.P.L. (1999). \emph{Development, implementation and 
#'evaluation of multiple imputation strategies for the statistical analysis of 
#'incomplete data sets} (pp. 110-113). Dissertation. Rotterdam: Erasmus University. 
#'@export
ampute.mar.disc <- function(i, P, scores, prop, odds) {
  # MAR Amputation based on a Discrete probability function
  #
  # This function creates a missing data indicator for each pattern. MAR missingness 
  # is induced by using odds probabilities as described by Brand, 1999. The function 
  # is used in the multivariate amputation function ampute().
  R <- c()
  # The scores are divided into quantiles
  # Specify #quantiles by #odds values
  ng <- length(odds[!is.na(odds)])  
  quantiles <- quantile(scores, probs = seq(0, 1, by = 1 / ng))
  if (any(duplicated(quantiles)) | any(is.na(quantiles))) {
    stop("Division of sum scores into quantiles has not succeed. Possibly
         the sum scores contain too few different observations (in case of
         categorical or dummy variables). Try using more variables to 
         calculate the sum scores or diminish the number of quantiles in the
         odds matrix", call. = FALSE)
  }
  # For each candidate the quantile number is specified 
  R.temp <- rep(NA, length(scores))
  for (k in 1:ng) {
    R.temp <- replace(R.temp, scores >= quantiles[k] 
                      & scores <= quantiles[k + 1], k)
  }
  print(R.temp)
  # For each candidate, a random value between 0 and 1 is compared with the 
  # odds probability of being missing. If random value <= prob, the candidate 
  # is made missing according the pattern; if random value > prob, the 
  # candidate is kept complete 
  for (l in 1:ng) {
    prob <- (ng * prop * odds[l]) / sum(odds, na.rm = TRUE)
    if (prob >= 1.0) {
      warning("Combination of odds matrix and desired proportion of 
              missingness results to small quantile groups, probably 
              decreasing the obtained proportion of missingness",
              call. = FALSE)
    }
    gn <- length(R.temp[R.temp == l])
    random <- runif(n = gn, min = 0, max = 1)
    Q <- c()
    for (m in 1:gn) {
      if (random[m] <= prob) {
        Q[m] <- 0  # Candidate will be made missing
      } else {
        Q[m] <- 1  # Candidate will be kept complete
      }
    }
    print(Q)
    # Give the result to the right candidate
    R.temp <- replace(R.temp, R.temp == l, Q) 
    print(R.temp)
  }
  print(R.temp)
  # Give the result to the right cases in the data
  R <- replace(P, P == (i + 1), R.temp)
  R <- replace(R, P != (i + 1), 1)
  print(R)
  return(R)
}