#'Multivariate Amputation Based On Discrete Probability Functions
#'
#'This function creates a missing data indicator for each pattern. Odds probabilities 
#'(Brand, 1999, pp. 110-113) will be induced on the weighted sum scores, calculated earlier 
#'in the multivariate amputation function \code{\link{ampute}}.
#'
#'@param P A vector containing the pattern numbers of the cases's candidacies. 
#'For each case, a value between 1 and #patterns is given. For example, a 
#'case with value 2 is candidate for missing data pattern 2. 
#'@param scores A list containing vectors with the candidates's weighted sum scores, 
#'the result of an underlying function in \code{\link{ampute}}.
#'@param prop A scalar specifying the proportion of missingness. Should be a value 
#'between 0 and 1. Default is a missingness proportion of 0.5.
#'@param odds A matrix where #patterns defines the #rows. Each row should contain 
#'the odds of being missing for the corresponding pattern. The amount of odds values 
#'defines in how many quantiles the sum scores will be divided. The values are 
#'relative probabilities: a quantile with odds value 4 will have a probability of 
#'being missing that is four times higher than a quantile with odds 1. The 
#'#quantiles may differ between the patterns, specify NA for cells remaining empty. 
#'Default is 4 quantiles with odds values 1, 2, 3 and 4, the result of 
#'\code{\link{ampute.default.odds}}.
#'@return A list containing vectors with \code{0} if a case should be made missing 
#'and \code{1} if a case should remain complete. The first vector refers to the 
#'first pattern, the second vector to the second pattern, etcetera.  
#'@author Rianne Schouten, 2016
#'@seealso \code{\link{ampute}}, \code{\link{ampute.default.odds}}
#'@references Brand, J.P.L. (1999). \emph{Development, implementation and 
#'evaluation of multiple imputation strategies for the statistical analysis of 
#'incomplete data sets.} Dissertation. Rotterdam: Erasmus University.
#'@keywords internal 
#'@export
ampute.discrete <- function(P, scores, prop, odds) {
  # Multivariate Amputation Based On Discrete Probability Functions
  #
  # This function creates a missing data indicator for each pattern. Odds probabilities 
  # (Brand, 1999, pp. 110-113) will be induced on the weighted sum scores calculated 
  # earlier in the multivariate amputation function ampute().
  #
  R <- vector(mode = "list", length = nrow(odds))
  for (i in seq_len(nrow(odds))) {
    if (scores[[i]][[1]] == 0) {
      R[[i]] <- 0
    } else {
      # The scores are divided into quantiles
      # Specify #quantiles by #odds values
      ng <- length(odds[i, ][!is.na(odds[i, ])])  
      quantiles <- quantile(scores[[i]], probs = seq.int(0, 1, by = 1 / ng))
      if (anyDuplicated(quantiles) || anyNA(quantiles)) {
        stop("Division of sum scores into quantiles did not succeed. Possibly
             the sum scores contain too few different observations (in case of
             categorical or dummy variables). Try using more variables to 
             calculate the sum scores or diminish the number of quantiles in the
             odds matrix", call. = FALSE)
      }
      # For each candidate the quantile number is specified 
      R.temp <- rep.int(NA, length(scores[[i]]))
      for (k in seq_len(ng)) {
        R.temp <- replace(R.temp, scores[[i]] >= quantiles[k] 
                          & scores[[i]] <= quantiles[k + 1], k)
      }
      # For each candidate, a random value between 0 and 1 is compared with the 
      # odds probability of being missing. If random value <= prob, the candidate 
      # will receive missing data indicator 0, meaning he will be made missing 
      # according the pattern; if random value > prob, the candidate will receive
      # missing data indicator 1, meaning the candidate will remain complete. 
      for (l in seq_len(ng)) {
        prob <- (ng * prop * odds[i, l]) / sum(odds[i, ], na.rm = TRUE)
        if (prob >= 1.0) {
          warning("Combination of odds matrix and desired proportion of 
                  missingness results to small quantile groups, probably 
                  decreasing the obtained proportion of missingness",
                  call. = FALSE)
        }
        gn <- length(R.temp[R.temp == l])
        if (gn != 0) {
          random <- runif(n = gn, min = 0, max = 1)
          Q <- c()
          for (m in seq_len(gn)) {
            if (random[m] <= prob) {
              Q[m] <- 0  # Candidate will be made missing
            } else {
              Q[m] <- 1  # Candidate will be kept complete
            }
          }
          # Give the result to the right candidate
          R.temp <- replace(R.temp, R.temp == l, Q) 
        }
      }
      # Give the result to the right cases in the data
      R[[i]] <- replace(P, P == (i + 1), R.temp)
      R[[i]] <- replace(R[[i]], P != (i + 1), 1)
    }
  }
  return(R)
}