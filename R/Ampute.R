#'Generate Missing Data for Simulation Purposes
#' 
#'This function generates multivariate missing data in a MCAR or MAR manner. 
#'Imputation of data sets containing missing values can be performed with 
#'\code{\link{mice}}. 
#'
#'When new multiple imputation techniques are tested, values in simulated 
#'data sets have to be made missing. For these simulation studies, \code{ampute}
#'is developed. Imputation can be performed with \code{\link{mice}}. 
#'
#'Until recently, univariate amputation procedures were used to generate missing
#'data in complete, simulated data sets. This univeriate approach was repeated in 
#'case several variables should need to be made incomplete. However, with a univeriate
#'approach, it is difficult to relate missingness on one variable to the missingness 
#'on another variable. A multivariate amputation procedure solves this issue and 
#'moreover, it does justice to the multivariate nature of data sets. Hence, 
#'\code{ampute} is developed to perform the amputation according the researcher's 
#'desires. 
#'
#'The idea behind the function is the specification of several missingness 
#'patterns. Each pattern is a combination of variables with and without missing 
#'values (denoted by \code{0} and \code{1} respectively). For example, one might
#'want to create two missingness patterns on a data set with four variables. The
#'patterns would be something like: \code{0, 0, 1, 1} and \code{1, 0, 1, 0}. 
#'Each combination of zeros and ones may occur, except a pattern of merely ones 
#'or zeros. 
#'
#'Furthermore, the researcher specifies the proportion of missingness, either the 
#'proportion of missing cases or the proportion of missing cells, and the relative 
#'frequency each pattern occurs. Consequently, the data is divided over the 
#'patterns with these probabilities. Now, each case is candidate for a certain 
#'missingness pattern. Whether the case will have missing values eventually, 
#'depends on other specifications. 
#'
#'When the user sets the missingness mechanism to \code{"MCAR"}, the candidates 
#'have an equal probability of having missing values. No other specifications 
#'have to be made. For a \code{"MAR"} mechanism, weighted sum scores are calculated. 
#'First, the data is standardized. Second, for each case, the observed values are 
#'multiplied with weights, specified by argument \code{weights}. These weighted
#'scores will be summed, resulting in a weighted sum score for each case. 
#'
#'The weights may differ between patterns and they may be negative or zero as well. 
#'Naturally, it is the relative difference between the weights that will result
#'in a \code{MAR} effect in the sum scores. For example, for the first missing data
#'pattern mentioned above, the weights for the third and fourth variables might
#'be set to 2 and 4. However, weight values of 0.2 and 0.4 will have the exact
#'same effect on the weighted sum score: the fourth variable is weighted twice as
#'much as variable 3. In this specific pattern, variable 1 and 2 will be set missing. 
#'Consequently, their weights are not of importance. 
#'
#'Based on the weighted sum scores, either a discrete or continuous distribution 
#'of probabilities is used to calculate whether a candidate will have missing values. 
#'
#'For a discrete distribution of probabilities, the weighted sum scores are 
#'divided into subgroups of equal size (quantiles). Thereafter, the user 
#'specifies for each subgroup the odds of being missing. Both the number of 
#'subgroups and the odds values guide the incomplete data mechanism for a certain 
#'pattern. For example, for a MARRIGHT-like mechanism, scoring in one of the 
#'higher quantiles should have high missingness odds, whereas for a MARMID-like 
#'mechanism, the central groups should have higher odds. Again, not the size of 
#'the odds values are of importance, but the relative distance between the values. 
#'
#'The continuous distribution of probabilities is based on the logit function, as 
#'described by Van Buuren (2012). The user can specify the type of missingness, 
#'which, again, may differ between patterns. 
#'
#'An extensive example is shown in a vignette 
#'titled "Multivariate Amputation using Ampute". 
#'
#'@param data A complete data matrix or dataframe. Values should be numeric. 
#'Categorical variables should have been transformed into dummies.
#'@param prop A scalar specifying the proportion of missingness. Should be value 
#'between 0 and 1 with 3 decimal places at most. Default is 0.5.
#'@param patterns A matrix of size #patterns by #variables where 0 indicates a variable
#'should have missing values and 1 indicates a variable should remain complete. 
#'The user may specify as many patterns as desired. One pattern (a vector) is 
#'also possible. Default is a square matrix of size #variables where each pattern 
#'has missingness on one variable only. \code{\link{md.pattern}} can be used to 
#'have an overview of the missing data patterns that are created.  
#'@param freq A vector of length #patterns containing the relative frequency with 
#'which the patterns should occur. For example, for three missing data patterns, 
#'the vector could be \code{c(0.4, 0.4, 0.2)}, meaning that of all cases with 
#'missing values, 40 percent should have pattern 1, 40 percent pattern 2 and 20 
#'percent pattern 3. The vector should sum to 1. Default is an equal probability 
#'for each pattern.   
#'@param mechanism A string specifying the missingness mechanism, either MCAR or 
#'MAR. Default is MAR.   
#'@param weights A matrix of size #patterns by #variables, only useful when 
#'\code{mechanism = "MAR"}. The matrix contains the weights that will be used
#'to calculate the weighted sum scores. The weights may differ between patterns 
#'and between variables. Within each pattern, the relative size of the values are 
#'of importance. Default is equal weights for all patterns and all variables.  
#'@param continuous Logical. If TRUE, the probabilities of being missing are based 
#'on a continuous logit distribution defined by argument \code{type}. If FALSE, the 
#'probabilities of being missing are based on a discrete distribution defined by 
#'the \code{odds} argument. Default is TRUE.   
#'@param type A vector of strings containing the type of MAR missingness for each 
#'pattern. Either \code{"MARLEFT"}, \code{"MARMID"}, \code{"MARTAIL"} or 
#'\code{"MARRIGHT"}. If a single missingness type is entered, all patterns will 
#'be created by the same type. If missingness types should differ over patterns, 
#'a vector of missingness types should be entered. Default is \code{"MARRIGHT"} for all 
#'patterns.   
#'@param odds A matrix where patterns defines the rows. Each row should contain 
#'the odds of being missing for the concurrent pattern. The amount of odds values 
#'defines in how many quantiles the sum scores will be divided. The values are 
#'relative probabilities: a quantile with odds value 4 will have a probability of 
#'being missing that is four times higher than a quantile with odds 1. The 
#'quantiles may differ between patterns, specify NA for cells remaining empty. 
#'Default is 4 quantiles with odds values 1, 2, 3 and 4, for each pattern, 
#'imitating a MARRIGHT type of missingness.
#'@param bycases Logical. If TRUE, the proportion of missingness is defined in 
#'terms of cases. If FALSE, the proportion of missingness is defined in terms of 
#'cells. Default is TRUE.
#'@param run Logical. If TRUE, the amputations are implemented. If FALSE, the 
#'return object will contain everything but the amputed data set. 
#'
#'@return Returns a S3 object of class \code{\link{mads-class}} (multivariately amputed 
#'data set)
#'@author Rianne Schouten, Gerko Vink, Peter Lugtig, 2016
#'@seealso \code{\link{mads-class}} 
#'@references Brand, J.P.L. (1999). \emph{Development, implementation and 
#'evaluation of multiple imputation strategies for the statistical analysis of 
#'incomplete data sets} (pp. 110-113). Dissertation. Rotterdam: Erasmus University. 
#'
#'Van Buuren, S., Brand, J.P.L., Groothuis-Oudshoorn, C.G.M., Rubin, D.B. (2006). 
#'Fully conditional specification in multivariate imputation. \emph{Journal of 
#'Statistical Computation and Simulation}, 76\emph{(12)}, Appendix B.
#'
#'Van Buuren, S. (2012). \emph{Flexible imputation of missing data} (pp. 63-64). 
#'Boca Raton, FL.: Chapman & Hall/CRC Press.
#'
#'Vink, G. (2016). Towards a standardized evaluation of multiple imputation 
#'routines. 
#'
#'@examples 
#'# Simulate data set with \code{mvrnorm} from package \code{\pkg{MASS}}.
#'require(MASS)
#'sigma <- matrix(data = c(1, 0.2, 0.2, 0.2, 1, 0.2, 0.2, 0.2, 1), nrow = 3)
#'complete.data <- mvrnorm(n = 100, mu = c(5, 5, 5), Sigma = sigma)
#'# Perform quick amputation
#'result1 <- ampute(data = complete.data)
#'# Change default matrices as desired
#'patterns <- result1$patterns
#'patterns[1:3, 2] <- 0
#'odds <- result1$odds
#'odds[2,3:4] <- c(2, 4)
#'odds[3,] <- c(3, 1, NA, NA)
#'# Rerun amputation
#'result3 <- ampute(data = complete.data, patterns = patterns, freq = 
#'c(0.3, 0.3, 0.4), continuous = FALSE, odds = odds)
#'# Run an amputation procedure with continuous probabilities
#'result4 <- ampute(data = complete.data, type = c("MARRIGHT", "MARTAIL", 
#'"MARLEFT"))
#'
#'@export
ampute <- function(data, prop = 0.5, patterns = NULL, freq = NULL,
                   mechanism = "MAR", weights = NULL, continuous = TRUE, 
                   type = NULL, odds = NULL, 
                   bycases = TRUE, run = TRUE) {
  # Multivariate amputation of complete data sets
  # 
  # This function amputes multivariate data in a MCAR or MAR manner. The details 
  # section gives a concise explanation of the why and how of this function.  
  #
  # ------------------------ sum.scores -----------------------------------
  #
  sum.scores <- function(P, data, patterns, weights, mechanism) {
    scores <- list()
    for (i in 1:nrow(patterns)) {
      if (length(P[P == (i + 1)]) == 0) {
        scores[[i]] <- 0
      } else {
        candidates <- as.matrix(data[P == (i + 1), ])
        # For each candidate in the pattern, a weighted sum score is calculated
        if (mechanism == "MAR") {
          scores[[i]] <- apply(candidates, 1, 
                               function(x) (weights[i, ] *  patterns[i, ]) %*% x)
        } else {
          scores[[i]] <- apply(candidates, 1, 
                               function(x) weights[i, ] %*% x)
        }
      }
    }
    return(scores)
  }
  # ------------------------ recalculate.prop -----------------------------
  #
  recalculate.prop <- function(prop, n, patterns, freq) {
    miss <- prop * n^2  # Desired #missing cells 
    # Calculate #cases according prop and #zeros in patterns
    cases <- c()
    for (i in 1:nrow(patterns)) {
      cases [i] <- (miss * freq[i]) / length(patterns[i,][patterns[i,] == 0]) 
    }
    if (sum(cases) > n) {
      stop("Proportion of missing cells is too large in combination with 
           the desired number of missing variables",
           call. = FALSE)
    } else {
      prop <- sum(cases) / n
    }
    return(prop)
  }
  # --------------------------  recalculate.freq -----------------------------
  #
  recalculate.freq <- function(freq) {
    s <- sum(freq)
    for (p in 1:length(freq)) {
      freq[p] <- freq[p] / s
    }
    return(freq)
  }
  # -------------------------  check.patterns ---------------------------------
  #   
  check.patterns <- function(patterns, freq, prop) {
    for (h in 1:nrow(patterns)) {
      if (any(!patterns[h, ] %in% c(0, 1))) {
        stop(paste("Argument patterns can only contain 0 and 1, pattern", h, 
                   "contains another element"), call. = FALSE)
      }
      prop.one <- 0
      row.one <- c()
      if (all(patterns[h, ] %in% 1)) {
        prop.one <- prop.one + freq[h]
        row.one <- c(row.one, h)
      }
    }
    if (prop.one != 0) {
      warning(paste("Proportion of missingness has changed from", prop, "to", 
                    prop.one, "because of a pattern with merely ones"), call. = FALSE)
      prop <- prop.one
      freq <- freq[-row.one]
      freq <- recalculate.freq(freq)
      patterns <- patterns[-row.one, ]
      warning("Frequency vector and patterns matrix have changed because of a 
              pattern with merely ones", call. = FALSE)
    }
    if (is.vector(patterns)) {
      patterns <- matrix(patterns, 1)
    }
    prop.zero <- 0
    row.zero <- c()
    for (h in 1:nrow(patterns)) {
      if (all(patterns[h, ] %in% 0)) {
        prop.zero <- prop.zero + freq[h]
        row.zero <- c(row.zero, h)
      }
    }
    objects = list(patterns = patterns,
                   prop = prop,
                   freq = freq,
                   row.zero = row.zero)
    return(objects)
  }
  # ------------------------ AMPUTE ---------------------------------------
  #
  if (is.null(data)) {
    stop("Argument data is missing, with no default", call. = FALSE)
  }
  if (!(is.matrix(data) | is.data.frame(data))) {
    stop("Data should be a matrix or data frame", call. = FALSE)
  }
  if (any(sapply(data, is.na))) {
    stop("Data cannot contain NAs", call. = FALSE)
  }
  if (ncol(data) < 2) {
    stop("Data should contain at least two columns", call. = FALSE)
  } 
  if (any(sapply(data, is.numeric) == FALSE) & mechanism != "MCAR") {
    data <- as.data.frame(sapply(data, as.numeric))
    warning("Data is made numeric because the calculation of weights requires numeric data",
            call. = FALSE)
  }
  st.data <- data.frame(scale(data))
  if (prop < 0 | prop > 100) {
    stop("Proportion of missingness should be a value between 0 and 1 
         (for a proportion) or between 1 and 100 (for a percentage)", call. = FALSE)
  } else if (prop > 1) {
    prop <- prop / 100
  }
  if (is.null(patterns)) {
    patterns <- ampute.default.patterns(n = ncol(data))
  } else if (is.vector(patterns) & (length(patterns) / ncol(data)) %% 1 == 0) {
    patterns <- matrix(patterns, length(patterns) / ncol(data), byrow = TRUE)
    if (nrow(patterns) == 1 & all(patterns[1, ] %in% 1)) {
      stop("One pattern with merely ones results to no amputation at all, the 
           procedure is therefore stopped", call. = FALSE)
    }
  } else if (is.vector(patterns)) {
    stop("Length of pattern vector does not match #variables", call. = FALSE)
  }  
  if (is.null(freq)) {
    freq <- ampute.default.freq(patterns = patterns)
  }
  if (!is.vector(freq)) {
    freq <- as.vector(freq)
    warning("Frequency should be a vector", call. = FALSE)
  }
  if (length(freq) != nrow(patterns)) {
    if (length(freq) > nrow(patterns)) {
      freq <- freq[1:length(nrow(patterns))]
    } else {
      freq <- c(freq, rep(0.2, nrow(patterns) - length(freq)))
    }
      warning(paste("Length of vector with relative frequencies does not match 
      #patterns and is therefore changed to", freq), call. = FALSE)
  }
  if (sum(freq) != 1) {
    freq <- recalculate.freq(freq = freq)
  }
  if (bycases == FALSE) {
    prop <- recalculate.prop(prop = prop, 
                             freq = freq,
                             patterns = patterns,
                             n = ncol(data))
  }
  check.pat <- check.patterns(patterns = patterns,
                              freq = freq,
                              prop = prop)
  patterns.new <- check.pat[["patterns"]]
  freq <- check.pat[["freq"]]
  prop <- check.pat[["prop"]]
  if (any(!mechanism %in% c("MCAR", "MAR", "MNAR"))) {
    stop("Mechanism should be either MCAR, MAR or MNAR", call. = FALSE)
  }
  if (!is.vector(mechanism)) {
    mechanism <- as.vector(mechanism)
    warning("Mechanism should contain merely MCAR, MAR or MNAR", call. = FALSE)
  } else if (length(mechanism) > 1) {
      mechanism <- mechanism[1]
      warning("Mechanism should contain merely MCAR, MAR or MNAR. First element is used", 
              call. = FALSE)
  }
  if (mechanism == "MCAR" & !is.null(weights)) {
    warning("Weights matrix is not used when mechanism is MCAR", call. = FALSE)
  }
  if (mechanism == "MCAR" & !is.null(odds)) {
    warning("Odds matrix is not used when mechanism is MCAR", call. = FALSE)
  }
  if (mechanism != "MCAR" & !is.null(weights)) {
    if (is.vector(weights) & (ncol(data) / length(weights))%%1 == 0) {
      weights <- matrix(weights, ncol(data) / length(weights), byrow = TRUE)
    } else if (is.vector(weights)) {
      stop("Length of weight vector does not match #variables", call. = FALSE)
    }  
  }
  if (is.null(weights)) {
    weights <- ampute.default.weights(patterns = patterns.new)
  }
  if (!is.vector(continuous)) {
    continuous <- as.vector(continuous)
    warning("Continuous should contain merely TRUE or FALSE", call. = FALSE)
  } else if (length(continuous) > 1) {
    continuous <- continuous[1]
    warning("Continuous should contain merely TRUE or FALSE. First element is used", 
            call. = FALSE)
  }
  if (!is.logical(continuous)) {
    stop("Continuous should contain TRUE or FALSE", call. = FALSE)
  }
  if (continuous == TRUE & !is.null(odds)) {
    warning("Odds matrix is not used when continuous probabilities are specified", 
            call. = FALSE)
  }
  if (continuous == FALSE & !is.null(type)) {
    warning("Type is not used when continuous probabilities are specified")
  }
  if (is.null(type)) {
    type <- ampute.default.type(patterns = patterns.new)
  }
  if (any(!type %in% c("MARLEFT","MARMID","MARTAIL","MARRIGHT"))) {
    stop("Type should contain MARLEFT, MARMID, MARTAIL or MARRIGHT", 
         call. = FALSE)
  }
  if (!is.vector(type)) {
    type <- as.vector(type)
    warning("Type should be a vector of strings", call. = FALSE)
  } else if (!length(type) %in% c(1, nrow(patterns), nrow(patterns.new))) {
    type <- type[1]
    warning("Type should either have length 1 or length equal to #patterns, first
            element is used for all patterns", call. = FALSE)
  }
  if (!is.null(odds) & !is.matrix(odds)) {
    odds <- matrix(odds, nrow(patterns.new), byrow = TRUE)
  }
  if (is.null(odds)) {
    odds <- ampute.default.odds(patterns = patterns.new)
  }
  if (continuous == FALSE) {
    for (h in 1:nrow(odds)) {
      if(any(!is.na(odds[h, ]) & odds[h, ] < 0)) {
        stop("Odds matrix can only have positive values", call. = FALSE)
      }
    }
  }
  if (!nrow(weights) %in% c(nrow(patterns), nrow(patterns.new))) {
    stop("The objects patterns and weights are not matching", call. = FALSE)
  }
  if (!nrow(odds) %in% c(nrow(patterns), nrow(patterns.new))) {
    stop("The objects patterns and odds are not matching", call. = FALSE)
  }
  #
  # Start using arguments
  # Create empty objects
  P <- NULL
  scores <- NULL
  missing.data <- NULL
  # Apply function (run = TRUE) or merely return objects (run = FALSE)
  if (run) { 
    # Assign cases to the patterns according probs
    # Because 0 and 1 will be used for missingness, 
    # the numbering of the patterns will start from 2
    P <- sample(x = 1:nrow(patterns.new), size = nrow(data), 
                replace = T, prob = freq) + 1
    # Calculate missingness according MCAR or calculate weighted sum scores
    # Standardized data is used to calculate weighted sum scores
    if (mechanism == "MCAR") {
      R <- ampute.mcar(P = P,
                       patterns = patterns.new,
                       prop = prop)
    } else {
      # Check if there is a pattern with merely zeroos
      if (!is.null(check.pat[["row.zero"]]) & mechanism == "MAR") {
        stop(paste("Patterns object contains merely zeroos and this kind of pattern 
                   is not possible when mechanism is MAR"), 
             call. = FALSE)
      } else {
        scores <- sum.scores(P = P,
                             patterns = patterns.new,
                             data = st.data,
                             weights = weights,
                             mechanism = mechanism)
      }
      if (!continuous) {
        R <- ampute.mar.disc(P = P,
                             scores = scores, 
                             odds = odds, 
                             prop = prop)
      } else if (continuous) {
        R <- ampute.mar.cont(P = P,
                             scores = scores,
                             prop = round(prop, 3),
                             type = type)
      }
    }
    missing.data <- data
    for (i in 1:nrow(patterns.new)) {
      if (any(P == (i + 1))) {
        missing.data[R[[i]] == 0, patterns.new[i, ] == 0] <- NA
      }
    }
  }
  #
  # Create return object
  call <- match.call()
  missing.data <- as.data.frame(missing.data, col.names = names(data))
  result <- list(call = call, 
                 prop = prop, 
                 patterns = patterns.new,
                 freq = freq,
                 mech = mechanism,
                 weights = weights,
                 type = type,
                 odds = odds,
                 amp = missing.data,
                 cand = P - 1,
                 scores = scores, 
                 data = as.data.frame(data))
  #
  # Return result
  oldClass(result) <- "mads"
  return(result)
}
