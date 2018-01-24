#'Generate Missing Data for Simulation Purposes
#' 
#'This function generates multivariate missing data in a MCAR, MAR or MNAR manner. 
#'Imputation of data sets containing missing values can be performed with 
#'\code{\link{mice}}. 
#'
#'When new multiple imputation techniques are tested, missing values need to be
#'generated in simulated data sets. The generation of missing values is what
#'we call: amputation. The function \code{ampute} is developed to perform any kind
#'of amputation desired by the researcher. An extensive example and more explanation
#'of the function can be found in the vignette \emph{Generate missing values with 
#'ampute}, available in \pkg{mice} as well. For imputation, the function 
#'\code{\link{mice}} is advised.  
#'
#'Until recently, univariate amputation procedures were used to generate missing
#'data in complete, simulated data sets. With this approach, variables are made
#'incomplete one variable at a time. When several variables need to be amputed, 
#'the procedure is repeated multiple times. 
#'
#'With this univariate approach, it is difficult to relate the missingness on one 
#'variable to the missingness on another variable. A multivariate amputation procedure 
#'solves this issue and moreover, it does justice to the multivariate nature of 
#'data sets. Hence, \code{ampute} is developed to perform the amputation according 
#'the researcher's desires. 
#'
#'The idea behind the function is the specification of several missingness 
#'patterns. Each pattern is a combination of variables with and without missing 
#'values (denoted by \code{0} and \code{1} respectively). For example, one might
#'want to create two missingness patterns on a data set with four variables. The
#'patterns could be something like: \code{0, 0, 1, 1} and \code{1, 0, 1, 0}. 
#'Each combination of zeros and ones may occur. 
#'
#'Furthermore, the researcher specifies the proportion of missingness, either the 
#'proportion of missing cases or the proportion of missing cells, and the relative 
#'frequency each pattern occurs. Consequently, the data is divided over the 
#'patterns with these probabilities. Now, each case is candidate for a certain 
#'missingness pattern, but whether the case will have missing values eventually, 
#'depends on other specifications.
#'
#'The first of these specifications is the missing mechanism. There are three possible 
#'mechanisms: the missingness depends completely on chance (MCAR), the missingness 
#'depends on the values of the observed variables (i.e. the variables that remain
#'complete) (MAR) or on the values of the variables that will be made incomplete (MNAR). 
#'For a more thorough explanation of these definitions, I refer to Van Buuren 
#'(2012).   
#'
#'When the user sets the missingness mechanism to \code{"MCAR"}, the candidates 
#'have an equal probability of having missing values. No other specifications 
#'have to be made. For a \code{"MAR"} or \code{"MNAR"} mechanism, weighted sum 
#'scores are calculated. These scores are a linear combination of the 
#'variables. 
#'
#'In order to calculate the weighted sum scores, the data is standardized. That 
#'is the reason the data has to be numeric. Second, for each case, the values in 
#'the data set are multiplied with the weights, specified by argument \code{weights}. 
#'These weighted scores will be summed, resulting in a weighted sum score for each case. 
#'
#'The weights may differ between patterns and they may be negative or zero as well.
#'Naturally, in case of a \code{MAR} mechanism, the weights corresponding to the 
#'variables that will be made incomplete, have a \code{0}. Note that this might be
#'different for each pattern. In case of \code{MNAR} missingness, especially
#'the weights of the variables that will be made incomplete are of importance. However,
#'the other variables might be weighted as well. 
#'  
#'It is the relative difference between the weights that will result in an effect 
#'in the sum scores. For example, for the first missing data
#'pattern mentioned above, the weights for the third and fourth variables might
#'be set to 2 and 4. However, weight values of 0.2 and 0.4 will have the exact
#'same effect on the weighted sum score: the fourth variable is weighted twice as
#'much as variable 3.
#'
#'Based on the weighted sum scores, either a discrete or continuous distribution 
#'of probabilities is used to calculate whether a candidate will have missing values. 
#'
#'For a discrete distribution of probabilities, the weighted sum scores are 
#'divided into subgroups of equal size (quantiles). Thereafter, the user 
#'specifies for each subgroup the odds of being missing. Both the number of 
#'subgroups and the odds values are important for the generation of missing data. 
#'For example, for a RIGHT-like mechanism, scoring in one of the 
#'higher quantiles should have high missingness odds, whereas for a MID-like 
#'mechanism, the central groups should have higher odds. Again, not the size of 
#'the odds values are of importance, but the relative distance between the values. 
#'
#'The continuous distributions of probabilities are based on the logit function, as 
#'described by Van Buuren (2012). The user can specify the type of missingness, 
#'which, again, may differ between patterns.
#'
#'For an extensive example of the working of the function, I gladly refer to the 
#'vignette \emph{Generate missing values with ampute}. 
#'
#'@param data A complete data matrix or dataframe. Values should be numeric. 
#'Categorical variables should have been transformed into dummies.
#'@param prop A scalar specifying the proportion of missingness. Should be a value 
#'between 0 and 1. Default is a missingness proportion of 0.5.
#'@param patterns A matrix or data frame of size #patterns by #variables where 
#'\code{0} indicates a variable should have missing values and \code{1} indicates 
#'a variable should remain complete. The user may specify as many patterns as 
#'desired. One pattern (a vector) or double patterns are possible as well. Default 
#'is a square matrix of size #variables where each pattern has missingness on one 
#'variable only (created with \code{\link{ampute.default.patterns}}). After the 
#'amputation procedure, \code{\link{md.pattern}} can be used to investigate the 
#'missing data patterns in the data.   
#'@param freq A vector of length #patterns containing the relative frequency with 
#'which the patterns should occur. For example, for three missing data patterns, 
#'the vector could be \code{c(0.4, 0.4, 0.2)}, meaning that of all cases with 
#'missing values, 40 percent should have pattern 1, 40 percent pattern 2 and 20 
#'percent pattern 3. The vector should sum to 1. Default is an equal probability 
#'for each pattern, created with \code{\link{ampute.default.freq}}.
#'@param mech A string specifying the missingness mechanism, either MCAR 
#'(Missing Completely At Random), MAR (Missing At Random) or MNAR (Missing Not At 
#'Random). Default is a MAR missingness mechanism.    
#'@param weights A matrix or data frame of size #patterns by #variables. The matrix 
#'contains the weights that will be used to calculate the weighted sum scores. For 
#'a MAR mechanism, weights of the variables that will be made incomplete, should be 
#'zero. For a MNAR mechanism, these weights might have any possible value. Furthermore, 
#'the weights may differ between patterns and between variables. They may be negative 
#'as well. Within each pattern, the relative size of the values are of importance. 
#'The default weights matrix is made with \code{\link{ampute.default.weights}} and 
#'returns a matrix with equal weights for all variables. In case of MAR, variables 
#'that will be amputed will be weighted with \code{0}. If it is MNAR, variables 
#'that will be observed will be weighted with \code{0}. If mechanism is MCAR, the 
#'weights matrix will not be used.  
#'@param cont Logical. Whether the probabilities should be based on a continuous 
#'or discrete distribution. If TRUE, the probabilities of being missing are based 
#'on a continuous logistic distribution function. \code{\link{ampute.continuous}} 
#'will be used to calculate and assign the probabilities. These will be based on 
#'argument \code{type}. If FALSE, the probabilities of being missing are based on 
#'a discrete distribution (\code{\link{ampute.discrete}}) based on the \code{odds} 
#'argument. 
#'Default is TRUE.   
#'@param type A vector of strings containing the type of missingness for each 
#'pattern. Either \code{"LEFT"}, \code{"MID"}, \code{"TAIL"} or '\code{"RIGHT"}. 
#'If a single missingness type is entered, all patterns will be created by the same 
#'type. If missingness types should differ over patterns, a vector of missingness 
#'types should be entered. Default is RIGHT for all patterns and is the result of
#'\code{\link{ampute.default.type}}.   
#'@param odds A matrix where #patterns defines the #rows. Each row should contain 
#'the odds of being missing for the corresponding pattern. The amount of odds values 
#'defines in how many quantiles the sum scores will be divided. The values are 
#'relative probabilities: a quantile with odds value 4 will have a probability of 
#'being missing that is four times higher than a quantile with odds 1. The 
#'#quantiles may differ between the patterns, specify NA for cells remaining empty. 
#'Default is 4 quantiles with odds values 1, 2, 3 and 4, the result of 
#'\code{\link{ampute.default.odds}}.
#'@param bycases Logical. If TRUE, the proportion of missingness is defined in 
#'terms of cases. If FALSE, the proportion of missingness is defined in terms of 
#'cells. Default is TRUE.
#'@param run Logical. If TRUE, the amputations are implemented. If FALSE, the 
#'return object will contain everything but the amputed data set. 
#'
#'@return Returns an S3 object of class \code{\link{mads-class}} (multivariate 
#'amputed data set)
#'@author Rianne Schouten [aut, cre], Gerko Vink [aut], Peter Lugtig [ctb], 2016
#'@seealso \code{\link{mads-class}}, \code{\link{bwplot}}, \code{\link{xyplot}},
#'\code{\link{mice}}
#' 
#'@references Brand, J.P.L. (1999). \emph{Development, implementation and 
#'evaluation of multiple imputation strategies for the statistical analysis of 
#'incomplete data sets} (pp. 110-113). Dissertation. Rotterdam: Erasmus University. 
#'
#'Van Buuren, S., Brand, J.P.L., Groothuis-Oudshoorn, C.G.M., Rubin, D.B. (2006). 
#'Fully conditional specification in multivariate imputation. \emph{Journal of 
#'Statistical Computation and Simulation}, 76\emph{(12)}, Appendix B.
#'
#'Van Buuren, S. (2012). \emph{Flexible imputation of missing data.} 
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
#'c(0.3, 0.3, 0.4), cont = FALSE, odds = odds)
#'# Run an amputation procedure with continuous probabilities
#'result4 <- ampute(data = complete.data, type = c("RIGHT", "TAIL", "LEFT"))
#'
#'@export
ampute <- function(data, prop = 0.5, patterns = NULL, freq = NULL,
                   mech = "MAR", weights = NULL, cont = TRUE, 
                   type = NULL, odds = NULL, 
                   bycases = TRUE, run = TRUE) {
  # Generate Missing Data for Simulation Purposes
  # 
  # This function generates multivariate missing data in a MCAR, MAR or MNAR manner. 
  # The details section gives a concise explanation of the why and how of this function.  
  #
  # ------------------------ sum.scores -----------------------------------
  #
  sum.scores <- function(P, data, weights) {
    # This is an underlying function of multivariate amputation function ampute().
    # This function is used to calculate the weighted sum scores of the candidates.
    # Based on the data, the weights matrix and the kind of mechanism, each case
    # will obtain a certain score that will define his probability to be made missing.
    # The calculation of the probabilities occur in the function ampute.mcar(), 
    # ampute.continuous() or ampute.discrete(), based on the kind of missingness. 
    weights <- as.matrix(weights)
    
    f <- function(i) {
      if (length(P[P == (i + 1)]) == 0) {
        return(0)
      } else {
        candidates <- as.matrix(data[P == (i + 1), ])
        # For each candidate in the pattern, a weighted sum score is calculated
        return(apply(candidates, 1, function(x) weights[i, ] %*% x))
      }
    }
    
    scores <- lapply(seq_len(nrow(patterns)), f)
    return(scores)
  }
  #
  # ------------------------ recalculate.prop -----------------------------
  #
  recalculate.prop <- function(prop, n, patterns, freq) {
    # This is an underlying function of multivariate amputation function ampute().
    # The function recalculates the proportion of missing cases for the desired
    # #missing cells. 
    miss <- prop * n^2  # Desired #missing cells 
    # Calculate #cases according prop and #zeros in patterns
    cases <- vapply(seq_len(nrow(patterns)),
                    function(i) (miss * freq[i]) / length(patterns[i,][patterns[i,] == 0]),
                    numeric(1))
    if (sum(cases) > n) {
      stop("Proportion of missing cells is too large in combination with 
           the desired number of missing variables",
           call. = FALSE)
    } else {
      prop <- sum(cases) / n
    }
    return(prop)
  }
  #
  # --------------------------  recalculate.freq -----------------------------
  #
  recalculate.freq <- function(freq) {
    # This is an underlying function of multivariate amputation function ampute().
    # The function recalculates the frequency vector to make the sum equal to 1. 
    return(freq / sum(freq))
  }
  #
  # -------------------------  check.patterns ---------------------------------
  #   
  check.patterns <- function(patterns, freq, prop) {
    # This is an underlying function of multivariate amputation function ampute().
    # The function checks whether there are patterns with merely ones or zeroos. 
    # In case of the first, these patterns will be removed, and argument prop
    # and freq will be changed. In case there is a pattern with merely zeroos, 
    # this is ascertained and saved in the object row.zero. 
    prop.one <- 0
    row.one <- c()
    for (h in seq_len(nrow(patterns))) {
      if (any(!patterns[h, ] %in% c(0, 1))) {
        stop(paste("Argument patterns can only contain 0 and 1, pattern", h, 
                   "contains another element"), call. = FALSE)
      }
      if (all(patterns[h, ] %in% 1)) {
        prop.one <- prop.one + freq[h]
        row.one <- c(row.one, h)
      }
    }
    if (prop.one != 0) {
      warning(paste("Proportion of missingness has changed from", prop, "to", 
                    prop.one, "because of pattern(s) with merely ones"), call. = FALSE)
      prop <- prop.one
      freq <- freq[-row.one]
      freq <- recalculate.freq(freq)
      patterns <- patterns[-row.one, ]
      warning("Frequency vector and patterns matrix have changed because of 
              pattern(s) with merely ones", call. = FALSE)
    }
    prop.zero <- 0
    row.zero <- c()
    for (h in seq_len(nrow(patterns))) {
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
  #
  # ------------------------ AMPUTE ------------------------------------------
  #
  if (is.null(data)) {
    stop("Argument data is missing, with no default", call. = FALSE)
  }
  if (!(is.matrix(data) || is.data.frame(data))) {
    stop("Data should be a matrix or data frame", call. = FALSE)
  }
  if (anyNA(data)) {
    stop("Data cannot contain NAs", call. = FALSE)
  }
  if (ncol(data) < 2) {
    stop("Data should contain at least two columns", call. = FALSE)
  } 
  if (any(vapply(data, Negate(is.numeric), logical(1))) && mech != "MCAR") {
    data <- as.data.frame(sapply(data, as.numeric))
    warning("Data is made numeric because the calculation of weights requires 
            numeric data",
            call. = FALSE)
  }
  data <- data.frame(data)
  st.data <- data.frame(scale(data))
  if (prop < 0 || prop > 100) {
    stop("Proportion of missingness should be a value between 0 and 1 
         (for a proportion) or between 1 and 100 (for a percentage)", call. = FALSE)
  } else if (prop > 1) {
    prop <- prop / 100
  }
  if (is.null(patterns)) {
    patterns <- ampute.default.patterns(n = ncol(data))
  } else if (is.vector(patterns) && (length(patterns) / ncol(data)) %% 1 == 0) {
    patterns <- matrix(patterns, length(patterns) / ncol(data), byrow = TRUE)
    if (nrow(patterns) == 1 && all(patterns[1, ] %in% 1)) {
      stop("One pattern with merely ones results to no amputation at all, the 
           procedure is therefore stopped", call. = FALSE)
    }
  } else if (is.vector(patterns)) {
    stop("Length of pattern vector does not match #variables", call. = FALSE)
  }
  if (!is.data.frame(patterns)) {
    patterns <- data.frame(patterns)
    names(patterns) <- names(data)
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
      freq <- freq[seq_along(nrow(patterns))]
    } else {
      freq <- c(freq, rep.int(0.2, nrow(patterns) - length(freq)))
    }
      warning(paste("Length of vector with relative frequencies does not match 
      #patterns and is therefore changed to", freq), call. = FALSE)
  }
  if (sum(freq) != 1) {
    freq <- recalculate.freq(freq = freq)
  }
  if (!bycases) {
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
  if (any(!mech %in% c("MCAR", "MAR", "MNAR"))) {
    stop("Mechanism should be either MCAR, MAR or MNAR", call. = FALSE)
  }
  if (!is.vector(mech)) {
    mech <- as.vector(mech)
    warning("Mechanism should contain merely MCAR, MAR or MNAR", call. = FALSE)
  } else if (length(mech) > 1) {
      mech <- mech[1]
      warning("Mechanism should contain merely MCAR, MAR or MNAR. First element is used", 
              call. = FALSE)
  }
  if (mech == "MCAR" && !is.null(weights)) {
    warning("Weights matrix is not used when mechanism is MCAR", call. = FALSE)
  }
  if (mech == "MCAR" && !is.null(odds)) {
    warning("Odds matrix is not used when mechanism is MCAR", call. = FALSE)
  }
  if (mech != "MCAR" && !is.null(weights)) {
    if (is.vector(weights) & (length(weights) / ncol(data)) %% 1 == 0) {
      weights <- matrix(weights, length(weights) / ncol(data), byrow = TRUE)
    } else if (is.vector(weights)) {
      stop("Length of weight vector does not match #variables", call. = FALSE)
    }  
  }
  if (is.null(weights)) {
    weights <- ampute.default.weights(patterns = patterns.new,
                                      mech = mech)
  }
  weights <- data.frame(weights)
  names(weights) <- names(data)
  if (!is.vector(cont)) {
    cont <- as.vector(cont)
    warning("Continuous should contain merely TRUE or FALSE", call. = FALSE)
  } else if (length(cont) > 1) {
    cont <- cont[1]
    warning("Continuous should contain merely TRUE or FALSE. First element is used", 
            call. = FALSE)
  }
  if (!is.logical(cont)) {
    stop("Continuous should contain TRUE or FALSE", call. = FALSE)
  }
  if (cont && !is.null(odds)) {
    warning("Odds matrix is not used when continuous probabilities are specified", 
            call. = FALSE)
  }
  if (!cont && !is.null(type)) {
    warning("Type is not used when continuous probabilities are specified",
            call. = FALSE)
  }
  if (is.null(type)) {
    type <- ampute.default.type(patterns = patterns.new)
  }
  if (any(!type %in% c("LEFT","MID","TAIL","RIGHT"))) {
    stop("Type should contain LEFT, MID, TAIL or RIGHT", 
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
  if (!is.null(odds) && !is.matrix(odds)) {
    odds <- matrix(odds, nrow(patterns.new), byrow = TRUE)
  }
  if (is.null(odds)) {
    odds <- ampute.default.odds(patterns = patterns.new)
  }
  if (!cont) {
    for (h in seq_len(nrow(odds))) {
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
    P <- sample.int(n = nrow(patterns.new), size = nrow(data), 
                replace = TRUE, prob = freq) + 1
    # Calculate missingness according MCAR or calculate weighted sum scores
    # Standardized data is used to calculate weighted sum scores
    if (mech == "MCAR") {
      R <- ampute.mcar(P = P,
                       patterns = patterns.new,
                       prop = prop)
    } else {
      # Check if there is a pattern with merely zeroos
      if (!is.null(check.pat[["row.zero"]]) && mech == "MAR") {
        stop(paste("Patterns object contains merely zeros and this kind of pattern 
                   is not possible when mechanism is MAR"), 
             call. = FALSE)
      } else {
        scores <- sum.scores(P = P,
                             data = st.data,
                             weights = weights)
      }
      if (!cont) {
        R <- ampute.discrete(P = P,
                             scores = scores, 
                             odds = odds, 
                             prop = prop)
      } else if (cont) {
        R <- ampute.continuous(P = P,
                               scores = scores,
                               prop = round(prop, 3),
                               type = type)
      }
    }
    missing.data <- data
    for (i in seq_len(nrow(patterns.new))) {
      if (any(P == (i + 1))) {
        missing.data[R[[i]] == 0, patterns.new[i, ] == 0] <- NA
      }
    }
  }
  #
  # Create return object
  call <- match.call()
  missing.data <- data.frame(missing.data)
  result <- list(call = call, 
                 prop = prop, 
                 patterns = patterns.new,
                 freq = freq,
                 mech = mech,
                 weights = weights,
                 cont = cont,
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
