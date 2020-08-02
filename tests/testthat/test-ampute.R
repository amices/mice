context("ampute")

# make objects for testfunctions
sigma <- matrix(data = c(1, 0.2, 0.2, 0.2, 1, 0.2, 0.2, 0.2, 1), nrow = 3)
complete.data <- MASS::mvrnorm(n = 100, mu = c(5, 5, 5), Sigma = sigma)

test_that("all examples work", {
  
  compl_boys <- cc(boys)[1:3]
  
  expect_error(ampute(data = compl_boys), NA)
  
  mads_boys <- ampute(data = compl_boys)
  
  my_patterns <- mads_boys$patterns
  my_patterns[1:3, 2] <- 0

  my_weights <- mads_boys$weights
  my_weights[2, 1] <- 2
  my_weights[3, 1] <- 0.5

  expect_error(ampute(data = compl_boys, patterns = my_patterns, 
                      freq = c(0.3, 0.3, 0.4), weights = my_weights, 
                      type = c("RIGHT", "TAIL", "LEFT")), NA)

})

test_that("all arguments work", {
  
  # empty run
  expect_error(ampute(data = complete.data, run = FALSE), NA)
  # missingness by cells
  expect_error(ampute(data = complete.data, prop = 0.1, bycases = FALSE), NA)
  # prop with 3 dec, weigths with negative values, unequal odds matrix
  expect_error(ampute(data = complete.data, prop = 0.314, 
                      freq = c(0.25, 0.4, 0.35),
                      patterns = matrix(data = c(1, 0, 1, 
                                                 0, 1, 0, 
                                                 0, 1, 1), 
                                        nrow = 3, byrow = TRUE),
                      weights = matrix(data = c(-1, 1, 0, 
                                                -4, -4, 1,  
                                                0, 0, -1),
                                       nrow = 3, byrow = TRUE),
                      odds = matrix(data = c(1, 4, NA, NA,
                                             0, 3, 3, NA,
                                             4, 1, 1, 4), 
                                    nrow = 3, byrow = TRUE), 
                      cont = FALSE), NA)
  # 1 pattern with vector for patterns and weights
  expect_error(ampute(data = complete.data, freq = 1, patterns = c(1, 0, 1), 
                     weights = c(3, 3, 0)), NA)
  # multiple patterns given in vectors
  expect_error(ampute(data = complete.data, patterns = c(1, 0, 1, 1, 0, 0), 
                      cont = TRUE, weights = c(1, 4, -2, 0, 1, 2),
                      type = c("LEFT","TAIL")), NA)
  # one pattern with odds vector
  expect_error(ampute(data = complete.data, patterns = c(1, 0, 1), 
                      weights = c(4, 1, 0), odds = c(2, 1), cont = FALSE), NA)
  # argument standardized
  expect_error(ampute(data = complete.data, std = FALSE), NA)
  
  # sum scores cannot be NaN
  dich.data <- matrix(c(0, 0, 0, 1, 0, 0, 0, 0, 0,
                        1, 0, 0, 0, 0, 0, 0, 0, 0), ncol = 2, byrow = FALSE)
  wss <- ampute(data = dich.data, mech = "MNAR")$scores
  check_na <- function(x){return(any(is.na(x)))}
  expect_false(any(unlist(lapply(wss, check_na))))
})

test_that("function works around unusual arguments", {
  
  # data
  nasty.data <- complete.data
  nasty.data[, 1] <- rep(c("one", "two"), 50)
  # when data is categorical and mech != mcar, warning is expected
  expect_warning(ampute(data = nasty.data), 
                 "Data is made numeric because the calculation of weights requires numeric data")
  # when data is categorical and mech = mcar, function can continue
  expect_warning(ampute(data = nasty.data, mech = "MCAR"), NA) 
  
  # patterns
  expect_error(ampute(data = complete.data, patterns = c(0, 0, 0), mech = "MCAR"), NA)
  expect_error(ampute(data = complete.data, patterns = c(0, 0, 1, 0, 0, 0), mech = "MNAR"), NA)
  expect_warning(ampute(data = complete.data, patterns = c(1, 1, 1, 0, 1, 0)))
  
  # freq
  expect_warning(ampute(data = complete.data, freq = c(0.8, 0.4)))
  
  # prop
  expect_warning(ampute(data = complete.data, prop = 1))
  expect_error(ampute(data = complete.data, prop = 48.5), NA)

  # mech, type and weights
  expect_warning(ampute(data = complete.data, mech = c("MCAR", "MAR")), 
                 "Mechanism should contain merely MCAR, MAR or MNAR. First element is used")
  expect_warning(ampute(data = complete.data, type = c("LEFT", "RIGHT")),
                 "Type should either have length 1 or length equal to #patterns, first element is used for all patterns")
  expect_warning(ampute(data = complete.data, mech = "MCAR", 
                        odds = matrix(data = c(1, 4, NA, NA,
                                               0, 3, 3, NA,
                                               4, 1, 1, 4), 
                                      nrow = 3, byrow = TRUE), cont = FALSE), 
                 "Odds matrix is not used when mechanism is MCAR")
  expect_warning(ampute(data = complete.data, mech = "MCAR", 
                        weights = c(1, 3, 4)), 
                 "Weights matrix is not used when mechanism is MCAR")
  expect_warning(ampute(data = complete.data, odds = matrix(data = c(1, 4, NA, NA,
                                                                     0, 3, 3, NA,
                                                                     4, 1, 1, 4), 
                                                            nrow = 3, byrow = TRUE)))
  expect_warning(ampute(data = complete.data, cont = FALSE, type = "LEFT"))

})

test_that("error messages work properly", {
  
  # data
  expect_error(ampute(data = as.list(complete.data)), 
               "Data should be a matrix or data frame")
  
  nasty.data <- complete.data
  nasty.data[1:10, 1] <- NA
  
  expect_error(ampute(data = nasty.data), "Data cannot contain NAs")
  expect_error(ampute(data = as.data.frame(complete.data[, 1])),
               "Data should contain at least two columns")
  
  # prop
  expect_error(ampute(data = complete.data, prop = 104))
  expect_error(ampute(data = complete.data, prop = 0.9, bycases = FALSE), 
               "Proportion of missing cells is too large in combination with the desired number of missing variables")
  
  # patterns
  expect_error(ampute(data = complete.data, patterns = c(1, 1, 1)), 
               "One pattern with merely ones results to no amputation at all, the procedure is therefore stopped")
  expect_error(ampute(data = complete.data, patterns = c(0, 0, 0), mech = "MAR"),
               "Patterns object contains merely zeros and this kind of pattern is not possible when mechanism is MAR")
  expect_error(ampute(data = complete.data, patterns = c(1, 0, 1, 1)), 
               "Length of pattern vector does not match #variables")
  expect_error(ampute(data = complete.data, patterns = c(1, 0, 2)),
               "Argument patterns can only contain 0 and 1, pattern 1 contains another element")
  expect_error(ampute(data = complete.data, mech = "MAR", patterns = c(0, 0, 1, 0, 0, 0)),
               "Patterns object contains merely zeros and this kind of pattern is not possible when mechanism is MAR")
  
  # mech, type, weights and odds
  expect_error(ampute(data = complete.data, mech = "MAAR"),
               "Mechanism should be either MCAR, MAR or MNAR")
  expect_error(ampute(data = complete.data, type = "MARLEFT"),
               "Type should contain LEFT, MID, TAIL or RIGHT")
  expect_error(ampute(data = complete.data, weights = c(1, 2, 1, 4)), 
               "Length of weight vector does not match #variables")
  expect_error(ampute(data = complete.data, 
                      odds = matrix(c(1, 4, -3, 2, 1, 1), nrow = 3), 
                      cont = FALSE), "Odds matrix can only have positive values")
  expect_error(ampute(data = complete.data, 
                      patterns = matrix(data = c(1, 0, 1, 
                                                 0, 1, 0, 
                                                 0, 1, 1), 
                                        nrow = 3, byrow = TRUE),
                      weights = matrix(data = c(-1, 1, 0, 
                                                -4, -4, 1,  
                                                0, 0, -1,
                                                1, 1, 0),
                                       nrow = 4, byrow = TRUE)), 
               "The objects patterns and weights are not matching")
  expect_error(ampute(data = complete.data, 
                      patterns = matrix(data = c(1, 0, 1, 
                                                 0, 1, 0, 
                                                 0, 1, 1), 
                                        nrow = 3, byrow = TRUE),
                      odds = matrix(data = c(1, 4, NA, NA,
                                             0, 3, 3, 0), 
                                    nrow = 2, byrow = TRUE), cont = FALSE), 
               "The objects patterns and odds are not matching")

})


# The following test was contributed by Shangzhi-hong (#216) Dec 2019
context("ampute robust version")

set.seed(1)

# Set-up
# Dataset
NUM_OBS_DF <- 25
NUM_VAR_DF <- 10

data <- replicate(
  n = NUM_VAR_DF,
  expr = { rnorm(n = NUM_OBS_DF, mean = 1, sd = 1) },
  simplify = "matrix")

# Ampute pattern
covNum <- NUM_VAR_DF - 1
misPatCov1 <- t(combn(x = covNum, m = 1,
                      FUN = function(x) replace(rep(1, covNum), x, 0)))
misPat1 <- cbind(rep(1, choose(covNum, 1)), misPatCov1)
misPatCov2 <- t(combn(x = covNum, m = 2,
                      FUN = function(x) replace(rep(1, covNum), x, 0)))
misPat2 <- cbind(rep(1, choose(covNum, 2)), misPatCov2)
patterns <- rbind(misPat1, misPat2)
weights <- matrix(0, nrow = nrow(patterns), ncol = ncol(patterns))
weights[,1] <- 1

prop <- 0.5
mech <- "MAR"
type <- "RIGHT"
bycases <- TRUE
# Other params
# freq <- NULL
# std <- TRUE
# cont <- TRUE
# type <- NULL
# odds <- NULL
# run <- TRUE

test_that("ampute() works under extreme condition", {
  ampDf <- ampute(
    data = data,
    prop = prop,
    mech = mech,
    type = type,
    bycases = bycases,
    patterns = patterns,
    weights = weights
  )$amp
  outProp <-  sum(complete.cases(ampDf)) / NUM_OBS_DF
  expect_true(outProp > 0.3 & outProp < 0.7)
})

# --- end test Shangzhi-hong (#216) Dec 2019