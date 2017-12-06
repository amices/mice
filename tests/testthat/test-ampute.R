context("ampute")

require(MASS)
sigma <- matrix(data = c(1, 0.2, 0.2, 0.2, 1, 0.2, 0.2, 0.2, 1), nrow = 3)
complete.data <- mvrnorm(n = 100, mu = c(5, 5, 5), Sigma = sigma)

test_that("all examples work", {
  expect_error(ampute(data = complete.data), NA)
  
  result1 <- ampute(data = complete.data)
  patterns <- result1$patterns
  patterns[1:3, 2] <- 0
  odds <- result1$odds
  odds[2,3:4] <- c(2, 4)
  odds[3,] <- c(3, 1, NA, NA)
  
  expect_error(ampute(data = complete.data, patterns = patterns, 
                      freq = c(0.3, 0.3, 0.4), cont = FALSE, odds = odds), NA)
  expect_error(ampute(data = complete.data, type = c("RIGHT", "TAIL", "LEFT")), NA)

})