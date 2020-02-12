#'Compare two nested models using D3-statistic
#'
#'The D3-statistics is a likelihood-ratio test statistic.
#'
#'@inheritParams D1
#'@references
#' Meng, X. L., and D. B. Rubin. 1992. 
#' Performing Likelihood Ratio Tests with Multiply-Imputed Data Sets. 
#' \emph{Biometrika}, 79 (1): 103–11.
#'
#'\url{https://stefvanbuuren.name/fimd/sec-multiparameter.html#sec:likelihoodratio}
#'@examples
#'# Compare two linear models:
#'imp <- mice(nhanes2, seed = 51009, print = FALSE)
#'mi1 <- with(data = imp, expr = lm(bmi ~ age + hyp + chl))
#'mi0 <- with(data = imp, expr = lm(bmi ~ age + hyp))
#'D3(mi1, mi0)
#'
#'# Compare two logistic regression models
#'imp  <- mice(boys, maxit = 2, print = FALSE)
#'fit1 <- with(imp, glm(gen > levels(gen)[1] ~ hgt + hc + reg, family = binomial))
#'fit0 <- with(imp, glm(gen > levels(gen)[1] ~ hgt + hc, family = binomial))
#'D3(fit1, fit0)
#'@export
D3 <- function(fit1, fit0 = NULL, df.com = Inf, ...) {
  call <- match.call()
  fit1 <- getfit(fit1)
  m <- length(fit1)
  est1 <- pool(fit1)
  qbar1 <- getqbar(est1)
  
  if (is.null(fit0)) {
    # test all estimates equal to zero
    beta <- rep(0, length(qbar1))
    names(beta) <- names(qbar1)
    fit0 <- lapply(fit1, fix.coef, beta = beta)
  }
  else fit0 <- getfit(fit0)
  
  est0 <- pool(fit0)
  qbar0 <- getqbar(est0)
  k <- length(qbar1) - length(qbar0)
  
  # For each imputed dataset, calculate the deviance between the two 
  # models as fitted
  dev1.M <- lapply(fit1, glance) %>% 
    bind_rows() %>% 
    pull(.data$deviance)
  dev0.M <- lapply(fit0, glance) %>% 
    bind_rows() %>% 
    pull(.data$deviance)
  
  # For each imputed dataset, calculate the deviance between the two 
  # models with coefficients restricted to qbar
  mds1 <- lapply(fit1, fix.coef, beta = qbar1)
  dev1.L <- lapply(mds1, glance) %>% bind_rows() %>% pull(.data$deviance)
  
  mds0 <- lapply(fit0, fix.coef, beta = qbar0)
  dev0.L <- lapply(mds0, glance) %>% bind_rows() %>% pull(.data$deviance)
  
  deviances <- list(dev1.M = dev1.M, dev0.M = dev0.M, 
                    dev1.L = dev1.L, dev0.L = dev0.L)
  
  dev.M <- mean(dev0.M - dev1.M)  # scaled deviance, as fitted
  dev.L <- mean(dev0.L - dev1.L)  # scaled deviance, restricted
  rm <- ((m + 1)/(k * (m - 1))) * (dev.M - dev.L)
  Dm <- dev.L / (k * (1 + rm))
  
  # Degrees of freedom for F distribution
  v <- k * (m - 1)
  if (v > 4) 
    w <- 4 + (v - 4) * ((1 + (1 - 2 / v) * (1 / rm))^2)  
  else 
    w <- v * (1 + 1 / k) * ((1 + 1 / rm)^2) / 2
  pvalue = 1 - pf(Dm, k, w)

  test <- 
  out <- list(
    call = match.call(),
    result = c(Dm, k, w, pvalue, rm),
    formulas = list(`1` = formula(getfit(fit1, 1L)),
                    `2` = formula(getfit(fit0, 1L))),
    m = m,
    method = "D3",
    use = NULL,
    df.com = df.com,
    deviances = deviances)
  class(out) <- c("mice.anova", class(fit1))
  out
}
