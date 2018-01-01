#'Compare two nested models using D3-statistic
#'
#'@param fit1 An object of class 'mira', produced by \code{with.mids()}.
#'@param fit0 An object of class 'mira', produced by \code{with.mids()}. The
#'model in \code{fit0} is a nested fit0 of \code{fit1}.
#'@param \dots Not used
#'@author Stef van Buuren, 2018
#'@export
D3 <- function(fit1, fit0 = NULL, ...) {
  fit1 <- getfit(fit1)
  m <- length(fit1)
  est1 <- pool(fit1)
  qbar1 <- est1$qbar
  
  if (is.null(fit0)) {
    # test all estimates equal to zero
    beta <- rep(0, length(qbar1))
    names(beta) <- names(qbar1)
    fit0 <- lapply(fit1, fix.coef, beta = beta)
  }
  else fit0 <- getfit(fit0)
  
  est0 <- pool(fit0)
  k <- length(est1$qbar) - length(est0$qbar)
  
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
  qbar1 <- pool(fit1)$qbar
  mds1 <- lapply(fit1, fix.coef, beta = qbar1)
  dev1.L <- lapply(mds1, glance) %>% bind_rows() %>% pull(.data$deviance)
  
  qbar0 <- pool(fit0)$qbar
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

  statistic <- list(m = m, 
                    qbar1 = est1$qbar, qbar0 = est0$qbar, 
                    ubar1 = est1$ubar, ubar0 = est0$ubar, 
                    deviances = deviances,
                    Dm = Dm, rm = rm, df1 = k, df2 = w, 
                    pvalue = pvalue)
  statistic
}
