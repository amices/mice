#'Compare two nested models using D2-statistic
#'
#'@inheritParams D1
#'@export
D2 <- function(fit1, fit0 = NULL, ...) {
  # fit1: a fitlist or mira-object
  # fit0: named numerical vector, character vector, or list
  fit1 <- as.mitml.result(fit1)
  est1 <- pool(fit1)
  qbar1 <- est1$qbar
  
  if (is.null(fit0)) {
    # test all estimates equal to zero
    beta <- rep(0, length(qbar1))
    names(beta) <- names(qbar1)
    fit0 <- lapply(fit1, fix.coef, beta = beta)
    fit0 <- as.mitml.result(fit0)
  }
  else if (is.mira(fit0)) {
    fit0 <- as.mitml.result(fit0)
  }
  
  tmr <- testModels(fit1, fit0, method = "D2", ...)
  tmr
}
