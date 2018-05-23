#'Compare two nested models using D2-statistic
#'
#'@inheritParams D1
#'@inheritParams mitml::testModels
#'@details
#'The \code{D2} method does not use the \code{df.com} parameter, so
#'it does not pass it down to \code{testModels}.
#'This prevents the following warning thrown by \code{testModels}: 
#'\code{Complete-data degrees of freedom are not available 
#'for use with 'D2', and thus were ignored.}
#'@export
D2 <- function(fit1, fit0 = NULL, df.com = NULL, use = "wald", ...) {
  # fit1: a fitlist or mira-object
  # fit0: named numerical vector, character vector, or list
  fit1 <- as.mitml.result(fit1)
  est1 <- pool(fit1)
  qbar1 <- getqbar(est1)
  
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
  
  tmr <- testModels(fit1, fit0, method = "D2", use = use, ...)

  out <- list(
    call = match.call(),
    result = tmr$test,
    formulas = list(`1` = formula(fit1[[1L]]),
                    `2` = formula(fit0[[1L]])),
    m = tmr$m,
    method = "D2",
    use = use,
    df.com = NA)
  class(out) <- c("mice.anova", class(fit1))
  out
}
