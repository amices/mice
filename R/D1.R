#'Compare two nested models using D1-statistic
#'
#'@param fit1 An object of class \code{mira}, produced by \code{with()}.
#'@param fit0 An object of class \code{mira}, produced by \code{with()}. The
#'model in \code{fit0} is a nested within \code{fit1}. The default null 
#'model \code{fit0 = NULL} compares \code{fit1} to the intercept-only model.
#'@param df.com A single number or a numeric vector denoting the 
#'complete-data degrees of freedom for the hypothesis test. If not specified,
#'it is set equal to \code{df.residual} of model \code{fit1}. Not used 
#'for method \code{D2()}.
#'@param \dots Not used.
#'@export
D1 <- function(fit1, fit0 = NULL, df.com = NULL, ...) {
  # fit1: a fitlist or mira-object
  # fit0: named numerical vector, character vector, or list
  fit1 <- as.mitml.result(fit1)
  est1 <- pool(fit1)
  qbar1 <- getqbar(est1)
  
  if (is.null(fit0)) {
    # test all estimates equal to zero, except intercept
    beta <- rep(0, length(qbar1))
    names(beta) <- names(qbar1)
    fit0 <- lapply(fit1, fix.coef, beta = beta)
    fit0 <- as.mitml.result(fit0)
  }
  else if (is.mira(fit0)) {
    fit0 <- as.mitml.result(fit0)
  }
  
  # automatic setting of df.com
  if (is.null(df.com)) {
    # better option might be pair of df.com
    # pair <- list(getfit(fit1, 1), getfit(fit0, 1))
    # df.com <- unlist(sapply(pair, glance)["df.residual", ])
    df.com <- glance(getfit(fit1, 1))[, "df.residual"]
  }
  
  tmr <- testModels(fit1, fit0, method = "D1", df.com = df.com)
  
  out <- list(
    call = match.call(),
    result = tmr$test,
    formulas = list(`1` = formula(fit1[[1L]]),
                    `2` = formula(fit0[[1L]])),
    m = tmr$m,
    method = "D1",
    use = NULL,
    df.com = tmr$df.com
  )
  class(out) <- c("mice.anova", class(fit1))
  out
}
