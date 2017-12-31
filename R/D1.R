#'Compare two nested models using D1-statistic
#'
#'@param model An object of class 'mira', produced by \code{with.mids()}.
#'@param null.model An object of class 'mira', produced by \code{with.mids()}. The
#'model in \code{fit0} is a nested fit0 of \code{fit1}.
#'@param \dots Passed down to \code{\link[mitml]{testModels}}
#'@export
D1 <- function(model, null.model = NULL, ...) {
  # model: a fitlist or mira-object
  # null.model: named numerical vector, character vector, or list
  model <- as.mitml.result(model)
  est1 <- pool(model)
  qbar1 <- est1$qbar
  
  if (is.null(null.model) && is.null(remove)) {
    # test all estimates equal to zero
    beta <- rep(0, length(qbar1))
    names(beta) <- names(qbar1)
    null.model <- lapply(model, fix.coef, beta = beta)
    null.model <- as.mitml.result(null.model)
  }
  else if (is.mira(null.model)) {
    null.model <- as.mitml.result(null.model)
  }

  tmr <- testModels(model, null.model, method = "D1", ...)
  tmr
}
