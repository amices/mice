obtain.design <- function(data, formula = ~.) {
  mf <- model.frame(formula, data = data, na.action = na.pass)

  # Convert logical variables to numeric to prevent dummy expansion
  for (v in names(mf)) {
    if (is.logical(mf[[v]])) {
      mf[[v]] <- as.numeric(mf[[v]])
    }
  }

  model.matrix(formula, data = mf)
}