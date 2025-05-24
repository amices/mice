obtain.design <- function(data, formula = ~.) {
  mf <- model.frame(formula, data = data, na.action = na.pass)
  model.matrix(formula, data = mf)
}
