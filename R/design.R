obtain.design <- function(data, formula = ~.) {
  # try out the following
  # formula <- update(formula, . ~ . - 1)
  mf <- model.frame(formula, data = data, na.action = na.pass)
  model.matrix(formula, data = mf)
}
