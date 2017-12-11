initialize.design <- function(data) {
  
  # expand factors to dummy variables
  mf <- model.frame (~ . , data = data, na.action = na.pass)
  model.matrix(~ . , data = mf)
}
