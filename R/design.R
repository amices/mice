obtain.design <- function(data, formula = ~ .) {
  
  mf <- model.frame(formula, data = data, na.action = na.pass)
  model.matrix(formula, data = mf)
}

update.design <- function(design, data, varname = ".") {
  # Updates columns of the design matrix related to variable
  # varname in data
  
  varname <- as.character(varname[1])
  idx <- attr(design, "assign") %in% grep(varname, names(data))
  
  # variable j not found
  if (varname == "" || !any(idx)) return(design)
  
  # create model frame of variable j only
  fj <- as.formula(paste("~", varname))
  mfj <- model.frame(fj, data = data, na.action = na.pass)
  design[, idx] <- model.matrix(fj, data = mfj)[, -1, drop = FALSE]
  design
}
