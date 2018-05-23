 
df.residual.mira <- function(object, ...) {
    fit <- object$analyses[[1]]
    return(df.residual(fit))
}

df.residual.lme <- function(object, ...) {
    return(object$fixDF[["X"]][1])
}

df.residual.mer <- function(object, ...) {
    return(sum(object@dims[2:4] * c(1, -1, -1)) + 1)
}

df.residual.multinom <- function(object, ...) {
  return(nrow(object$residuals) - object$edf)
}
