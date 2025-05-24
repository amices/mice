#' @export
df.residual.mira <- function(object, ...) {
  fit <- object$analyses[[1]]
  df.residual(fit)
}


#' @export
df.residual.lme <- function(object, ...) {
  object$fixDF[["X"]][1]
}


#' @export
df.residual.mer <- function(object, ...) {
  sum(object@dims[2:4] * c(1, -1, -1)) + 1
}

#' @export
df.residual.multinom <- function(object, ...) {
  nrow(object$residuals) - object$edf
}
