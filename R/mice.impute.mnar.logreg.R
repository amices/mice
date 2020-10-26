#' @rdname mice.impute.mnar
#' @export
mice.impute.mnar.logreg <- function(y, ry, x, wy = NULL,
                                    ums = NULL, umx = NULL, ...) {

  ## Undentifiable part:
  u <- parse.ums(x, ums = ums, umx = umx, ...)

  if (is.null(wy)) wy <- !ry
  wyold <- wy

  ## Identifiable part: exactly the same as mice.impute.logreg

  # augment data in order to evade perfect prediction
  aug <- augment(y, ry, x, wy)
  x <- aug$x
  y <- aug$y
  ry <- aug$ry
  wy <- aug$wy
  w <- aug$w

  # fit model
  x <- cbind(1, as.matrix(x))
  expr <- expression(glm.fit(
    x = x[ry, , drop = FALSE],
    y = y[ry],
    family = quasibinomial(link = logit),
    weights = w[ry]
  ))
  fit <- eval(expr)
  fit.sum <- summary.glm(fit)
  beta <- coef(fit)
  rv <- t(chol(sym(fit.sum$cov.unscaled)))
  beta.star <- beta + rv %*% rnorm(ncol(rv))

  ## Draw imputations
  p <- 1 / (1 + exp(-(x[wy, , drop = FALSE] %*% beta.star +
    u$x[wyold, , drop = FALSE] %*% u$delta)))
  vec <- (runif(nrow(p)) <= p)
  vec[vec] <- 1
  if (is.factor(y)) {
    vec <- factor(vec, c(0, 1), levels(y))
  }

  vec
}
