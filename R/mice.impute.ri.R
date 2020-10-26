#' Imputation by the random indicator method for nonignorable data
#'
#' Imputes nonignorable missing data by the random indicator method.
#'
#' @aliases mice.impute.ri ri
#' @inheritParams mice.impute.pmm
#' @param ri.maxit Number of inner iterations
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#' @author Shahab Jolani (University of Utrecht)
#' @details
#' The random indicator method estimates an offset between the
#' distribution of the observed and missing data using an algorithm
#' that iterates over the response and imputation models.
#'
#' This routine assumes that the response model and imputation model
#' have same predictors.
#'
#' For an MNAR alternative see also \code{\link{mice.impute.mnar.logreg}}.
#' @references Jolani, S. (2012).
#' \emph{Dual Imputation Strategies for Analyzing Incomplete Data}.
#' Dissertation. University of Utrecht, Dec 7 2012.
#' @family univariate imputation functions
#' @keywords datagen
#' @export
mice.impute.ri <- function(y, ry, x, wy = NULL, ri.maxit = 10, ...) {
  if (is.null(wy)) {
    wy <- !ry
  }
  x <- cbind(1, as.matrix(x))
  xy <- x
  xr <- xy
  y.dot <- y
  y.dot[wy] <- mice.impute.sample(y, ry, wy = wy)
  for (k in seq_len(ri.maxit)) {
    r.dot <- .r.draw(y.dot, ry, xr, ...)
    y.dot <- .y.draw(y, ry, r.dot, xy, wy, ...)
  }
  y.dot[wy]
}

# generting a realization of the response indicator r
.r.draw <- function(ydot, ry, xr, ...) {
  n <- length(ry)
  xr <- cbind(xr, ydot)
  expr <- expression(glm.fit(xr, ry, family = binomial(link = logit)))
  fit <- suppressWarnings(eval(expr))
  fit.sum <- summary.glm(fit)
  psi <- coef(fit)
  rv <- t(chol(sym(fit.sum$cov.unscaled)))
  psi.star <- psi + rv %*% rnorm(ncol(rv))
  p <- 1 / (1 + exp(-(xr %*% psi.star)))
  vec <- (runif(nrow(p)) <= p)
  vec[vec] <- 1
  vec[seq_len(n)]
}

# Imputation of y given rdot
.y.draw <- function(y, ry, rdot, xy, wy, ...) {
  parm <- .norm.draw(y, ry, cbind(xy, rdot), ...)
  if (all(rdot[ry] == 1) || all(rdot[ry] == 0)) parm$coef[length(parm$coef)] <- 0
  ydot <- y
  rydot <- as.logical(rdot)
  ydot[wy] <- xy[wy, , drop = FALSE] %*% parm$beta[-length(parm$coef), ] +
    rnorm(sum(wy)) * parm$sigma
  ydot[wy & !rydot] <- ydot[wy & !rydot] - parm$coef[length(parm$coef)]
  ydot
}
