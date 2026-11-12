# Helpers for D3(): log-likelihood of a fitted model evaluated at a fixed set
# of parameters.
#
# Meng and Rubin (1992) evaluate the complete-data likelihood ratio at the
# pooled estimates of all parameters, including the nuisance parameters
# (residual variance, variance components). d3.pars() extracts the parameters
# of one fitted model as a named list, D3() averages the lists over the
# imputations, and d3.loglik() evaluates the log-likelihood of each fitted
# model at the averaged parameters. The definitions follow
# mitml::testModels(method = "D3"), so that both functions give the same
# result. A d3.pars() method that returns NULL signals that the class is not
# supported; D3() then constrains only the coefficients by fix.coef() and
# re-estimates the nuisance parameters.

d3.pars <- function(model) {
  UseMethod("d3.pars")
}

#' @export
d3.pars.default <- function(model) {
  NULL
}

#' @export
d3.pars.lm <- function(model) {
  r <- residuals(model)
  w <- model$prior.weights
  if (is.null(w)) {
    w <- weights(model)
  }
  if (is.null(w)) {
    w <- rep(1, length(r))
  }
  beta <- coef(model)
  beta[is.na(beta)] <- 0
  list(beta = beta, sigma2 = sum(w * r^2) / length(r))
}

#' @export
d3.pars.glm <- function(model) {
  fam <- model$family$family
  if (fam == "gaussian") {
    return(d3.pars.lm(model))
  }
  if (fam %in% c("binomial", "poisson")) {
    beta <- coef(model)
    beta[is.na(beta)] <- 0
    return(list(beta = beta))
  }
  # families with a free dispersion parameter are not supported
  NULL
}

#' @export
d3.pars.lmerMod <- function(model) {
  beta <- lme4::fixef(model)
  theta <- lme4::getME(model, "theta")
  sig <- stats::sigma(model)
  cnms <- lme4::getME(model, "cnms")
  nvc <- lengths(cnms)
  theta.cl <- split(theta, rep.int(seq_along(cnms), (nvc * (nvc + 1)) / 2))
  Tau <- vector("list", length(cnms))
  names(Tau) <- paste0("Tau", seq_along(cnms))
  for (i in seq_along(cnms)) {
    q <- nvc[i]
    L <- matrix(0, nrow = q, ncol = q)
    L[lower.tri(L, diag = TRUE)] <- theta.cl[[i]] * sig
    Tau[[i]] <- L %*% t(L)
  }
  c(list(beta = beta), Tau, list(sigma2 = sig^2))
}

d3.loglik <- function(model, pars) {
  UseMethod("d3.loglik")
}

#' @export
d3.loglik.lm <- function(model, pars) {
  mf <- model.frame(model)
  y <- model.response(mf)
  X <- model.matrix(model)
  off <- model.offset(mf)
  if (is.null(off)) {
    off <- 0
  }
  w <- model$prior.weights
  if (is.null(w)) {
    w <- weights(model)
  }
  if (is.null(w)) {
    w <- rep(1, length(y))
  }
  beta <- pars$beta
  beta[is.na(beta)] <- 0
  r <- as.vector(y - off - X %*% beta)
  n <- length(y)
  0.5 * sum(log(w)) -
    (n / 2) * log(2 * pi * pars$sigma2) -
    sum(w * r^2) / (2 * pars$sigma2)
}

#' @export
d3.loglik.glm <- function(model, pars) {
  if (model$family$family == "gaussian") {
    return(d3.loglik.lm(model, pars))
  }
  # refit without free parameters and with the fixed linear predictor as
  # offset; the likelihood of that model is the likelihood at beta
  mf <- model.frame(model)
  X <- model.matrix(model)
  off <- model.offset(mf)
  if (is.null(off)) {
    off <- 0
  }
  beta <- pars$beta
  beta[is.na(beta)] <- 0
  eta <- as.vector(X %*% beta + off)
  fml <- formula(model)
  fml[[3L]] <- 0
  cl <- getCall(model)
  cl$formula <- fml
  cl$data <- mf
  cl$offset <- eta
  fit <- eval(cl)
  as.numeric(logLik(fit))
}

#' @export
d3.loglik.lmerMod <- function(model, pars) {
  if (any(abs(lme4::getME(model, "offset")) > .Machine$double.eps)) {
    stop("D3 does not support 'lmerMod' objects fitted with an offset.", call. = FALSE)
  }
  X <- lme4::getME(model, "X")
  eta <- as.vector(X %*% pars$beta)
  rf <- paste(deparse(formula(model, random.only = TRUE)), collapse = "")
  fml <- as.formula(sub("~", "~ 0 +", rf))
  # Do not use update(): update.formula() rewrites "~ 0 + (1 | g)" into
  # "(1 | g) - 1", for which lmer() still fits an intercept. Modify the
  # original call instead and embed the objects.
  cl <- getCall(model)
  cl$formula <- fml
  cl$data <- model.frame(model)
  cl$offset <- eta
  newobj <- eval(cl)
  Tau <- pars[grep("^Tau", names(pars))]
  theta <- unlist(lapply(Tau, function(v) {
    r <- lme4::cov2sdcor(v)
    r[lower.tri(r, diag = TRUE)]
  }))
  dev.fun <- lme4::devfun2(newobj)
  -dev.fun(pars = c(theta, sqrt(pars$sigma2))) / 2
}

# elementwise mean of a list of parameter lists
d3.pool.pars <- function(pars) {
  nms <- names(pars[[1L]])
  out <- lapply(nms, function(nm) {
    Reduce(`+`, lapply(pars, `[[`, nm)) / length(pars)
  })
  names(out) <- nms
  out
}

# refit merMod objects fitted by REML with ML
d3.ml <- function(model) {
  if (inherits(model, "merMod") && lme4::isREML(model)) {
    return(lme4::refitML(model))
  }
  model
}

# log-likelihood of each fitted model, evaluated at the pooled parameters
d3.restricted <- function(fits) {
  pars <- lapply(fits, d3.pars)
  if (any(vapply(pars, is.null, logical(1)))) {
    # class not supported: constrain the coefficients only
    qbar <- colMeans(do.call(rbind, lapply(fits, clean.coef)))
    mds <- lapply(fits, fix.coef, beta = qbar)
    return(vapply(mds, function(f) as.numeric(logLik(f)), numeric(1)))
  }
  pooled <- d3.pool.pars(pars)
  vapply(fits, d3.loglik, numeric(1), pars = pooled)
}
