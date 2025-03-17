#' Imputation of categorical data by the ordered logistic (polr) model
#'
#' The function \code{mice.impute.polr()} imputes missing data in an
#' ordinal categorical variable using the proportional odds logistic
#' regression model (`polr`). This model, also known as the cumulative
#' link model, estimates cumulative probabilities using a set of
#' threshold parameters. The method assumes that the effect of predictor
#' variables is the same across all category transitions (proportional odds
#' assumption).
#'
#' @aliases mice.impute.polr
#' @inheritParams mice.impute.pmm
#' @inheritParams mice.impute.polyreg
#' @param polr.to.loggedEvents A logical indicating whether each fall-back
#' to the \code{multinom()} function should be written to \code{loggedEvents}.
#' The default is \code{FALSE}.
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#' @details
#' The algorithm of \code{mice.impute.polr} uses the function \code{polr()} from
#' the \code{MASS} package.
#'
#' In order to avoid bias due to perfect prediction, the algorithm augment the
#' data according to the method of White, Daniel and Royston (2010).
#'
#' Calls to \code{polr} might fail if the data are very sparse.
#' In that case, \code{multinom} is tried as a fallback.
#' If the local flag \code{polr.to.loggedEvents} is set to TRUE,
#' a record is written to the \code{loggedEvents} component of
#' the \code{\link{mids}} object.
#' Use \code{mice(data, polr.to.loggedEvents = TRUE)} to set the flag.
#'
#' @note
#' In December 2019 Simon White alerted that the
#' \code{polr} could always fail silently. I can confirm this behaviour for
#' versions \code{mice 3.0.0 - mice 3.6.6}, so any method requests
#' for \code{polr} in these versions were in fact handled by \code{multinom}.
#' See \url{https://github.com/amices/mice/issues/206} for details.
#'
#' @author Stef van Buuren
#' @seealso \code{\link{mice}}, \code{\link[nnet]{multinom}},
#' \code{\link[MASS]{polr}}
#' @references
#'
#' Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}: Multivariate
#' Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical
#' Software}, \bold{45}(3), 1-67. \doi{10.18637/jss.v045.i03}
#'
#' White, I.R., Daniel, R. Royston, P. (2010). Avoiding bias due to perfect
#' prediction in multiple imputation of incomplete categorical variables.
#' \emph{Computational Statistics and Data Analysis}, 54, 2267-2275.
#'
#' Venables, W.N. & Ripley, B.D. (2002). \emph{Modern applied statistics with
#' S-Plus (4th ed)}. Springer, Berlin.
#' @family univariate imputation functions
#' @keywords datagen
#' @export
mice.impute.polr <- function(y, ry, x, wy = NULL,
                             task = "impute", model = NULL,
                             nnet.maxit = NULL, nnet.MaxNWts = NULL,
                             maxit = NULL, MaxNWts = NULL, reltol = NULL,
                             warmstart = FALSE, polr.to.loggedEvents = FALSE,
                             ...) {
  check.model.exists(model, task)
  method <- "polr"
  if (is.null(wy)) {
    wy <- !ry
  }

  # Augment data to evade issues with perfect prediction
  aug <- augment(y, ry, x, wy)
  x <- aug$x
  y <- aug$y
  ry <- aug$ry
  wy <- aug$wy
  w <- aug$w

  if (task == "fill" && check.model.match(model, x, method)) {
    impy <- polr.draw(x = x[wy, , drop = FALSE],
                      beta = model$beta.mis,
                      zeta = model$zeta.mis,
                      levels = model$factor$labels)
    return(impy)
  }

  # Escape estimation with same impute if the dependent does not vary
  cat.has.all.obs <- table(y[ry]) == sum(ry)
  if (any(cat.has.all.obs)) {
    return(rep(levels(y)[cat.has.all.obs], sum(wy)))
  }

  # Set hyper-parameters
  dots <- list(
    model = FALSE,
    method = "logistic",
    control = list(
      trace = 0L,
      maxit = ifelse(is.null(maxit), 100L, maxit),
      reltol = ifelse(is.null(reltol), 0.0001, reltol))
  )
  if (warmstart && task == "train" &&
     !is.null(model$beta.mis) && !is.null(model$zeta.mis)) {
   dots$start <- c(model$beta.mis, model$zeta.mis)
  }

  # Estimate ordered logistic (polr) model with polr
  # Fall back to multinom if polr fails
  xy <- cbind.data.frame(y, x)
  execute <- "polr"
  fun <- MASS::polr
  args <- c(list(formula = formula(xy),
                 data = xy[ry, , drop = FALSE]),
            dots)
  fit <- try(suppressWarnings(do.call(fun, args)), silent = TRUE)
  if (inherits(fit, "try-error")) {
    if (polr.to.loggedEvents) {
      updateLog(out = "polr falls back to multinom", frame = 6)
    }
    execute <- "multinom"
    impy <- mice.impute.polyreg(
      y = y, ry = ry, x = x, wy = wy,
      task = task, model = model,
      nnet.maxit = nnet.maxit, nnet.MaxNWts = nnet.MaxNWts,
      maxit = maxit, MaxNWts = MaxNWts, reltol = reltol,
      warmstart = warmstart, ...)
  }

  # Save for future use
  if (task == "train" && execute == "polr") {
    model$setup <- list(method = method,
                        n = sum(ry),
                        task = task,
                        maxit = dots$maxit,
                        reltol = dots$reltol,
                        warmstart = warmstart)
    model$result <- list(value = fit$value,
                         convergence = fit$convergence)
    model$beta.mis <- setNames(coef(fit), colnames(x))
    model$zeta.mis <- fit$zeta
    model$factor <- list(labels = levels(y), quant = NULL)
    model$xnames <- colnames(x)
  }

  # Draw imputations
  if (execute == "polr") {
    impy <- polr.draw(x = x[wy, , drop = FALSE],
                      beta = coef(fit),
                      zeta = fit$zeta,
                      levels = levels(y))
  }

  return(impy)
}

polr.draw <- function(x, beta, zeta, levels) {
  if (nrow(x) == 0L) return(character(0))
  eta <- x %*% beta
  cumpr <- plogis(matrix(zeta, nrow(x), length(zeta), byrow = TRUE) - as.vector(eta))
  post <- t(apply(cumpr, 1L, function(x) diff(c(0, x, 1))))
  un <- rep(runif(nrow(x)), each = length(levels))
  draws <- un > apply(post, 1L, cumsum)
  idx <- 1L + apply(draws, 2L, sum)
  return(levels[idx])
}

safe_call <- function(fun, args) {
  result <- try(suppressWarnings(do.call(fun, args)), silent = TRUE)
  return(result)
}
safe_polr <- function(formula, data, ...) {
  args <- list(formula = formula, data = data, ...)
  safe_call(MASS::polr, args)
}


