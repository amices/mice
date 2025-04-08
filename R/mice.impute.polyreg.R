#' Imputation of unordered data by polytomous regression
#'
#' Imputes missing data in a categorical variable using polytomous regression
#' for unordered factors.
#'
#' @aliases mice.impute.polyreg
#' @inheritParams mice.impute.pmm
#' @param maxit Tuning parameter for \code{nnet()}.
#' @param MaxNWts Tuning parameter for \code{nnet()}. Internally, the procedure
#' computes the number of weights needed for the multinomial model as
#' 100 + \code{ncol(x)} times \code{length(levels(y)) - 1L)}.
#' Use \code{MaxNWts} to override this default if you get the
#' “too many weights” error.
#' @param nnet.maxit Legacy parameter.
#' @param nnet.MaxNWts Legacy parameter.
#' @param reltol Convergence parameter for \code{nnet()}.
#' @param warmstart Logical. If \code{TRUE}, the estimation process
#' uses weights from the previous iteration as warm starts.
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#' @author Stef van Buuren, Karin Groothuis-Oudshoorn, 2000-2010
#' @details
#' The function \code{mice.impute.polyreg()} imputes categorical response
#' variables by the Bayesian polytomous regression model. See J.P.L. Brand
#' (1999), Chapter 4, Appendix B.
#'
#' The method consists of the following steps:
#' \enumerate{
#' \item Fit categorical response as a multinomial model
#' \item Compute predicted categories
#' \item Add appropriate noise to predictions
#' }
#'
#' The algorithm of \code{mice.impute.polyreg} uses the function
#' \code{multinom()} from the \code{nnet} package.
#'
#' In order to avoid bias due to perfect prediction, the algorithm augment the
#' data according to the method of White, Daniel and Royston (2010).
#' @seealso \code{\link{mice}}, \code{\link[nnet]{multinom}},
#' \code{\link[MASS]{polr}}
#' @references
#'
#' Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}: Multivariate
#' Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical
#' Software}, \bold{45}(3), 1-67. \doi{10.18637/jss.v045.i03}
#'
#' Brand, J.P.L. (1999) \emph{Development, implementation and evaluation of
#' multiple imputation strategies for the statistical analysis of incomplete
#' data sets.} Dissertation. Rotterdam: Erasmus University.
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
mice.impute.polyreg <- function(
    y, ry, x, wy = NULL,
    task = "impute", model = NULL,
    nnet.maxit = NULL, nnet.MaxNWts = NULL,
    maxit = NULL, MaxNWts = NULL, reltol = NULL,
    warmstart = FALSE, ...) {

  check.model.exists(model, task)
  method <- "polyreg"
  if (is.null(wy)) wy <- !ry

  # Augment data
  aug <- augment(y, ry, x, wy)
  x <- aug$x
  y <- aug$y
  ry <- aug$ry
  wy <- aug$wy
  w <- aug$w

  if (task == "fill") {
    x <- cbind(`(Intercept)` = rep(1, nrow(x)), x)
    cols <- check.model.match(model, x, method)
    x <- x[wy, cols, drop = FALSE]
    return(polyreg.draw(
      x = x,
      beta = model$beta.dot,
      levels = model$factor$labels,
      class = model$class[1L])
    )
  }

  # Escape perfect prediction
  cat.has.all.obs <- table(y[ry]) == sum(ry)
  if (any(cat.has.all.obs)) {
    return(rep(levels(y)[cat.has.all.obs], sum(wy)))
  }

  # Set hyperparameters
  if (!missing(nnet.maxit)) maxit <- nnet.maxit
  if (!missing(nnet.MaxNWts)) MaxNWts <- nnet.MaxNWts
  MaxNWts_needed <- 100L + as.integer(ncol(x) * (length(levels(y)) - 1))
  dots <- list(...)
  dots$maxit <- ifelse(is.null(maxit), 100L, maxit)
  dots$MaxNWts <- ifelse(is.null(MaxNWts), MaxNWts_needed, MaxNWts)
  dots$reltol <- ifelse(is.null(reltol), 0.0001, reltol)
  if (warmstart && task == "train" && !is.null(model$wts)) {
    dots$Wts <- model$wts
  }

  # Fit multinomial model
  y <- droplevels(y)
  xy <- cbind.data.frame(y, x)
  fit <- do.call(nnet::multinom, c(
    list(formula(xy),
         data = xy[ry, , drop = FALSE],
         weights = w[ry],
         model = FALSE, trace = FALSE),
    dots
  ))

  # Process beta coefficients
  x <- x[wy, , drop = FALSE]
  x <- cbind(`(Intercept)` = rep(1, nrow(x)), x)
  beta <- coef(fit)
  if (is.vector(beta)) {
    beta <- matrix(beta, ncol = 1L)
  } else {
    beta <- t(beta)
  }
  rownames(beta) <- gsub("`", "", rownames(beta))

  if (task == "train") {
    model$setup <- list(
      method = method,
      n = sum(ry),
      task = task,
      maxit = dots$maxit,
      MaxNWts = dots$MaxNWts,
      reltol = dots$reltol,
      warmstart = warmstart
    )
    model$result <- list(
      nWts = length(fit$wts),
      value = fit$value,
      convergence = fit$convergence
    )
    model$beta.dot <- beta
    if (warmstart) model$wts <- fit$wts
    model$factor <- list(labels = levels(y), quant = NULL)
    model$class <- if (is.ordered(y)) "ordered" else class(y)[1L]
    model$xnames <- colnames(x)
  }

  # Return imputed values
  polyreg.draw(
    x = x,
    beta = beta,
    levels = levels(y),
    class = if (is.ordered(y)) "ordered" else class(y)[1L]
  )
}

polyreg.draw <- function(x, beta, levels, class = NULL) {
  if (nrow(x) == 0L) return(character(0))
  lp <- x %*% beta
  p <- exp(lp) / rowSums(exp(lp) + 1)
  post <- cbind(1 - rowSums(p), p)

  un <- rep(runif(nrow(x)), each = length(levels))
  draws <- un > apply(post, 1L, cumsum)
  idx <- 1L + apply(draws, 2L, sum)

  out <- levels[idx]

  if (!is.null(class) && class %in% c("factor", "ordered")) {
    out <- factor(out, levels = levels, ordered = (class == "ordered"))
  }

  return(out)
}
