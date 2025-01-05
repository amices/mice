#' Imputation by predictive mean matching
#'
#' @aliases mice.impute.lasso.pmm
#' @param y Vector to be imputed
#' @param ry Logical vector of length \code{length(y)} indicating the
#' the subset \code{y[ry]} of elements in \code{y} to which the imputation
#' model is fitted. The \code{ry} generally distinguishes the observed
#' (\code{TRUE}) and missing values (\code{FALSE}) in \code{y}.
#' @param x Numeric design matrix with \code{length(y)} rows with predictors for
#' \code{y}. Matrix \code{x} may have no missing values.
#' @param remove.values Dependent values to exclude from the imputation model
#' and the collection of donor values
#' @param quantify Logical. If \code{TRUE}, factor levels are replaced
#' by the first canonical variate before fitting the imputation model.
#' If false, the procedure reverts to the old behaviour and takes the
#' integer codes (which may lack a sensible interpretation).
#' Relevant only of \code{y} is a factor.
#' @param trim Scalar integer. Minimum number of observations required in a
#' category in order to be considered as a potential donor value.
#' Relevant only of \code{y} is a factor.
#' @param wy Logical vector of length \code{length(y)}. A \code{TRUE} value
#' indicates locations in \code{y} for which imputations are created.
#' @param donors The size of the donor pool among which a draw is made.
#' The default is \code{donors = 5L}. Setting \code{donors = 1L} always selects
#' the closest match, but is not recommended. Values between 3L and 10L
#' provide the best results in most cases (Morris et al, 2015).
#' @param matchtype Type of matching distance. The default choice
#' (\code{matchtype = 1L}) calculates the distance between
#' the \emph{predicted} value of \code{yobs} and
#' the \emph{drawn} values of \code{ymis} (called type-1 matching).
#' Other choices are \code{matchtype = 0L}
#' (distance between predicted values) and \code{matchtype = 2L}
#' (distance between drawn values). The functions sets \code{matchtype = 1L}
#' for sample sizes to 1000, and \code{matchtype = 0L} otherwise.
#' @param dfmax Maximum number of non-zero coefficients in the LASSO path.
#' @param \dots Other named arguments.
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#' @note
#' This version uses glmnet with \code{dfmax} to speed up the process
#' @author Stef van Buuren
#' @references Little, R.J.A. (1988), Missing data adjustments in large surveys
#' (with discussion), Journal of Business Economics and Statistics, 6, 287--301.
#'
#' Morris TP, White IR, Royston P (2015). Tuning multiple imputation by predictive
#' mean matching and local residual draws. BMC Med Res Methodol. ;14:75.
#'
#' Van Buuren, S. (2018).
#' \href{https://stefvanbuuren.name/fimd/sec-pmm.html}{\emph{Flexible Imputation of Missing Data. Second Edition.}}
#' Chapman & Hall/CRC. Boca Raton, FL.
#'
#' Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}: Multivariate
#' Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical
#' Software}, \bold{45}(3), 1-67. \doi{10.18637/jss.v045.i03}
#' @family univariate imputation functions
#' @keywords datagen
#' @examples
#' # We normally call mice.impute.lasso.pmm() from within mice()
#' # But we may call it directly as follows (not recommended)
#'
#' set.seed(53177)
#' xname <- c("age", "hgt", "wgt")
#' r <- stats::complete.cases(boys[, xname])
#' x <- boys[r, xname]
#' y <- boys[r, "tv"]
#' ry <- !is.na(y)
#' table(ry)
#'
#' # percentage of missing data in tv
#' sum(!ry) / length(ry)
#'
#' # Impute missing tv data
#' yimp <- mice.impute.lasso.pmm(y, ry, x)
#' length(yimp)
#' hist(yimp, xlab = "Imputed missing tv")
#'
#' # Impute all tv data
#' yimp <- mice.impute.lasso.pmm(y, ry, x, wy = rep(TRUE, length(y)))
#' length(yimp)
#' hist(yimp, xlab = "Imputed missing and observed tv")
#' plot(jitter(y), jitter(yimp),
#'   main = "Predictive mean matching on age, height and weight",
#'   xlab = "Observed tv (n = 224)",
#'   ylab = "Imputed tv (n = 224)"
#' )
#' abline(0, 1)
#' cor(y, yimp, use = "pair")
#'
#' # Use blots to exclude different values per column
#' # Create blots object
#' blots <- make.blots(boys)
#' # Exclude ml 1 through 5 from tv donor pool
#' blots$tv$remove.values <- c(1:5)
#' # Exclude 100 random observed heights from tv donor pool
#' blots$hgt$remove.values <- sample(unique(boys$hgt), 100)
#' imp <- mice(boys, method = "lasso.pmm", m = 1, maxit = 1, print = FALSE,
#'        blots = blots, seed = 123)
#' #' Check if all values are removed
#' all(!blots$hgt$remove.values %in% unlist(c(imp$imp$hgt[, -"row_id"])))
#' all(!blots$tv$remove.values %in% unlist(c(imp$imp$tv[, -"row_id"])))
#'
#' # Factor quantification
#' xname <- c("age", "hgt", "wgt")
#' br <- boys[c(1:10, 101:110, 501:510, 601:620, 701:710), ]
#' r <- stats::complete.cases(br[, xname])
#' x <- br[r, xname]
#' y <- factor(br[r, "tv"])
#' ry <- !is.na(y)
#' table(y, useNA = "always")
#'
#' # impute 38 NA's in factor by optimal scaling of y | x
#' table(mice.impute.lasso.pmm(y, ry, x))
#'
#' # only categories with at least 2 cases can be donor
#' table(mice.impute.lasso.pmm(y, ry, x, trim = 2L))
#'
#' # in addition, eliminate category 20
#' table(mice.impute.lasso.pmm(y, ry, x, trim = 2L, remove.value = 20))
#'
#' # to get old behavior: as.integer(y))
#' table(mice.impute.lasso.pmm(y, ry, x, quantify = FALSE))
#' @export
mice.impute.lasso.pmm <- function(y, ry, x, wy = NULL,
                             donors = 5L,
                             matchtype = ifelse(sum(ry) >= 1000L, 0L, 1L),
                             remove.values = NULL,
                             quantify = TRUE,
                             trim = 1L,
                             dfmax = NULL,
                             ...) {
  stopifnot(ncol(x) >= 2L)
  x <- as.matrix(x)

  dots <- list(...)
  if ("type" %in% names(dots)) {
    dots[["type"]] <- NULL
  }
  if ("exclude" %in% names(dots)) {
    dots[["exclude"]] <- NULL
  }

  if (is.null(wy)) {
    wy <- !ry
  }

  # Remove rare categories from the outcome variable
  if (is.factor(y)) {
    active <- !ry | y %in% (levels(y)[table(y) >= trim])
    y <- y[active]
    ry <- ry[active]
    x <- x[active, , drop = FALSE]
    wy <- wy[active]
  }

  # Remove donor values from the outcome variable
  if (!is.null(remove.values)) {
    active <- !ry | !y %in% remove.values
    y <- y[active]
    ry <- ry[active]
    x <- x[active, , drop = FALSE]
    wy <- wy[active]
  }

  # Quantify categories of factors
  ynum <- y
  if (is.factor(y)) {
    if (quantify) {
      ynum <- quantify(y, ry, x)
    } else {
      # as.integer() may not make sense for unordered factors
      ynum <- as.integer(y)
    }
  }

  # Define yobs and xobs for modeling
  xobs <- x[ry & complete.cases(x, y), , drop = FALSE]
  yobs <- as.numeric(ynum[ry])

  # If yobs is near constant, return imputations as random draws from yobs
  if (var(yobs, na.rm = TRUE) < 1000 * .Machine$double.eps) {
    return(sample(yobs, sum(wy), replace = TRUE))
  }

  # Create partial LASSO path with dfmax non-zero coefficients
  dfmax <- ifelse(is.null(dfmax), ncol(x), dfmax)
  fit <- do.call(glmnet::glmnet,
                 c(list(x = xobs, y = yobs, dfmax = dfmax), dots))

  # Find lambda from partial LASSO path
  indices <- which(fit$df <= dfmax)
  closest <- indices[which.max(fit$df[indices])]
  lambda <- fit$lambda[closest]

  # Calculate predicted values for ximp
  ximp <- x[wy, , drop = FALSE]
  if (matchtype == 0L) {
    # One model for observed and missing data
    yhatobs <- predict(fit, newx = xobs, s = lambda)
    yhatmis <- predict(fit, newx = ximp, s = lambda)
  }
  if (matchtype == 1L) {
    # Two different models for observed and missing data
    # Bootstrap observed records to account for sampling uncertainty
    n <- sum(ry)
    s <- sample(n, n, replace = TRUE)
    xobs1 <- x[ry & complete.cases(x, y), , drop = FALSE][s, , drop = FALSE]
    yobs1 <- as.numeric(ynum[ry][s])
    fit1 <- do.call(glmnet::glmnet,
                    c(list(x = xobs1, y = yobs1, lambda = lambda), dots))
    yhatobs <- predict(fit, newx = xobs, s = lambda)
    yhatmis <- predict(fit1, newx = ximp, s = lambda)
  }
  if (matchtype == 2L) {
    # One refitted model for observed and missing data
    n <- sum(ry)
    s <- sample(n, n, replace = TRUE)
    xobs1 <- x[ry & complete.cases(x, y), , drop = FALSE][s, , drop = FALSE]
    yobs1 <- as.numeric(ynum[ry][s])
    fit1 <- do.call(glmnet::glmnet,
                    c(list(x = xobs1, y = yobs1, lambda = lambda), dots))
    yhatobs <- predict(fit1, newx = xobs, s = lambda)
    yhatmis <- predict(fit1, newx = ximp, s = lambda)
  }

  # Predictive mean matching
  idx <- matchindex(yhatobs, yhatmis, donors)
  yimp <- y[ry][idx]

  return(yimp)
}
