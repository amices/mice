#' Imputation by predictive mean matching
#'
#' @aliases mice.impute.pmm pmm
#' @param y Vector to be imputed
#' @param ry Logical vector of length \code{length(y)} indicating the
#' the subset \code{y[ry]} of elements in \code{y} to which the imputation
#' model is fitted. The \code{ry} generally distinguishes the observed
#' (\code{TRUE}) and missing values (\code{FALSE}) in \code{y}.
#' @param x Numeric design matrix with \code{length(y)} rows with predictors for
#' \code{y}. Matrix \code{x} may have no missing values.
#' @param exclude Dependent values to exclude from the imputation model
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
#' (distance between drawn values).
#' @param ridge The ridge penalty used in \code{.norm.draw()} to prevent
#' problems with multicollinearity. The default is \code{ridge = 1e-05},
#' which means that 0.01 percent of the diagonal is added to the cross-product.
#' Larger ridges may result in more biased estimates. For highly noisy data
#' (e.g. many junk variables), set \code{ridge = 1e-06} or even lower to
#' reduce bias. For highly collinear data, set \code{ridge = 1e-04} or higher.
#' @param use.matcher Logical. Set \code{use.matcher = TRUE} to specify
#' the C function \code{matcher()}, the now deprecated matching function that
#' was default in versions
#' \code{2.22} (June 2014) to \code{3.11.7} (Oct 2020). Since version \code{3.12.0}
#' \code{mice()} uses the much faster \code{matchindex} C function. Use
#' the deprecated \code{matcher} function only for exact reproduction.
#' @param \dots Other named arguments.
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#' @author Gerko Vink, Stef van Buuren, Karin Groothuis-Oudshoorn
#' @details
#' Imputation of \code{y} by predictive mean matching, based on
#' van Buuren (2012, p. 73). The procedure is as follows:
#'
#' \enumerate{
#' \item{Calculate the cross-product matrix \eqn{S=X_{obs}'X_{obs}}.}
#' \item{Calculate \eqn{V = (S+{diag}(S)\kappa)^{-1}}, with some small ridge
#' parameter \eqn{\kappa}.}
#' \item{Calculate regression weights \eqn{\hat\beta = VX_{obs}'y_{obs}.}}
#' \item{Draw \eqn{q} independent \eqn{N(0,1)} variates in vector \eqn{\dot z_1}.}
#' \item{Calculate \eqn{V^{1/2}} by Cholesky decomposition.}
#' \item{Calculate \eqn{\dot\beta = \hat\beta + \dot\sigma\dot z_1 V^{1/2}}.}
#' \item{Calculate \eqn{\dot\eta(i,j)=|X_{{obs},[i]|}\hat\beta-X_{{mis},[j]}\dot\beta}
#' with \eqn{i=1,\dots,n_1} and \eqn{j=1,\dots,n_0}.}
#' \item{Construct \eqn{n_0} sets \eqn{Z_j}, each containing \eqn{d}
#' candidate donors, from \eqn{y_{obs}} such that \eqn{\sum_d\dot\eta(i,j)} is
#' minimum for all \eqn{j=1,\dots,n_0}. Break ties randomly.}
#' \item{Draw one donor \eqn{i_j} from \eqn{Z_j} randomly for \eqn{j=1,\dots,n_0}.}
#' \item{Calculate imputations \eqn{\dot y_j = y_{i_j}} for \eqn{j=1,\dots,n_0}.}
#' }
#'
#' The name \emph{predictive mean matching} was proposed by Little (1988).
#'
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
#' # We normally call mice.impute.pmm() from within mice()
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
#' yimp <- mice.impute.pmm(y, ry, x)
#' length(yimp)
#' hist(yimp, xlab = "Imputed missing tv")
#'
#' # Impute all tv data
#' yimp <- mice.impute.pmm(y, ry, x, wy = rep(TRUE, length(y)))
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
#' blots$tv$exclude <- c(1:5)
#' # Exclude 100 random observed heights from tv donor pool
#' blots$hgt$exclude <- sample(unique(boys$hgt), 100)
#' imp <- mice(boys, method = "pmm", print = FALSE, blots = blots, seed=123)
#' blots$hgt$exclude %in% unlist(c(imp$imp$hgt)) # MUST be all FALSE
#' blots$tv$exclude %in% unlist(c(imp$imp$tv)) # MUST be all FALSE
#'
#' # Factor quantification
#' xname <- c("age", "hgt", "wgt")
#' br <- boys[c(1:10, 101:110, 501:510, 601:620, 701:710), ]
#' r <- stats::complete.cases(br[, xname])
#' x <- br[r, xname]
#' y <- factor(br[r, "tv"])
#' ry <- !is.na(y)
#' table(y)
#'
#' # impute factor by optimizing canonical correlation y, x
#' mice.impute.pmm(y, ry, x)
#'
#' # only categories with at least 2 cases can be donor
#' mice.impute.pmm(y, ry, x, trim = 2L)
#'
#' # in addition, eliminate category 20
#' mice.impute.pmm(y, ry, x, trim = 2L, exclude = 20)
#'
#' # to get old behavior: as.integer(y))
#' mice.impute.pmm(y, ry, x, quantify = FALSE)
#' @export
mice.impute.pmm <- function(y, ry, x, wy = NULL, donors = 5L,
                            matchtype = 1L, exclude = NULL,
                            quantify = TRUE, trim = 1L,
                            ridge = 1e-05, use.matcher = FALSE, ...) {
  if (is.null(wy)) {
    wy <- !ry
  }

  # Reformulate the imputation problem such that
  # 1. the imputation model disregards records with excluded y-values
  # 2. the donor set does not contain excluded y-values

  # Keep sparse categories out of the imputation model
  if (is.factor(y)) {
    active <- !ry | y %in% (levels(y)[table(y) >= trim])
    y <- y[active]
    ry <- ry[active]
    x <- x[active, , drop = FALSE]
    wy <- wy[active]
  }
  # Keep excluded values out of the imputation model
  if (!is.null(exclude)) {
    active <- !ry | !y %in% exclude
    y <- y[active]
    ry <- ry[active]
    x <- x[active, , drop = FALSE]
    wy <- wy[active]
  }

  x <- cbind(1, as.matrix(x))

  # quantify categories for factors
  ynum <- y
  if (is.factor(y)) {
    if (quantify) {
      ynum <- quantify(y, ry, x)
    } else {
      ynum <- as.integer(y)
    }
  }

  # parameter estimation
  parm <- .norm.draw(ynum, ry, x, ridge = ridge, ...)

  if (matchtype == 0L) {
    yhatobs <- x[ry, , drop = FALSE] %*% parm$coef
    yhatmis <- x[wy, , drop = FALSE] %*% parm$coef
  }
  if (matchtype == 1L) {
    yhatobs <- x[ry, , drop = FALSE] %*% parm$coef
    yhatmis <- x[wy, , drop = FALSE] %*% parm$beta
  }
  if (matchtype == 2L) {
    yhatobs <- x[ry, , drop = FALSE] %*% parm$beta
    yhatmis <- x[wy, , drop = FALSE] %*% parm$beta
  }
  if (use.matcher) {
    idx <- matcher(yhatobs, yhatmis, k = donors)
  } else {
    idx <- matchindex(yhatobs, yhatmis, donors)
  }

  return(y[ry][idx])
}

#' Finds an imputed value from matches in the predictive metric (deprecated)
#'
#' This function finds matches among the observed data in the predictive
#' mean metric. It selects the \code{donors} closest matches, randomly
#' samples one of the donors, and returns the observed value of the
#' match.
#'
#' This function is included for backward compatibility. It was
#' used up to \code{mice 2.21}. The current \code{mice.impute.pmm()}
#' function calls the faster \code{C} function \code{matcher} instead of
#' \code{.pmm.match()}.
#'
#' @aliases .pmm.match
#' @param z A scalar containing the predicted value for the current case
#' to be imputed.
#' @param yhat A vector containing the predicted values for all cases with an observed
#' outcome.
#' @param y A vector of \code{length(yhat)} elements containing the observed outcome
#' @param donors The size of the donor pool among which a draw is made. The default is
#' \code{donors = 5}. Setting \code{donors = 1} always selects the closest match. Values
#' between 3 and 10 provide the best results. Note: This setting was changed from
#' 3 to 5 in version 2.19, based on simulation work by Tim Morris (UCL).
#' @param \dots Other parameters (not used).
#' @return A scalar containing the observed value of the selected donor.
#' @author Stef van Buuren
#' @rdname pmm.match
#' @references
#' Schenker N & Taylor JMG (1996) Partially parametric techniques
#' for multiple imputation. \emph{Computational Statistics and Data Analysis}, 22, 425-446.
#'
#' Little RJA (1988) Missing-data adjustments in large surveys (with discussion).
#' \emph{Journal of Business Economics and Statistics}, 6, 287-301.
#'
#' @export
.pmm.match <- function(z, yhat = yhat, y = y, donors = 5, ...) {
  d <- abs(yhat - z)
  f <- d > 0
  a1 <- ifelse(any(f), min(d[f]), 1)
  d <- d + runif(length(d), 0, a1 / 10^10)
  if (donors == 1) {
    return(y[which.min(d)])
  }
  donors <- min(donors, length(d))
  donors <- max(donors, 1)
  ds <- sort.int(d, partial = donors)
  m <- sample(y[d <= ds[donors]], 1)
  return(m)
}

quantify <- function(y, ry, x) {
  # replaces (reduced set of) categories by optimal scaling
  yf <- factor(y[ry], exclude = NULL)
  yd <- model.matrix(~ 0 + yf)
  xd <- x[ry, , drop = FALSE]
  cca <- cancor(yd, xd, xcenter = FALSE, ycenter = FALSE)
  ynum <- as.integer(y)
  ynum[ry] <- scale(as.vector(yd %*% cca$xcoef[, 2L]))
  # plot(y[ry], ynum[ry])
  return(ynum)
}
