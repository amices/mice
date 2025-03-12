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
#' @param wy Logical vector of length \code{length(y)}. A \code{TRUE} value
#' indicates locations in \code{y} for which imputations are created.
#' @param donors The size of the donor pool among which a draw is made.
#' The default is \code{donors = 5L}. Setting \code{donors = 1L} always selects
#' the closest match, but is not recommended. Values between 5L and 10L
#' provide the best results (Morris et al, 2015).
#' For task \code{"train"}, the number of donors
#' is calculated internally based on the number of
#' observations in \code{yobs}.
#' @param matchtype Type of matching distance. The default (recommended) choice
#' (\code{matchtype = 1L}) calculates the distance between
#' the \emph{predicted} value of \code{yobs} and
#' the \emph{drawn} values of \code{ymis} (called type-1 matching).
#' Other choices are \code{matchtype = 0L}
#' (distance between predicted values) and \code{matchtype = 2L}
#' (distance between drawn values).
#' @param quantify Logical. If \code{TRUE}, factor levels are replaced
#' by the first canonical variate before fitting the imputation model.
#' If false, the procedure reverts to the old behaviour and takes the
#' integer codes (which may lack a sensible interpretation).
#' Relevant only of \code{y} is a factor.
#' @param exclude Dependent values to exclude from the imputation model
#' and the collection of donor values
#' @param trim Scalar integer. Minimum number of observations required in a
#' category in order to be considered as a potential donor value.
#' Relevant only of \code{y} is a factor.
#' @param task Character string. The task to be performed. Can
#' be \code{"impute"}, \code{"train"} or \code{"fill"}.
#' The default is \code{"impute"} (classic MICE). See \code{mice()} for
#' details.
#' @param model An environment created by a parent to store the imputation
#' model setup and estimates. The model is stored in the \code{mids} object
#' under tasks \code{"train"}, and is needed as input
#' for task \code{"fill"}. The object \code{model} is not used under
#' task \code{"impute"}.
#' @param nimp Experimental. Number of random imputations per missing values
#' generated from a fitted model under task \code{"train"}.
#' The default is 1. The \code{nimp} parameter is different from \code{m},
#' the number of multiple imputations, because it generates repeated
#' imputations from a single model. The \code{nimp} parameter is useful
#' for large samples to reduce the computational burden, but still awaits
#' support within the mice algorithm.
#' @param nbins The number of bins used to store the predictive mean matching
#' model. Under task \code{"train"}, the number of donors
#' is calculated internally based on the number of observations in \code{yobs}
#' and the number of unique predictive values.
#' @param ridge The ridge penalty used in \code{.norm.draw()} to prevent
#' problems with multicollinearity. The default is \code{ridge = 1e-05},
#' which means that 0.01 percent of the diagonal is added to the cross-product.
#' Larger ridges may result in more biased estimates. For highly noisy data
#' (e.g. many junk variables), set \code{ridge = 1e-06} or even lower to
#' reduce bias. For highly collinear data, set \code{ridge = 1e-04} or higher.
#' @param use.matcher Logical. Set \code{use.matcher = TRUE} to specify
#' the C function \code{matcher()}, the now deprecated matching function that
#' was default in versions of \code{mice} prior to \code{3.12.0}.
#' @param \dots Other named arguments.
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#' @author Stef van Buuren
#' @details
#' Imputation of \code{y} by predictive mean matching, based on
#' van Buuren (2012, p. 73). The procedure is as follows:
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
mice.impute.pmm <- function(y, ry, x, wy = NULL,
                            donors = 5L, matchtype = 1L, quantify = TRUE,
                            exclude = NULL, trim = 1L,
                            task = "impute", model = NULL, nimp = 1L,
                            nbins = NULL, ridge = 1e-05, use.matcher = FALSE,
                            ...)
{
  check.model(model, task)
  if (is.null(wy)) wy <- !ry

  # Remove excluded values and trim small categories
  if (is.factor(y)) {
    freq <- table(y)
    keep_levels <- names(freq[freq >= trim & !(names(freq) %in% exclude)])
    y <- factor(y, levels = keep_levels)
    idx <- !ry | y %in% keep_levels
  } else {
    idx <- !ry | !(y %in% exclude)
  }
  if (any(!idx)) {
    y <- y[idx]
    ry <- ry[idx]
    x <- x[idx, , drop = FALSE]
    wy <- wy[idx]
  }

  # Add intercept column to x
  x <- cbind(1, as.matrix(x))

  # >>> Fill task: fill from stored model
  if (task == "fill") {
    formula <- model$formula
    if (!length(formula)) {
      stop("No model stored in environment")
    }
    mnames <- rownames(model$beta.mis)
    dnames <- colnames(x)
    if (ncol(x) != nrow(model$beta.mis) || any(mnames != dnames)) {
      stop(paste("Model-Data mismatch: ", deparse(formula), "\n",
                 " Model:", paste(mnames, collapse = " "), "\n",
                 " Data: ", paste(dnames, collapse = " "), "\n"))
    }
    yhatmis <- x[wy, ] %*% model$beta.mis
    impy <- draw.neighbors.pmm(yhatmis,
                               edges = model$edges,
                               lookup = model$lookup,
                               nimp = nimp)
    return(impy)
  }

  # Quantify factor levels
  f <- quantify(y, ry, x, quantify = quantify)
  ynum <- f$ynum

  # Predict ynum on observed data with linear model
  parm <- .norm.draw(ynum, ry, x, ridge = ridge, ...)
  if (matchtype == 1L) {
    beta.obs <- parm$coef
    beta.mis <- parm$beta
  } else if (matchtype == 0L) {
    beta.mis <- beta.obs <- parm$coef
  } else if (matchtype == 2L) {
    beta.mis <- beta.obs <- parm$beta
  }
  x_ry <- x[ry, , drop = FALSE]
  x_wy <- x[wy, , drop = FALSE]
  yhatobs <- as.vector(x_ry %*% beta.obs)
  yhatmis <- x_wy %*% beta.mis

  # >>> Impute task: Impute values (classic MICE PMM)
  if (task == "impute") {
    if (use.matcher) {
      idx <- matcher(yhatobs, yhatmis, k = donors)
    } else {
      idx <- matchindex(yhatobs, yhatmis, donors)
    }
    return(y[ry][idx])
  }

  # >>> Train task: Store model in environment
  nbins <- initialize.nbins(nbins, length(yhatobs), length(unique(yhatobs)))
  donors <- initialize.donors(donors, length(yhatobs))
  edges <- quantile(yhatobs, probs = seq(0, 1, length.out = nbins + 1L),
                    type = 7L, na.rm = TRUE)
  lookup <- bin.yhat(yhatobs, ynum[ry], k = donors, edges = edges)

  # Store the imputation model in models environment
  model$setup <- list(method = "pmm",
                      n = length(yhatobs),
                      donors = donors,
                      matchtype = matchtype,
                      quantify = quantify,
                      exclude = exclude,
                      trim = trim,
                      task = task,
                      nbins = nbins,
                      ridge = ridge)
  model$beta.obs <- beta.obs
  model$beta.mis <- beta.mis
  model$edges <- edges
  model$lookup <- matrix((unquantify(lookup, f$quant, levels(y))),
                         nrow = nbins)
  model$factor <- list(labels = f$labels, quant = f$quant)

  # Compute imputations from model
  impy <- draw.neighbors.pmm(yhatmis,
                             edges = model$edges,
                             lookup = model$lookup,
                             nimp = nimp)
  return(impy)
}

# --- PMM helpers

initialize.nbins <- function(nbins, n, nu) {
  if (is.null(nbins)) {
    nbins <- round(4 * log(n) + 1.5)
  }

  # max nbin is number of unique yhat values, but ensure at least 2 bins
  if (nbins > nu) {
    nbins <- nu
  }
  nbins <- max(2L, nbins)
  return(nbins)
}

initialize.donors <- function(donors, n) {
  if (is.null(donors)) {
    donors <- round(n / 600 + 7)
  }
  donors <- max(1L, min(donors, n))
  return(donors)
}

bin.yhat <- function(yhat, y, k, edges) {
  stopifnot(length(yhat) == length(y))

  # Sort yhat and y together
  sort_order <- order(yhat)
  yhat_sorted <- yhat[sort_order]
  y_sorted <- y[sort_order]

  # Assign values to bins
  bin <- findInterval(yhat_sorted, vec = edges, all.inside = TRUE)

  # Split y_sorted by bins
  values_list <- split(y_sorted, bin)

  # Fill lookup table with k potential donor values per bin
  # If bin is empty, sample from entire y_sorted to avoid NA values
  # If only one value is available, repeat it
  # Watch out for odd sample(x) behavior when length(x) == 1
  # Sample with replacement if we fewer than k bin values
  nbins <- length(edges) - 1L
  lookup <- t(sapply(seq_len(nbins), function(b) {
    values <- values_list[[as.character(b)]]
    if (length(values) == 0L) {
      sample(y_sorted, size = k, replace = TRUE)
    } else if (length(values) == 1L) {
      rep(values, k)
    } else {
      sample(values, size = k, replace = length(values) < k)
    }}))

  return(lookup)
}

draw.neighbors.pmm <- function(yhat, edges, lookup, nimp = 1L) {
  # Bins are defined by edges[i] and edges[i+1]
  n <- length(yhat)
  nbins <- length(edges) - 1L

  # Result matrix: rows = number of queries, columns = nimp draws per query
  imputed_values <- matrix(NA_real_, nrow = n, ncol = nimp)

  # Find the bin for each query value
  bin <- findInterval(yhat, edges, rightmost.closed = TRUE, all.inside = TRUE)

  # Compute probability of selecting from left bin (smooth transition)
  t0 <- edges[pmax(bin, 1L)]
  t1 <- edges[pmin(bin + 1L, nbins)]
  p_left <- ifelse(t1 > t0, (t1 - yhat) / (t1 - t0), 0.5)

  # Determine which bin to sample from
  selected_bin <- ifelse(runif(n) < p_left, bin, pmin(bin + 1L, nbins))

  # Vectorized sampling from lookup table
  indices <- matrix(sample(1L:ncol(lookup), n * nimp, replace = TRUE), nrow = n)
  impy <- matrix(lookup[cbind(selected_bin, indices)], nrow = n, ncol = nimp)
  return(impy)
}

