#' Imputation by predictive mean matching
#'
#'
#' \code{pmmsplit()} is an implementation of pmm that saves the imputation model
#' and generates imputations from the saved model.
#' @aliases pmmsplit
#' @param activity The activity to be performed. The default is \code{"walk"}.
#' @param model Storage for the model estimates
#' @param nbins The number of bins used to store the predictive mean matching
#' model. The default is 50.
#' @inheritParams mice.impute.pmm
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#' @author Stef van Buuren
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
#' # We normally call mice.impute.pmmsplit() from within mice()
#' # But we may call it directly as follows (not recommended)
#'
#' set.seed(53177)
#' xname <- c("age", "hgt", "wgt")
#' r <- stats::complete.cases(boys[, xname])
#' x <- as.matrix(boys[r, xname])
#' y <- boys[r, "tv"]
#' ry <- !is.na(y)
#' table(ry)
#'
#' # percentage of missing data in tv
#' sum(!ry) / length(ry)
#'
#' # Impute missing tv data
#' yimp <- mice.impute.pmmsplit(y, ry, x)
#' length(yimp)
#' hist(yimp, xlab = "Imputed missing tv")
#'
#' # Impute all tv data
#' yimp <- mice.impute.pmmsplit(y, ry, x, wy = rep(TRUE, length(y)))
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
#' imp <- mice(boys, method = "pmmsplit", print = FALSE, blots = blots, seed=123)
#' blots$hgt$exclude %in% unlist(c(imp$imp$hgt)) # MUST be all FALSE
#' blots$tv$exclude %in% unlist(c(imp$imp$tv)) # MUST be all FALSE
#'
#' # Factor quantification
#' xname <- c("age", "hgt", "wgt")
#' br <- boys[c(1:10, 101:110, 501:510, 601:620, 701:710), ]
#' r <- stats::complete.cases(br[, xname])
#' x <- as.matrix(br[r, xname])
#' y <- factor(br[r, "tv"])
#' ry <- !is.na(y)
#' table(y)
#'
#' # impute factor by optimizing canonical correlation y, x
#' mice.impute.pmmsplit(y, ry, x)
#'
#' # only categories with at least 2 cases can be donor
#' mice.impute.pmmsplit(y, ry, x, trim = 2L)
#'
#' # in addition, eliminate category 20
#' mice.impute.pmmsplit(y, ry, x, trim = 2L, exclude = 20)
#'
#' # to get old behavior: as.integer(y))
#' mice.impute.pmmsplit(y, ry, x, quantify = FALSE)
#' @export
mice.impute.pmmsplit <- function(y, ry, x, wy = NULL, donors = NULL,
                                 matchtype = 1L, exclude = NULL,
                                 quantify = TRUE, trim = 1L,
                                 ridge = 1e-05, nbins = NULL,
                                 activity = "walk",
                                 model = NULL, ...) {
  if (is.null(wy)) {
    wy <- !ry
  }

  # **Only enforce `model` for "train" and "run"**
  if (activity %in% c("train", "run")) {
    if (is.null(model)) {
      stop(paste("`model` cannot be NULL for activity:", activity))
    }
    if (!is.environment(model)) {
      stop("`model` must be an environment to store results persistently.")
    }
  }

  # **Handle "run" activity: Use Pre-Stored Model Without Re-Training**
  if (activity == "run") {
    if (!length(ls(model))) {
      stop("No stored model found for 'fill' activity.")
    }

    # Compute linear predictor for missing data
    yhatmis <- x[wy, , drop = FALSE] %*% model$beta.mis
    impy <- draw.neighbors.pmm(yhatmis,
                               edges = model$edges,
                               lookup = model$lookup,
                               m = 1L)

    # Convert back to factor if needed
    impy <- unquantify(impy, y)
    return(impy)
  }

  # **Handle "walk" and "train": Train Model**
  ynum <- quantify(y, ry, x, quantify = quantify)

  # Predicted values for observed part
  parm <- .norm.draw(ynum, ry, x, ridge = ridge, ...)
  if (matchtype == 0L) {
    beta.mis <- beta.obs <- parm$coef
  }
  if (matchtype == 1L) {
    beta.obs <- parm$coef
    beta.mis <- parm$beta
  }
  if (matchtype == 2L) {
    beta.mis <- beta.obs <- parm$beta
  }
  yhatobs <- as.vector(x[ry, , drop = FALSE] %*% beta.obs)

  # Divide predictions into bins
  nbins <- initialize.nbins(nbins, length(yhatobs), length(unique(yhatobs)))
  donors <- initialize.donors(donors, length(yhatobs))
  prep <- bin.yhat(yhatobs, ynum[ry], k = donors, nbins = nbins)

  # **Store Model for "train" (skip for "walk")**
  if (activity == "train") {
    model$setup <- list(method = "pmmsplit",
                        donors = donors,
                        nbins = nbins,
                        matchtype = matchtype,
                        exclude = exclude,
                        quantify = quantify,
                        trim = trim,
                        ridge = ridge)
    model$beta.obs <- beta.obs
    model$beta.mis <- beta.mis
    model$edges <- prep$edges
    model$lookup <- prep$lookup
  }

  # **Compute Missing Value Imputations**
  yhatmis <- x[wy, , drop = FALSE] %*% beta.mis
  impy <- draw.neighbors.pmm(yhatmis,
                             edges = prep$edges,
                             lookup = prep$lookup,
                             m = 1L)

  # Convert back to factor if needed
  impy <- unquantify(impy, y)
  return(impy)
}

initialize.nbins <- function(nbins, n, nu) {
  if (is.null(nbins)) {
    nbins <- round(4 * log(n) + 1.5)
  }

  # max nbin is number of unique yhat values
  if (nbins > nu) {
    # message("Warning: nbins (", nbins, ") exceeds unique yhat values (", nu, "). Adjusting to ", nu, ".")
    nbins <- nu
  }
  # Ensure at least 2 bins
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

bin.yhat <- function(yhat, y, k = 10, nbins = 25) {
  stopifnot(length(yhat) == length(y))

  # Compute percentile-based bin edges
  edges <- quantile(yhat, probs = seq(0, 1, length.out = nbins + 1), type = 7, na.rm = TRUE)

  # Sort yhat and y together
  sort_order <- order(yhat)
  yhat_sorted <- yhat[sort_order]
  y_sorted <- y[sort_order]

  # Initialize lookup table
  lookup <- matrix(NA_real_, nrow = nbins, ncol = k)

  # Assign values to bins
  bin_idx <- findInterval(yhat_sorted, vec = edges, all.inside = TRUE)

  # Split y_sorted by bins
  bin_values_list <- split(y_sorted, bin_idx)

  # Fill lookup table
  lookup <- t(sapply(seq_len(nbins), function(b) {
    bin_values <- bin_values_list[[as.character(b)]]

    if (length(bin_values) > 0) {
      # If more than k values are available, randomly sample k
      sample(bin_values, size = k, replace = length(bin_values) < k)
    } else {
      # If bin is empty, sample from entire y_sorted to avoid NA values
      sample(y_sorted, size = k, replace = TRUE)
    }
  }))

  return(list(edges = edges, lookup = lookup))
}

draw.neighbors.pmm <- function(yhat_query, edges, lookup, m = 1) {
  num_queries <- length(yhat_query)
  nbins <- length(edges) - 1  # Bins are defined by edges[i] and edges[i+1]

  # Initialize result matrix: rows = number of queries, columns = m draws per query
  imputed_values <- matrix(NA_real_, nrow = num_queries, ncol = m)

  # Find the bin for each query value
  bin_idx <- findInterval(yhat_query, edges, rightmost.closed = TRUE, all.inside = TRUE)

  # Compute probability of selecting from left bin (smooth transition)
  t0 <- edges[pmax(bin_idx, 1)]
  t1 <- edges[pmin(bin_idx + 1, nbins)]
  p_left <- ifelse(t1 > t0, (t1 - yhat_query) / (t1 - t0), 0.5)

  # Determine which bin to sample from
  selected_bin <- ifelse(runif(num_queries) < p_left, bin_idx, pmin(bin_idx + 1, nbins))

  # Vectorized sampling from lookup table
  sampled_indices <- matrix(sample(1:ncol(lookup), num_queries * m, replace = TRUE), nrow = num_queries)
  imputed_values <- matrix(lookup[cbind(selected_bin, sampled_indices)], nrow = num_queries, ncol = m)

  return(imputed_values)
}

quantify <- function(y, ry, x, quantify = TRUE) {
  if (!is.factor(y)) {
    return(y)
  }
  if (!quantify) {
    return(as.integer(y))
  }

  # replace (reduced set of) categories by optimal scaling
  yf <- factor(y[ry], exclude = NULL)
  yd <- model.matrix(~ 0 + yf)
  xd <- x[ry, , drop = FALSE]
  cca <- cancor(yd, xd, xcenter = FALSE, ycenter = FALSE)
  # NOTE: We must be more careful if imputations from a previous iteration
  # need to be processed. The line below is a quick fix.
  ynum <- as.integer(y)
  # Scale only observed y's, since these are used to predict missing y's
  ynum[ry] <- scale(as.vector(yd %*% cca$xcoef[, 2L]))
  return(ynum)
}

unquantify <- function(ynum, original_y, quantify = TRUE) {
  if (!is.factor(original_y)) return(ynum)
  factor_levels <- levels(original_y)
  if (!is.numeric(ynum)) stop("ynum must be numeric")

  if (quantify) {
    # Get unique numeric values and their corresponding factor levels in original y
    unique_ynum <- unique(ynum)
    unique_levels <- unique(original_y)

    # Ensure levels are assigned in the same order as original_y
    mapping <- setNames(as.character(unique_levels), unique_ynum)

    # Assign factor levels based on the mapping
    reconstructed_y <- factor(mapping[as.character(ynum)], levels = factor_levels)
  } else {
    # Reverse integer encoding (simple conversion)
    reconstructed_y <- factor(factor_levels[ynum], levels = factor_levels)
  }

  return(unname(reconstructed_y))
}
