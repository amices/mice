#' Imputation by predictive mean matching
#'
#'
#' \code{pmmsplit()} is an implementation of pmm that saves the imputation model
#' and generates imputations from the saved model.
#' @aliases pmmsplit
#' @param num_bins The number of bins used to store the predictive mean matching
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
#' x <- boys[r, xname]
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
#' x <- br[r, xname]
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
mice.impute.pmmsplit <- function(y, ry, x, wy = NULL, donors = 10L,
                                 matchtype = 1L, exclude = NULL,
                                 quantify = TRUE, trim = 1L,
                                 ridge = 1e-05,
                                 num_bins = 50, ...) {
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

  # train the imputation model using the data
  # parameter estimation
  parm <- .norm.draw(ynum, ry, x, ridge = ridge, ...)

  if (matchtype %in% c(0L, 1L)) {
    yhatobs <- x[ry, , drop = FALSE] %*% parm$coef
  } else {
    yhatobs <- x[ry, , drop = FALSE] %*% parm$beta
  }
  yhatobs <- as.vector(yhatobs)

  # save predictive beta, bin_edge, and lookup table
  stored <- preprocess_yhat(yhatobs, ynum[ry], k = donors, num_bins = num_bins)
  stored$beta <- parm$beta
  if (matchtype == 0L) {
    stored$beta <- parm$coef
  }

  # impute the missing data
  if (matchtype == 0L) {
    yhatmis <- x[wy, , drop = FALSE] %*% stored$beta
  } else {
    yhatmis <- x[wy, , drop = FALSE] %*% stored$beta
  }

  impy <- draw_neighbors_pmm(yhatmis,
                             bin_edges = stored$bin_edges,
                             lookup_table = stored$lookup_table,
                             m = 1)
  # convert back to factor
  impy <- unquantify(impy, y)

  return(impy)
  # idx <- matchindex(yhatobs, yhatmis, donors)
  # return(y[ry][idx])
}


preprocess_yhat <- function(yhat, y, k = 10, num_bins = 50) {
  stopifnot(length(yhat) == length(y))

  # Ensure valid k
  n <- length(yhat)
  k <- max(1, min(k, n))  # Clamp k between 1 and n

  # Determine unique yhat values and adjust num_bins accordingly
  unique_yhat <- unique(yhat)
  num_unique <- length(unique_yhat)

  if (num_bins > num_unique) {
  #  message("Warning: num_bins (", num_bins, ") exceeds unique yhat values (", num_unique, "). Adjusting to ", num_unique, ".")
    num_bins <- num_unique
  }
  num_bins <- max(2, num_bins)  # Ensure at least 2 bins

  # Compute percentile-based bin edges
  bin_edges <- quantile(yhat, probs = seq(0, 1, length.out = num_bins + 1), type = 7, na.rm = TRUE)

  # Sort yhat and y together
  sort_order <- order(yhat)
  yhat_sorted <- yhat[sort_order]
  y_sorted <- y[sort_order]

  # Initialize lookup table
  lookup_table <- matrix(NA_real_, nrow = num_bins, ncol = k)

  # Assign values to bins
  bin_idx <- findInterval(yhat_sorted, vec = bin_edges, all.inside = TRUE)

  # Split y_sorted by bins
  bin_values_list <- split(y_sorted, bin_idx)

  # Fill lookup table
  lookup_table <- t(sapply(seq_len(num_bins), function(b) {
    bin_values <- bin_values_list[[as.character(b)]]

    if (length(bin_values) > 0) {
      # If more than k values are available, randomly sample k
      sample(bin_values, size = k, replace = length(bin_values) < k)
    } else {
      # If bin is empty, sample from entire y_sorted to avoid NA values
      sample(y_sorted, size = k, replace = TRUE)
    }
  }))

  return(list(bin_edges = bin_edges, lookup_table = lookup_table))
}

draw_neighbors_pmm <- function(yhat_query, bin_edges, lookup_table, m = 1) {
  num_queries <- length(yhat_query)
  num_bins <- length(bin_edges) - 1  # Bins are defined by edges[i] and edges[i+1]

  # Initialize result matrix: rows = number of queries, columns = m draws per query
  imputed_values <- matrix(NA_real_, nrow = num_queries, ncol = m)

  # Find the bin for each query value
  bin_idx <- findInterval(yhat_query, bin_edges, rightmost.closed = TRUE, all.inside = TRUE)

  # Compute probability of selecting from left bin (smooth transition)
  t0 <- bin_edges[pmax(bin_idx, 1)]
  t1 <- bin_edges[pmin(bin_idx + 1, num_bins)]
  p_left <- ifelse(t1 > t0, (t1 - yhat_query) / (t1 - t0), 0.5)

  # Determine which bin to sample from
  selected_bin <- ifelse(runif(num_queries) < p_left, bin_idx, pmin(bin_idx + 1, num_bins))

  # Vectorized sampling from lookup table
  sampled_indices <- matrix(sample(1:ncol(lookup_table), num_queries * m, replace = TRUE), nrow = num_queries)
  imputed_values <- matrix(lookup_table[cbind(selected_bin, sampled_indices)], nrow = num_queries, ncol = m)

  return(imputed_values)
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