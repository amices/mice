#' @title Approximate Least Angle Regression for correlation matrix
#'
#' @description
#' Finds the best predictors for dependent_var_index using an
#' approximate Least Angle Regression (LAR) algorithm applied
#' to the correlation matrix corr_matrix. Stops when the change
#' in R2 is less than crit, max_steps predictors have been
#' selected, or when R2 exceeds 1 (which can happen due to non-positivity).
#' @details
#' This is not a true LAR implementation, which requires a more
#' complex iterative process with residual updates. Instead, this
#' function uses the sweep operator to approximate the LAR algorithm,
#' and will produce slightly different R2 values, and no Mallow Cp values.
#'
#' The correlation matrix corr_matrix should be symmetric,
#' positive definite, and contain no missing data. If not, the results
#' may be incorrect, but still useful.
#'
#' If you generate the correlation matrix from an incomplete dataset
#' e.g by \code{cor(data, use = "pairwise.complete.obs")} the matrix may not
#' be positive semidefinite. Try \code{force_pf = TRUE} to ensure
#' positive definiteness, though this may introduce other problems
#' like diagonal values unequal to 1.
#'
#' If two variables have never been jointly observed in the data, it is
#' not possible to calculate the correlation. In that case, imputing
#' missing correlations by 0 neutralizes their effect on variable selection.
#'
#' The procedure may produce inflated R2 when the correlation is not a good
#' description of the relationship between the variables. The example
#' section shows a case where the correlation between a missingness
#' indicator and the original data is high, yet has low predictive power.
#' The current advice is not to include missingness indicators in the
#' correlation matrix.
#' @param corr_matrix A symmetric correlation matrix (no data frame).
#' @param dependent_var_index The location in \code{corr_matrix} indicating the
#' dependent variable.
#' @param max_steps The maximum number of predictors to select. If NULL, all
#' predictors are selected.
#' @param crit The minimum change in R^2 to stop the algorithm.
#' @param force_pd Logical. If TRUE, forces the correlation matrix to be
#' positive definite.
#' @param check_input Logical. If TRUE, checks the input for validity.
#' @return A list with two elements:
#' \item{predictors}{The indices of the selected predictors.}
#' \item{R2}{The R^2 value for each step.}
#' @author Stef van Buuren, June 2024
#' @examples
#' # Find predictors for genital state
#' names(boys)
#' cor1 <- cor(data.matrix(boys), use = "pair")
#' correlar(cor1, dependent = 6)
#'
#' # Limit to three predictors
#' correlar(cor1, 6, max_steps = 3)
#'
#' # Problem case: Misleading correlation calculated from selective subset
#' # The correlation r(age, is.na(gen)) of -0.5 suggest strong predictive
#' # power and an R2 of 0.25. The actual R2 is only 0.03.
#' plot(y = is.na(boys$gen), x = boys$hgt)
#' cor2 <- cor(data.matrix(cbind(is.na(boys$gen), boys)), use = "pair")
#' cor2[is.na(cor2)] <- 0
#' correlar(cor2, 1)
#' @export
correlar <- function(corr_matrix,
                     dependent_var_index,
                     max_steps = NULL,
                     crit = 0.005,
                     force_pd = FALSE,
                     check_input = TRUE) {

  if (check_input) {
    stopifnot(
      is.matrix(corr_matrix),
      isTRUE(all.equal(corr_matrix, t(corr_matrix))),
      all(!is.na(corr_matrix)),
      is.numeric(dependent_var_index) && length(dependent_var_index) == 1L
    )
  }

  if (force_pd) {
    corr_matrix <- Matrix::nearPD(corr_matrix)$mat
  }

  n_vars <- ncol(corr_matrix)
  remaining_predictors <- setdiff(1L:n_vars, dependent_var_index)

  max_steps <- min(max_steps, n_vars - 1L)
  predictors <- integer(max_steps)
  R2 <- double(max_steps)

  current_matrix <- corr_matrix
  steps <- 0L
  converged <- FALSE

  while (length(remaining_predictors) > 0L &&
         (is.null(max_steps) || steps < max_steps) &&
         !converged) {

    # Find the predictor with the highest absolute correlation
    correlations <- current_matrix[dependent_var_index, remaining_predictors]
    best_predictor <- remaining_predictors[which.max(abs(correlations))]
    remaining_predictors <- setdiff(remaining_predictors, best_predictor)

    # Sweep out the best predictor
    current_matrix <- sweep_operator(current_matrix, best_predictor)

    # Store results
    steps <- steps + 1L
    predictors[steps] <- best_predictor
    R2[steps] <- 1 - current_matrix[dependent_var_index, dependent_var_index]

    # stop if criterion is met
    if (steps > 2L && R2[steps] - R2[steps - 1L] < crit) {
      converged <- TRUE
    }

    # stop if R2 is greater than 1
    if (R2[steps] > 1) {
      steps <- max(steps - 1L, 1L)
      converged <- TRUE
    }
  }

  return(list(
    predictors = predictors[seq(steps)],
    R2 = R2[seq(steps)]))
}
