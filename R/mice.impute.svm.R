#' Imputation by the Support Vector Machine (mice-SVM)
#'
#' This function performs proper multiple imputation using the Support Vector Machine (SVM) 
#' combined with bootstrapping, as proposed by Takahashi (2026). It is specifically 
#' designed for binary variables in high-dimensional data settings.
#' 
#' Note: When using this method in high-dimensional settings, it is recommended 
#' to set \code{eps = 0} in the \code{mice()} call to prevent \code{remove.lindep()} 
#' from removing predictors before they reach the SVM model.
#'
#' @inheritParams mice.impute.pmm
#' @param type A vector of length \code{ncol(x)} identifying the predictors. 
#' Captured here to avoid conflicts with the SVM 'type' argument from mice's internal calls.
#' @param C Cost of constraints violation (default = 1).
#' @param scaled A logical vector indicating the variables to be scaled.
#' @param kernel The kernel function used in training and predicting (default = "vanilladot").
#' @param tol Tolerance of termination criterion (default = 0.001).
#' @param kpar List of hyper-parameters for the kernel function (default = list()).
#' @param ... Other named arguments to be passed to \code{kernlab::ksvm()}.
#' @return A vector of length \code{sum(!ry)} with imputed values.
#' @references 
#' Takahashi, M. (2026). Multiple Imputation based on the Support Vector Machine for 
#' High-Dimensional Data with General Missing Patterns in Causal Inference. 
#' Journal of Statistical Computation and Simulation.
#' @export
mice.impute.svm <- function(y, ry, x, wy = NULL, type = NULL, C = 1, scaled = TRUE, kernel = "vanilladot", tol = 0.001, kpar = list(), ...) {
  
  if (!requireNamespace("kernlab", quietly = TRUE)) {
    stop("Package 'kernlab' is needed for this function. Please install it.")
  }
  
  if (is.null(wy)) wy <- !ry
  n_target <- sum(wy)
  
  # 1. Bootstrap for estimation uncertainty (Takahashi, 2026, Section 3.3, Steps 1-2)
  xobs <- x[ry, , drop = FALSE]
  yobs <- y[ry]
  n1 <- sum(ry)
  s <- sample(n1, n1, replace = TRUE)
  
  # The model must be trained on the bootstrapped observed data (y*, X*)
  y_star <- yobs[s]
  x_star <- xobs[s, , drop = FALSE]
  
  # Initialize draw with NAs
  draw <- rep(NA, n_target)
  
  # 2. SVM Model Training (Takahashi, 2026, Section 3.3, Step 3)
  if (length(unique(y_star)) == 2) {
    result <- tryCatch({
      svm.model <- NULL
      utils::capture.output(
        svm.model <- suppressWarnings(
          suppressMessages(
            kernlab::ksvm(
              y_star ~ x_star, 
              type = "C-svc", 
              kernel = kernel, 
              cross = 0, 
              C = C, 
              scaled = scaled, 
              prob.model = TRUE, 
              tol = tol,
              kpar = kpar,
              ...
            )
          )
        )
      )
      
  # 3. Predict probabilities for fundamental uncertainty (Takahashi, 2026, Section 3.3, Step 4)
      p_mat <- NULL
      utils::capture.output(
        p_mat <- suppressWarnings(
          suppressMessages(
            kernlab::predict(svm.model, x[wy, , drop = FALSE], type = "probabilities")
          )
        )
      )
      
  # 4. Stochastic drawing (Takahashi, 2026, Section 3.3, Step 5)
      # Extract probabilities for the positive class (assumed to be the 2nd column).
      p <- p_mat[, 2]
      as.integer(runif(length(p)) <= p)
      
    }, error = function(e) {
      NULL # Return NULL to trigger fallback on numerical or logical errors
    })
    
    if (!is.null(result)) {
      draw <- result
    }
  }
  
  # --- FALLBACK: If SVM failed or bootstrap sample had only 1 class ---
  if (any(is.na(draw))) {
    n_miss <- sum(is.na(draw))
    # Standard fallback: simple random sampling from observed values.
    y_fill <- sample(yobs, n_miss, replace = TRUE)
    
    if (is.factor(y)) {
      # Align levels with the original factor coding (0-based for integer drawing).
      draw[is.na(draw)] <- as.integer(factor(y_fill, levels = levels(y))) - 1
    } else {
      draw[is.na(draw)] <- y_fill
    }
  }
  
  # 5. Final type adjustment for the mice environment.
  if (is.factor(y)) {
    res <- factor(draw, levels = c(0, 1), labels = levels(y))
  } else {
    res <- draw
  }
  
  return(res)
}