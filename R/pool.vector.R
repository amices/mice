#' Combines estimates from a vector of parameters
#'
#' @param w A \code{data.frame} with parameter estimates
#' in tidy format (see details).
#' @param dfcom A positive number representing the degrees of freedom of the
#' residuals in the complete-data analysis. The \code{dfcom} argument is
#' used for the Barnard-Rubin adjustment. In a linear regression, \code{dfcom}
#' would be equivalent to the number of independent observation minus the number
#' of fitted parameters, but the expression becomes more complex for
#' regularized, proportional hazards, or other semi-parametric
#' techniques. Only used if \code{w} lacks a column named \code{"df.residual"}.
#' The default \code{dfcom = Inf} is appropriate for large samples
#' (n > 1000) and relatively concise parametric models.
#' @param rule A string indicating the pooling rule. Currently supported are
#' \code{"rubin1987"} (default, for analyses applied to multiply-imputed
#' incomplete data) and \code{"reiter2003"} (for analyses applied to
#' synthetic data created from complete data).
#' @param custom.t A custom character string to be parsed as a calculation
#' rule for the total variance \code{t}. The custom rule can use the
#' other calculated pooling statistics. The default \code{t} calculation
#' has the form \code{".data$ubar + (1 + 1 / .data$m) * .data$b"}.
#' See examples for an example.
#' @details
#' The input data \code{w} is a \code{data.frame} with columns named:
#'
#' 1. `term`, a character or factor with the parameter names
#' 2. `estimate`, a numeric vector with parameter estimates
#' 3. `std.error`, a numeric vector with standard errors of `estimate`
#' 4. `residual.df`, a numeric vector with the degrees of freedom
#'
#' Columns 1-3 are obligatory. Column 4 is optional. Usually,
#' all entries in column 4 are the same. The user can omit column 4,
#' and specify `dfcom` instead. If both are specified, then column
#' `residual.df` takes precedence. If neither are specified, then
#' `mice` tries to calculate the residual degrees of freedom.
#'
#' @return A data.frame with aggregated parameters estimates,
#' standard errors, confidence intervals and statistical tests.
#'
#' @examples
#' # conventional mice workflow
#' imp <- mice(nhanes2, m = 2, maxit = 2, seed = 1, print = FALSE)
#' fit <- with(imp, lm(chl ~ age + bmi + hyp))
#' est1 <- pool(fit)$pooled
#' est1
#'
#' # using pool.vector on tidy parameters
#' par <- summary(fit)[, c("term", "estimate", "std.error", "df.residual")]
#' est2 <- pool.vector(par)
#' est2
#'
#' identical(est1, est2)
#' @export
pool.vector <- function(w, dfcom = Inf, custom.t = NULL,
                        rule = c("rubin1987", "reiter2003")) {
  # rubin1987: Rubin's rules for scalar estimates
  # reiter2003: Reiter's rules for partially synthetic data
  rule <- match.arg(rule)

  present <- hasName(w, c("estimate", "std.error"))
  if (!all(present)) {
    stop("Column(s) not found: ",
         paste(c("estimate", "std.error")[!present], collapse = ", "))
  }
  grp <- intersect(names(w),
                   c("term", "parameter", "contrast", "y.level", "component"))
  if (!length(grp)) {
    warning("No parameter names found. Add a column named `term`.")
  }
  dfcom <- ifelse(hasName(w, "df.residual"), w[["df.residual"]], dfcom)
  if (is.infinite(dfcom)) {
    warning("Large sample assumed.")
  }

  # Convert to factor to preserve ordering
  if (hasName(w, "term"))
    w$term <- factor(w$term, levels = unique(w$term))
  if (hasName(w, "parameter"))
    w$parameter <- factor(w$parameter, levels = unique(w$parameter))
  if (hasName(w, "contrast"))
    w$contrast <- factor(w$contrast, levels = unique(w$contrast))
  if (hasName(w, "y.level"))
    w$y.level <- factor(w$y.level, levels = unique(w$y.level))
  if (hasName(w, "component"))
    w$component <- factor(w$component, levels = unique(w$component))

  # Prefer using robust.se when tidy object contains it
  if (hasName(w, "robust.se"))
    w$std.error <- w$robust.se

  # There we go..
  if (rule == "rubin1987") {
    pooled <- w %>%
      group_by(!!!syms(grp)) %>%
      summarize(
        m = n(),
        qbar = mean(.data$estimate),
        ubar = mean(.data$std.error^2),
        b = var(.data$estimate),
        t = ifelse(is.null(custom.t),
                   .data$ubar + (1 + 1 / .data$m) * .data$b,
                   eval(parse(text = custom.t))),
        dfcom = dfcom,
        df = barnard.rubin(.data$m, .data$b, .data$t, .data$dfcom),
        riv = (1 + 1 / .data$m) * .data$b / .data$ubar,
        lambda = (1 + 1 / .data$m) * .data$b / .data$t,
        fmi = (.data$riv + 2 / (.data$df + 3)) / (.data$riv + 1)
      )
  }

  if (rule == "reiter2003") {
    pooled <- w %>%
      group_by(!!!syms(grp)) %>%
      summarize(
        m = n(),
        qbar = mean(.data$estimate),
        ubar = mean(.data$std.error^2),
        b = var(.data$estimate),
        t = ifelse(is.null(custom.t),
                   .data$ubar + (1 / .data$m) * .data$b,
                   eval(parse(text = custom.t))),
        dfcom = dfcom,
        df = (.data$m - 1) * (1 + (.data$ubar / (.data$b / .data$m)))^2,
        riv = (1 + 1 / .data$m) * .data$b / .data$ubar,
        lambda = NA_real_,
        fmi = NA_real_
      )
  }

  pooled <- data.frame(pooled)
  names(pooled)[names(pooled) == "qbar"] <- "estimate"
  return(pooled)
}
