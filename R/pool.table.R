#' Combines estimates from a tidy table
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
#' @param rule A string indicating the pooling rule. Currently supported are
#' \code{"rubin1987"} (default, for analyses applied to multiply-imputed
#' incomplete data) and \code{"reiter2003"} (for analyses applied to
#' synthetic data created from complete data).
#' @param custom.t A custom character string to be parsed as a calculation
#' rule for the total variance \code{t}. The custom rule can use the
#' other calculated pooling statistics. The default \code{t} calculation
#' has the form \code{".data$ubar + (1 + 1 / .data$m) * .data$b"}.
#' @param type A string, either \code{"minimal"}, \code{"tests"} or \code{"all"}.
#' Use minimal to mimick the output of \code{summary(pool(fit))}. The default
#' is \code{"all"}.
#' @param conf.int Logical indicating whether to include
#' a confidence interval.
#' @param conf.level Confidence level of the interval, used only if
#' \code{conf.int = TRUE}. Number between 0 and 1.
#' @param exponentiate Flag indicating whether to exponentiate the
#' coefficient estimates and confidence intervals (typical for
#' logistic regression).
#' @param \dots Arguments passed down
#' @details
#' The input data \code{w} is a \code{data.frame} with columns named:
#'
#' \tabular{ll}{
#' \code{term}        \tab a character or factor with the parameter names\cr
#' \code{estimate}    \tab a numeric vector with parameter estimates\cr
#' \code{std.error}   \tab a numeric vector with standard errors of \code{estimate}\cr
#' \code{residual.df} \tab a numeric vector with the degrees of freedom
#' }
#'
#' Columns 1-3 are obligatory. Column 4 is optional. Usually,
#' all entries in column 4 are the same. The user can omit column 4,
#' and specify argument \code{pool.table(..., dfcom = ...)} instead.
#' If both are given, then column \code{residual.df} takes precedence.
#' If neither are specified, then \code{mice} tries to calculate the
#' residual degrees of freedom. If that fails (e.g. because there is
#' no information on sample size), \code{mice} sets \code{dfcom = Inf}.
#' The value \code{dfcom = Inf} is acceptable for large samples
#' (n > 1000) and relatively concise parametric models.
#'
#' @return
#'
#' \code{pool.table()} returns a \code{data.frame} with aggregated
#' estimates, standard errors, confidence intervals and statistical tests.
#'
#' The meaning of the columns is as follows:
#'
#' \tabular{ll}{
#' \code{term}      \tab Parameter name\cr
#' \code{m}         \tab Number of multiple imputations\cr
#' \code{estimate}  \tab Pooled complete data estimate\cr
#' \code{std.error} \tab Standard error of \code{estimate}\cr
#' \code{statistic} \tab t-statistic = \code{estimate} / \code{std.error}\cr
#' \code{df}        \tab Degrees of freedom for \code{statistic}\cr
#' \code{p.value}   \tab One-sided P-value under null hypothesis\cr
#' \code{conf.low}  \tab Lower bound of c.i. (default 95 pct)\cr
#' \code{conf.high} \tab Upper bound of c.i. (default 95 pct)\cr
#' \code{riv}       \tab Relative increase in variance\cr
#' \code{fmi}       \tab Fraction of missing information\cr
#' \code{ubar}      \tab Within-imputation variance of \code{estimate}\cr
#' \code{b}         \tab Between-imputation variance of \code{estimate}\cr
#' \code{t}         \tab Total variance, of \code{estimate}\cr
#' \code{dfcom}     \tab Residual degrees of freedom in complete data\cr
#' }
#'
#' @examples
#' # conventional mice workflow
#' imp <- mice(nhanes2, m = 2, maxit = 2, seed = 1, print = FALSE)
#' fit <- with(imp, lm(chl ~ age + bmi + hyp))
#' pld1 <- pool(fit)
#' pld1$pooled
#'
#' # using pool.table() on tidy table
#' tbl <- summary(fit)[, c("term", "estimate", "std.error", "df.residual")]
#' tbl
#' pld2 <- pool.table(tbl, type = "minimal")
#' pld2
#'
#' identical(pld1$pooled, pld2)
#'
#' # conventional workflow: all numerical output
#' all1 <- summary(pld1, type = "all", conf.int = TRUE)
#' all1
#'
#' # pool.table workflow: all numerical output
#' all2 <- pool.table(tbl)
#' all2
#'
#' class(all1) <- "data.frame"
#' identical(all1, all2)
#' @export
pool.table <- function(w,
                       type = c("all", "minimal", "tests"),
                       conf.int = TRUE,
                       conf.level = 0.95,
                       exponentiate = FALSE,
                       dfcom = Inf,
                       custom.t = NULL,
                       rule = c("rubin1987", "reiter2003"),
                       ...) {
  type <- match.arg(type)
  pooled <- pool.vector(w, dfcom = dfcom, custom.t = custom.t, rule = rule)
  if (type %in% c("all", "tests"))
  pooled <- summary_mipo.workhorse(x = pooled,
                                   type = type,
                                   conf.int = conf.int,
                                   conf.level = conf.level,
                                   exponentiate = exponentiate,
                                   ...)
  return(pooled)
}
