#' Combine estimates by pooling rules
#'
#' The `pool()` function combines the estimates from `m`
#' repeated complete data analyses. The typical sequence of steps to
#' perform a multiple imputation analysis is:
#' \enumerate{
#' \item Impute the missing data by the `mice()` function, resulting in
#' a multiple imputed data set (class `mids`);
#' \item Fit the model of interest (scientific model) on each imputed data set
#' by the `with()` function, resulting an object of class `mira`;
#' \item Pool the estimates from each model into a single set of estimates
#' and standard errors, resulting in an object of class `mipo`;
#' \item Optionally, compare pooled estimates from different scientific models
#' by the `D1()` or `D3()` functions.
#' }
#' A common error is to reverse steps 2 and 3, i.e., to pool the
#' multiply-imputed data instead of the estimates. Doing so may severely bias
#' the estimates of scientific interest and yield incorrect statistical
#' intervals and p-values. The `pool()` function will detect
#' this case.
#'
#' @details
#' The `pool()` function averages the estimates of the complete
#' data model, computes the total variance over the repeated analyses
#' by Rubin's rules (Rubin, 1987, p. 76), and computes the following
#' diagnostic statistics per estimate:
#' \enumerate{
#' \item Relative increase in variance due to nonresponse {`r`};
#' \item Residual degrees of freedom for hypothesis testing {`df`};
#' \item Proportion of total variance due to missingness {`lambda`};
#' \item Fraction of missing information {`fmi`}.
#' }
#' The degrees of freedom calculation for the pooled estimates uses the
#' Barnard-Rubin adjustment for small samples (Barnard and Rubin, 1999).
#'
#' The `pool.syn()` function combines estimates by Reiter's partially
#' synthetic data pooling rules (Reiter, 2003). This combination rule
#' assumes that the data that is synthesised is completely observed.
#' Pooling differs from Rubin's method in the calculation of the total
#' variance and the degrees of freedom.
#'
#' Pooling requires the following input from each fitted model:
#' \enumerate{
#' \item the estimates of the model;
#' \item the standard error of each estimate;
#' \item the residual degrees of freedom of the model.
#' }
#' The `pool()` and `pool.syn()` functions rely on the
#' `broom::tidy` and `broom::glance` for extracting these
#' parameters.
#'
#' Since `mice 3.0+`, the `broom`
#' package takes care of filtering out the relevant parts of the
#' complete-data analysis. It may happen that you'll see the messages
#' like `Error: No tidy method for objects of class ...` or
#' `Error: No glance method for objects of class ...`. The message
#' means that your complete-data method used in `with(imp, ...)` has
#' no `tidy` or `glance` method defined in the `broom` package.
#'
#' The `broom.mixed` package contains `tidy` and `glance` methods
#' for mixed models. If you are using a mixed model, first run
#' `library(broom.mixed)` before calling `pool()`.
#'
#' If no `tidy` or `glance` methods are defined for your analysis
#' tabulate the `m` parameter estimates and their variance
#' estimates (the square of the standard errors) from the `m` fitted
#' models stored in `fit$analyses`. For each parameter, run
#' [pool.scalar()] to obtain the pooled parameters estimate, its variance, the
#' degrees of freedom, the relative increase in variance and the fraction of missing
#' information.
#'
#' An alternative is to write your own `glance()` and `tidy()`
#' methods and add these to `broom` according to the specifications
#' given in <https://broom.tidymodels.org>.

#' In versions prior to `mice 3.0` pooling required that
#' `coef()` and `vcov()` methods were available for fitted
#' objects. *This feature is no longer supported*. The reason is that
#' `vcov()` methods are inconsistent across packages, leading to
#' buggy behaviour of the `pool()` function.
#'
#' Since `mice 3.13.2` function `pool()` uses the robust
#' the standard error estimate for pooling when it can extract
#' `robust.se` from the `tidy()` object.
#'
#' @param object An object of class `mira` (produced by `with.mids()`
#' or `as.mira()`), or a `list` with model fits.
#' @param dfcom A positive number representing the degrees of freedom in the
#' complete-data analysis. Normally, this would be the number of independent
#' observation minus the number of fitted parameters. The default
#' (`dfcom = NULL`) extract this information in the following
#' order: 1) the component
#' `residual.df` returned by `glance()` if a `glance()`
#' function is found, 2) the result of `df.residual(` applied to
#' the first fitted model, and 3) as `999999`.
#' In the last case, the warning `"Large sample assumed"` is printed.
#' If the degrees of freedom is incorrect, specify the appropriate value
#' manually.
#' @param rule A string indicating the pooling rule. Currently supported are
#' `"rubin1987"` (default, for missing data) and `"reiter2003"`
#' (for synthetic data created from a complete data set).
#' @param custom.t A custom character string to be parsed as a calculation rule
#' for the total variance `t`. The custom rule  can use the other calculated
#' pooling statistics where the dimensions must come from `.data$`. The
#' default `t` calculation would have the form
#' `".data$ubar + (1 + 1 / .data$m) * .data$b"`.
#' See examples for an example.
#' @return An object of class `mipo`, which stands for 'multiple imputation
#' pooled outcome'.
#' For rule `"reiter2003"` values for `lambda` and `fmi` are
#' set to `NA`, as these statistics do not apply for data synthesised from
#' fully observed data.
#' @seealso [with.mids()], [as.mira()], [pool.scalar()],
#' [`glance()`][broom::reexports], [`tidy()`][broom::reexports]
#' <https://github.com/amices/mice/issues/142>,
#' <https://github.com/amices/mice/issues/274>
#' @references
#' Barnard, J. and Rubin, D.B. (1999). Small sample degrees of
#' freedom with multiple imputation. *Biometrika*, 86, 948-955.
#'
#' Rubin, D.B. (1987). *Multiple Imputation for Nonresponse in Surveys*.
#' New York: John Wiley and Sons.
#'
#' Reiter, J.P. (2003). Inference for Partially Synthetic,
#' Public Use Microdata Sets. *Survey Methodology*, **29**, 181-189.
#'
#' van Buuren S and Groothuis-Oudshoorn K (2011). `mice`: Multivariate
#' Imputation by Chained Equations in `R`. *Journal of Statistical
#' Software*, **45**(3), 1-67. \doi{10.18637/jss.v045.i03}
#' @examples
#' # impute missing data, analyse and pool using the classic MICE workflow
#' imp <- mice(nhanes, maxit = 2, m = 2)
#' fit <- with(data = imp, exp = lm(bmi ~ hyp + chl))
#' summary(pool(fit))
#'
#' # generate fully synthetic data, analyse and pool
#' imp <- mice(cars,
#'   maxit = 2, m = 2,
#'   where = matrix(TRUE, nrow(cars), ncol(cars))
#' )
#' fit <- with(data = imp, exp = lm(speed ~ dist))
#' summary(pool.syn(fit))
#'
#' # use a custom pooling rule for the total variance about the estimate
#' # e.g. use t = b + b/m instead of t = ubar + b + b/m
#' imp <- mice(nhanes, maxit = 2, m = 2)
#' fit <- with(data = imp, exp = lm(bmi ~ hyp + chl))
#' pool(fit, custom.t = ".data$b + .data$b / .data$m")
#'
#' @export
pool <- function(object, dfcom = NULL, rule = NULL, custom.t = NULL) {
  call <- match.call()

  if (!is.list(object)) stop("Argument 'object' not a list", call. = FALSE)
  object <- as.mira(object)
  m <- length(object$analyses)

  if (m == 1) {
    warning("Number of multiple imputations m = 1. No pooling done.")
    return(getfit(object, 1))
  }

  model <- getfit(object, 1L)
  dfcom <- get.dfcom(model, dfcom)
  w <- summary(getfit(object), type = "tidy", exponentiate = FALSE)
  pooled <- pool.vector(w, dfcom = dfcom, custom.t = custom.t, rule = rule)

  # mipo object
  rr <- list(
    call = call, m = m,
    pooled = pooled,
    glanced = get.glanced(object)
  )
  class(rr) <- c("mipo", "data.frame")
  rr
}

#' @rdname pool
#' @export
pool.syn <- function(object, dfcom = NULL, rule = "reiter2003") {
  pool(object = object, dfcom = dfcom, rule = rule)
}
