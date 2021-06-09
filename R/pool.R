#' Combine estimates by Rubin's rules
#'
#' The \code{pool()} function combines the estimates from \code{m}
#' repeated complete data analyses. The typical sequence of steps to
#' do a multiple imputation analysis is:
#' \enumerate{
#' \item Impute the missing data by the \code{mice} function, resulting in
#' a multiple imputed data set (class \code{mids});
#' \item Fit the model of interest (scientific model) on each imputed data set
#' by the \code{with()} function, resulting an object of class \code{mira};
#' \item Pool the estimates from each model into a single set of estimates
#' and standard errors, resulting is an object of class \code{mipo};
#' \item Optionally, compare pooled estimates from different scientific models
#' by the \code{D1()} or \code{D3()} functions.
#' }
#' A common error is to reverse steps 2 and 3, i.e., to pool the
#' multiply-imputed data instead of the estimates. Doing so may severely bias
#' the estimates of scientific interest and yield incorrect statistical
#' intervals and p-values. The \code{pool()} function will detect
#' this case.
#'
#' The \code{pool()} function averages the estimates of the complete
#' data model, computes the
#' total variance over the repeated analyses by Rubin's rules
#' (Rubin, 1987, p. 76),
#' and computes the following diagnostic statistics per estimate:
#' \enumerate{
#' \item Relative increase in variance due to nonresponse {\code{r}};
#' \item Residual degrees of freedom for hypothesis testing {\code{df}};
#' \item Proportion of total variance due to missingness {\code{lambda}};
#' \item Fraction of missing information {\code{fmi}}.
#' }
#'
#' The function requires the following input from each fitted model:
#' \enumerate{
#' \item the estimates of the model, usually obtainable by \code{coef()}
#' \item the standard error of each estimate;
#' \item the residual degrees of freedom of the model.
#' }
#'
#' The degrees of freedom calculation for the pooled estimates uses the
#' Barnard-Rubin adjustment for small samples (Barnard and Rubin, 1999).
#'
#' The \code{pool()} function relies on the \code{broom::tidy} for
#' extracting the parameters. Versions before \code{mice 3.8.5} failed
#' when no \code{broom::glance()} function was found for extracting the
#' residual degrees of freedom. The \code{pool()} function is now
#' more forgiving.
#'
#' Since \code{mice 3.13.2} function \code{pool()} uses the robust
#' the standard error estimate for pooling when it can extract
#' \code{robust.se} from the \code{tidy()} object.
#'
#' In versions prior to \code{mice 3.0} pooling required only that
#' \code{coef()} and \code{vcov()} methods were available for fitted
#' objects. \emph{This feature is no longer supported}. The reason is that \code{vcov()}
#' methods are inconsistent across packages, leading to buggy behaviour
#' of the \code{pool()} function.
#'
#' Since \code{mice 3.0+}, the \code{broom}
#' package takes care of filtering out the relevant parts of the
#' complete-data analysis. It may happen that you'll see the messages
#' like \code{Error: No tidy method for objects of class ...} or
#' \code{Error: No glance method for objects of class ...}. The message
#' means that your complete-data method used in \code{with(imp, ...)} has
#' no \code{tidy} or \code{glance} method defined in the \code{broom} package.
#'
#' The \code{broom.mixed} package contains \code{tidy} and \code{glance} methods
#' for mixed models. If you are using a mixed model, first run
#' \code{library(broom.mixed)} before calling \code{pool()}.
#'
#' If no \code{tidy} or \code{glance} methods are defined for your analysis
#' tabulate the \code{m} parameter estimates and their variance
#' estimates (the square of the standard errors) from the \code{m} fitted
#' models stored in \code{fit$analyses}. For each parameter, run
#' \code{\link{pool.scalar}} to obtain the pooled parameters estimate, its variance, the
#' degrees of freedom, the relative increase in variance and the fraction of missing
#' information.
#'
#' An alternative is to write your own \code{glance()} and \code{tidy()}
#' methods and add these to \code{broom} according to the specifications
#' given in \url{https://broom.tidymodels.org}.
#'
#' @param object An object of class \code{mira} (produced by \code{with.mids()}
#' or \code{as.mira()}), or a \code{list} with model fits.
#' @param dfcom A positive number representing the degrees of freedom in the
#' complete-data analysis. Normally, this would be the number of independent
#' observation minus the number of fitted parameters. The default
#' (\code{dfcom = NULL}) extract this information in the following
#' order: 1) the component
#' \code{residual.df} returned by \code{glance()} if a \code{glance()}
#' function is found, 2) the result of \code{df.residual(} applied to
#' the first fitted model, and 3) as \code{999999}.
#' In the last case, the warning \code{"Large sample assumed"} is printed.
#' If the degrees of freedom is incorrect, specify the appropriate value
#' manually.
#' @return An object of class \code{mipo}, which stands for 'multiple imputation
#' pooled outcome'.
#' @seealso \code{\link{with.mids}}, \code{\link{as.mira}}, \code{\link{pool.scalar}},
#' \code{\link[broom:reexports]{glance}}, \code{\link[broom:reexports]{tidy}}
#' \url{https://github.com/amices/mice/issues/142},
#' \url{https://github.com/amices/mice/issues/274}
#' @references Barnard, J. and Rubin, D.B. (1999). Small sample degrees of
#' freedom with multiple imputation. \emph{Biometrika}, 86, 948-955.
#'
#' Rubin, D.B. (1987). \emph{Multiple Imputation for Nonresponse in Surveys}.
#' New York: John Wiley and Sons.
#'
#' van Buuren S and Groothuis-Oudshoorn K (2011). \code{mice}: Multivariate
#' Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical
#' Software}, \bold{45}(3), 1-67. \url{https://www.jstatsoft.org/v45/i03/}
#' @examples
#' # pool using the classic MICE workflow
#' imp <- mice(nhanes, maxit = 2, m = 2)
#' fit <- with(data = imp, exp = lm(bmi ~ hyp + chl))
#' summary(pool(fit))
#' @export
pool <- function(object, dfcom = NULL) {
  call <- match.call()

  if (!is.list(object)) stop("Argument 'object' not a list", call. = FALSE)
  object <- as.mira(object)
  m <- length(object$analyses)

  if (m == 1) {
    warning("Number of multiple imputations m = 1. No pooling done.")
    return(getfit(object, 1))
  }

  dfcom <- get.dfcom(object, dfcom)
  pooled <- pool.fitlist(getfit(object), dfcom = dfcom)

  # mipo object
  rr <- list(
    call = call, m = m,
    pooled = pooled,
    glanced = get.glanced(object)
  )
  class(rr) <- c("mipo", "data.frame")
  rr
}

pool.fitlist <- function(fitlist, dfcom = NULL) {
  w <- summary(fitlist, type = "tidy", exponentiate = FALSE)

  # Rubin's rules for scalar estimates
  grp <- intersect(names(w), c("parameter", "term", "y.level", "component"))

  # Note: group_by() changes the order of the terms, which is undesirable
  # We convert any parameter terms to factor to preserve ordering
  if ("term" %in% names(w)) w$term <- factor(w$term, levels = unique(w$term))
  if ("y.level" %in% names(w)) w$y.level <- factor(w$y.level, levels = unique(w$y.level))
  if ("component" %in% names(w)) w$component <- factor(w$component, levels = unique(w$component))

  # https://github.com/amices/mice/issues/310
  # Prefer using robust.se when tidy object contains it
  if ("robust.se" %in% names(w)) w$std.error <- w$robust.se

  pooled <- w %>%
    group_by(!!!syms(grp)) %>%
    summarize(
      m = n(),
      qbar = mean(.data$estimate),
      ubar = mean(.data$std.error^2),
      b = var(.data$estimate),
      t = .data$ubar + (1 + 1 / .data$m) * .data$b,
      dfcom = dfcom,
      df = barnard.rubin(.data$m, .data$b, .data$t, .data$dfcom),
      riv = (1 + 1 / .data$m) * .data$b / .data$ubar,
      lambda = (1 + 1 / .data$m) * .data$b / .data$t,
      fmi = (.data$riv + 2 / (.data$df + 3)) / (.data$riv + 1)
    )
  pooled <- data.frame(pooled)
  names(pooled)[names(pooled) == "qbar"] <- "estimate"
  pooled
}
