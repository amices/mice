#'Combine estimates by Rubin's rules
#' 
#'The \code{pool()} function combines the estimates from \code{m} 
#'repeated complete data analyses. The typical sequence of steps to 
#'do a multiple imputation analysis is:
#'\enumerate{
#'\item Impute the missing data by the \code{mice} function, resulting in 
#'a multiple imputed data set (class \code{mids});
#'\item Fit the model of interest (scientific model) on each imputed data set 
#'by the \code{with()} function, resulting an object of class \code{mira};
#'\item Pool the estimates from each model into a single set of estimates 
#'and standard errors, resulting is an object of class \code{mipo};
#'\item Optionally, compare pooled estimates from different scientific models 
#'by the \code{D1()} or \code{D3()} functions.
#'}
#'A common error is to reverse steps 2 and 3, i.e., to pool the 
#'multiply-imputed data instead of the estimates. Doing so may severely bias 
#'the estimates of scientific interest and yield incorrect statistical 
#'intervals and p-values. The \code{pool()} function will detect 
#'this case.
#'
#'The \code{pool()} function averages the estimates of the complete 
#'data model, computes the
#'total variance over the repeated analyses by Rubin's rules 
#'(Rubin, 1987, p. 76), 
#'and computes the following diagnostic statistics per estimate:
#'\enumerate{
#'\item Relative increase in variance due to nonresponse {\code{r}};
#'\item Residual degrees of freedom for hypothesis testing {\code{df}};
#'\item Proportion of total variance due to missingness {\code{lambda}};
#'\item Fraction of missing information {\code{fmi}}.
#'}
#'
#'The function requires the following input from each fitted model:
#'\enumerate{ 
#'\item the estimates of the model, usually obtainable by \code{coef()}
#'\item the standard error of each estimate;
#'\item the residual degrees of freedom of the model.
#'}
#' The \code{pool()} function relies on the \code{broom::tidy} for 
#' extracting the parameters. Versions before \code{mice 3.8.5} failed 
#' when no \code{broom::glance()} function was found for extracting the 
#' residual degrees of freedom. The \code{pool()} function is now 
#' more forgiving. 
#'
#' The degrees of freedom calculation for the pooled estimates uses the 
#' Barnard-Rubin adjustment for small samples (Barnard and Rubin, 1999).
#'
#'@param object An object of class \code{mira} (produced by \code{with.mids()} 
#'or \code{as.mira()}), or a \code{list} with model fits.
#'@param dfcom A positive number representing the degrees of freedom in the
#'complete-data analysis. Normally, this would be the number of independent
#'observation minus the number of fitted parameters. The default 
#'(\code{dfcom = NULL}) extract this information in the following 
#'order: 1) the component
#'\code{residual.df} returned by \code{glance()} if a \code{glance()}
#'function is found, 2) the result of \code{df.residual(} applied to 
#'the first fitted model, and 3) as \code{999999}. 
#'In the last case, the warning \code{"Large sample assumed"} is printed.
#'If the degrees of freedom is incorrect, specify the appropriate value 
#'manually.
#'@return An object of class \code{mipo}, which stands for 'multiple imputation
#'pooled outcome'. 
#'@seealso \code{\link{with.mids}}, \code{\link{as.mira}}, 
#'\code{\link[broom]{glance}}, \code{\link[broom]{tidy}}
#'@references Barnard, J. and Rubin, D.B. (1999). Small sample degrees of
#'freedom with multiple imputation. \emph{Biometrika}, 86, 948-955.
#'
#'Rubin, D.B. (1987). \emph{Multiple Imputation for Nonresponse in Surveys}.
#'New York: John Wiley and Sons.
#'
#'van Buuren S and Groothuis-Oudshoorn K (2011). \code{mice}: Multivariate
#'Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical
#'Software}, \bold{45}(3), 1-67. \url{https://www.jstatsoft.org/v45/i03/}
#'@keywords htest
#'@examples
#'# pool using the classic MICE workflow
#'imp <- mice(nhanes, maxit = 2, m = 2)
#'fit <- with(data = imp, exp = lm(bmi ~ hyp + chl))
#'summary(pool(fit))
#'@export
pool <- function (object, dfcom = NULL) {
  call <- match.call()
  
  if (!is.list(object)) stop("Argument 'object' not a list", call. = FALSE)
  object <- as.mira(object)
  m <- length(object$analyses)
  
  # deal with m = 1
  fa <- getfit(object, 1)
  if (m == 1) {
    warning("Number of multiple imputations m = 1. No pooling done.")
    return(fa)
  }
  
  pooled <- pool.fitlist(getfit(object), dfcom = dfcom)
  rr <- list(call = call, m = m, pooled = pooled)
  class(rr) <- c("mipo", "data.frame")
  rr
}

pool.fitlist <- function (fitlist, dfcom = NULL) {
  w <- summary(fitlist, type = "tidy", exponentiate = FALSE)
  
  # residual degrees of freedom of model fitted on hypothetically complete data
  # assumed to be the same across imputations
  if (!is.null(dfcom)) dfcom <- max(dfcom, 1) 
  else {
    if (is.null(dfcom)) {
      dfcom <- try(summary(fitlist, type = "glance")$df.residual[1L], 
                   silent = TRUE)
      if (inherits(dfcom, "try-error")) dfcom <- NULL
    }
    if (is.null(dfcom)) dfcom <- df.residual(getfit(fitlist, 1L))
    if (is.null(dfcom)) {
      dfcom <- 999999
      warning("Large sample assumed.")
    }
  }

  # combine y.level and term into term (for multinom)
  # if ("y.level" %in% names(w)) w$term <- paste(w$y.level, w$term, sep = ":")
  # y.level: multinom
  # component: broom.mixed
  
  # Rubin's rules for scalar estimates
  grp <- intersect(names(w), c("term", "y.level", "component"))
  
  # Note: group_by() changes the order of the terms, which is undesirable
  # We convert any parameter terms to factor to preserve ordering
  if ("term" %in% names(w)) w$term <- factor(w$term, levels = unique(w$term))
  if ("y.level" %in% names(w)) w$y.level <- factor(w$y.level, levels = unique(w$y.level))
  if ("component" %in% names(w)) w$component <- factor(w$component, levels = unique(w$component))
  
  pooled <- w %>%
    group_by(!!!syms(grp)) %>%
    summarize(m = n(),
              qbar = mean(.data$estimate),
              ubar = mean(.data$std.error ^ 2),
              b = var(.data$estimate),
              t = .data$ubar + (1 + 1 / .data$m) * .data$b,
              dfcom = dfcom,
              df = barnard.rubin(.data$m, .data$b, .data$t, .data$dfcom),
              riv = (1 + 1 / .data$m) * .data$b / .data$ubar,
              lambda = (1 + 1 / .data$m) * .data$b / .data$t,
              fmi = (.data$riv + 2 / (.data$df + 3)) / (.data$riv + 1))
  pooled <- data.frame(pooled)
  names(pooled)[names(pooled) == "qbar"] <- "estimate"
  pooled
}

