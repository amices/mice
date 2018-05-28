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
#'by the \code{pool.compare()} function.
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
#'The \code{pool()} function relies on the \code{broom::tidy} and 
#'\code{broom::glance} function for extracting this information from a 
#'list of fitted models. 
#'
#'The degrees of freedom calculation uses the Barnard-Rubin adjustment 
#'for small samples (Barnard and Rubin, 1999).
#'
#'@param object An object of class \code{mira} (produced by \code{with.mids()} 
#'or \code{as.mira()}), or a \code{list} with model fits.
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
pool <- function (object) {
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
  
  pooled <- pool.fitlist(getfit(object))
  rr <- list(call = call, m = m, pooled = pooled)
  class(rr) <- c("mipo", "data.frame")
  rr
}

pool.fitlist <- function (fitlist) {
  v <- summary(fitlist, type = "glance")
  w <- summary(fitlist, type = "tidy", exponentiate = FALSE)
  
  # residual degrees of freedom of model fitted on hypothetically complete data
  # assumed to be the same across imputations
  dfcom <- v$df.residual[1L]
  if (is.null(dfcom)) dfcom <- df.residual(getfit(fitlist, 1L))
  if (is.null(dfcom)) dfcom <- Inf
  
  # combine y.level and term into term (for multinom)
  if ("y.level" %in% names(w)) w$term <- paste(w$y.level, w$term, sep = ":")
  
  # Rubin's rules for scalar estimates
  pooled <- w %>%
    mutate(param = rep_len(1L:length(unique(term)), length.out = n())) %>%
    group_by(param) %>%
    summarize(m = n(),
              term = .data$term[1L],
              qbar = mean(.data$estimate),
              ubar = mean(.data$std.error ^ 2),
              b = var(.data$estimate),
              t = ubar + (1 + 1 / m) * b,
              dfcom = dfcom,
              df = barnard.rubin(m, b, t, dfcom),
              riv = (1 + 1 / m) * b / ubar,
              lambda = (1 + 1 / m) * b / t,
              fmi = (riv + 2 / (df + 3)) / (riv + 1)) %>%
    select(-m, -param)
  pooled <- data.frame(pooled[, -1L], 
                       row.names = pooled$term)
  names(pooled)[1L] <- "estimate"
  pooled
}

