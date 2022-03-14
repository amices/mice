#' Evaluate an expression in multiple imputed datasets
#'
#' Performs a computation of each of imputed datasets in data.
#'
#' @param data An object of type \code{mids}, which stands for 'multiply imputed
#' data set', typically created by a call to function \code{mice()}.
#' @param expr An expression to evaluate for each imputed data set. Formula's
#' containing a dot (notation for "all other variables") do not work.
#' @param \dots Not used
#' @return An object of S3 class \code{\link[=mira-class]{mira}}
#' @note Version 3.11.10 changed to tidy evaluation on a quosure. This change
#' should not affect any code that worked on previous versions.
#' It turned out that the latter statement was not true (#292).
#' Version 3.12.2 reverts to the old \code{with()} function.
#' @author Karin Oudshoorn, Stef van Buuren 2009, 2012, 2020
#' @seealso \code{\link[=mids-class]{mids}}, \code{\link[=mira-class]{mira}}, \code{\link{pool}},
#' \code{\link{D1}}, \code{\link{D3}}, \code{\link{pool.r.squared}}
#' @references van Buuren S and Groothuis-Oudshoorn K (2011). \code{mice}:
#' Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#' Statistical Software}, \bold{45}(3), 1-67.
#' \doi{10.18637/jss.v045.i03}
#' @keywords multivariate
#' @examples
#' imp <- mice(nhanes2, m = 2, print = FALSE, seed = 14221)
#'
#' # descriptive statistics
#' getfit(with(imp, table(hyp, age)))
#'
#' # model fitting and testing
#' fit1 <- with(imp, lm(bmi ~ age + hyp + chl))
#' fit2 <- with(imp, glm(hyp ~ age + chl, family = binomial))
#' fit3 <- with(imp, anova(lm(bmi ~ age + chl)))
#' @method with mids
#' @export
with.mids <- function(data, expr, ...) {
  call <- match.call()
  if (!is.mids(data)) {
    stop("The data must have class mids")
  }
  analyses <- as.list(seq_len(data$m))

  # do the repeated analysis, store the result.
  for (i in seq_along(analyses)) {
    data.i <- complete(data, i)
    analyses[[i]] <- eval(expr = substitute(expr), envir = data.i, enclos = parent.frame())
    if (is.expression(analyses[[i]])) {
      analyses[[i]] <- eval(expr = analyses[[i]], envir = data.i, enclos = parent.frame())
    }
  }

  # return the complete data analyses as a list of length nimp
  object <- list(call = call, call1 = data$call, nmis = data$nmis, analyses = analyses)
  # formula=formula(analyses[[1]]$terms))
  oldClass(object) <- c("mira", "matrix")
  object
}
