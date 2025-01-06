#' Linear regression for \code{mids} object
#'
#' Applies \code{lm()} to multiply imputed data set
#'
#' This function is included for backward compatibility with V1.0. The function
#' is superseded by \code{\link{with.mids}}.
#'
#' @param formula a formula object, with the response on the left of a ~
#' operator, and the terms, separated by + operators, on the right. See the
#' documentation of \code{\link{lm}} and \code{\link{formula}} for details.
#' @param data An object of type 'mids', which stands for 'multiply imputed data
#' set', typically created by a call to function \code{mice()}.
#' @param \dots Additional parameters passed to \code{\link{lm}}
#' @return An objects of class \code{mira}, which stands for 'multiply imputed
#' repeated analysis'.  This object contains \code{data$m} distinct
#' \code{lm.objects}, plus some descriptive information.
#' @author Stef van Buuren, Karin Groothuis-Oudshoorn, 2000
#' @seealso \code{\link{lm}}, \code{\link{mids}}, \code{\link{mira}}
#' @references Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}:
#' Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#' Statistical Software}, \bold{45}(3), 1-67.
#' \doi{10.18637/jss.v045.i03}
#' @keywords multivariate
#' @examples
#' imp <- mice(nhanes)
#' fit <- lm.mids(bmi ~ hyp + chl, data = imp)
#' fit
#' @export
lm.mids <- function(formula, data, ...) {
  .Deprecated("with",
    msg = "Use with(imp, lm(yourmodel))."
  )
  # adapted 28/1/00 repeated complete data regression (lm) on a mids data set
  call <- match.call()
  if (!is.mids(data)) {
    stop("The data must have class mids")
  }
  analyses <- lapply(seq_len(data$m), function(i) lm(formula, data = complete(data, i), ...))
  # return the complete data analyses as a list of length nimp
  object <- list(call = call, call1 = data$call, nmis = data$nmis, analyses = analyses)
  class(object) <- c("mira", "lm") ## FEH
  object
}


#' Generalized linear model for \code{mids} object
#'
#' Applies \code{glm()} to a multiply imputed data set
#'
#' This function is included for backward compatibility with V1.0. The function
#' is superseded by \code{\link{with.mids}}.
#'
#' @param formula a formula expression as for other regression models, of the
#' form response ~ predictors. See the documentation of \code{\link{lm}} and
#' \code{\link{formula}} for details.
#' @param family The family of the glm model
#' @param data An object of type \code{mids}, which stands for 'multiply imputed
#' data set', typically created by function \code{mice()}.
#' @param \dots Additional parameters passed to \code{\link{glm}}.
#' @return An objects of class \code{mira}, which stands for 'multiply imputed
#' repeated analysis'.  This object contains \code{data$m} distinct
#' \code{glm.objects}, plus some descriptive information.
#' @author Stef van Buuren, Karin Groothuis-Oudshoorn, 2000
#' @seealso \code{\link{with.mids}}, \code{\link{glm}}, \code{\link[=mids-class]{mids}},
#' \code{\link{mira}}
#' @references Van Buuren, S., Groothuis-Oudshoorn, C.G.M. (2000)
#' \emph{Multivariate Imputation by Chained Equations: MICE V1.0 User's manual.}
#' Leiden: TNO Quality of Life.
#' @keywords multivariate
#' @examples
#'
#' imp <- mice(nhanes)
#'
#' # logistic regression on the imputed data
#' fit <- glm.mids((hyp == 2) ~ bmi + chl, data = imp, family = binomial)
#' fit
#' @export
glm.mids <- function(formula, family = gaussian, data, ...) {
  .Deprecated("with",
    msg = "Use with(imp, glm(yourmodel))."
  )
  # adapted 04/02/00 repeated complete data regression (glm) on a mids data set
  call <- match.call()
  if (!is.mids(data)) {
    stop("The data must have class mids")
  }
  analyses <- lapply(
    seq_len(data$m),
    function(i) glm(formula, family = family, data = complete(data, i), ...)
  )
  # return the complete data analyses as a list of length nimp
  object <- list(call = call, call1 = data$call, nmis = data$nmis, analyses = analyses)
  class(object) <- c("mira", "glm", "lm")
  object
}
