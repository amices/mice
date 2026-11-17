#' Evaluate an expression in multiple imputed datasets
#'
#' Performs a computation of each of imputed datasets in data.
#'
#' @param data An object of type \code{mids}, which stands for 'multiply imputed
#' data set', typically created by a call to function \code{mice()}.
#' @param expr An expression to evaluate for each imputed data set. A
#' formula with a dot (notation for "all other variables"), such as
#' \code{glm(hyp ~ ., family = binomial)}, works if the formula is written
#' in the top-level call, the call has no \code{data} argument, and the
#' called function has one. See Details.
#' @param \dots Not used
#' @details
#' \code{with()} evaluates \code{expr} with the columns of each completed
#' data set as variables, so a model like \code{lm(bmi ~ age)} finds its
#' variables without a \code{data} argument. A dot in a formula, however,
#' can only be expanded from a \code{data} argument. If the top-level call
#' in \code{expr} has a formula with a dot, no \code{data} argument, and
#' calls a function with a \code{data} argument, \code{with()} adds
#' \code{data = .mice.data}, where \code{.mice.data} is the completed data
#' set. The fitted call then shows \code{data = .mice.data}. That object
#' only exists during \code{with()}, so \code{update()} of such a fit
#' outside \code{with()} needs a new \code{data} argument.
#'
#' A dot formula stored in a variable, or a dot formula in a nested call,
#' such as \code{summary(lm(bmi ~ .))}, is not detected. Write
#' \code{data = mget(names(nhanes))} (with the name of your data) to pass
#' the completed data explicitly in such cases.
#' @return An object of S3 class \code{\link{mira}}
#' @note Version 3.11.10 changed to tidy evaluation on a quosure. This change
#' should not affect any code that worked on previous versions.
#' It turned out that the latter statement was not true (#292).
#' Version 3.12.2 reverts to the old \code{with()} function.
#' @author Karin Oudshoorn, Stef van Buuren 2009, 2012, 2020
#' @seealso \code{\link{mids}}, \code{\link{mira}}, \code{\link{pool}},
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
#'
#' # a dot formula: all other variables as predictors
#' fit4 <- with(imp, lm(bmi ~ .))
#' @method with mids
#' @aliases with
#' @export
with.mids <- function(data, expr, ...) {
  call <- match.call()
  if (!is.mids(data)) {
    stop("The data must have class mids")
  }
  analyses <- as.list(seq_len(data$m))
  ex <- substitute(expr)
  enclos <- parent.frame()

  # a dot formula needs a data argument (#265)
  inject <- needs.data.arg(ex, enclos)
  if (inject) {
    ex$data <- as.name(".mice.data")
  }

  # do the repeated analysis, store the result.
  for (i in seq_along(analyses)) {
    data.i <- complete(data, i)
    env <- enclos
    if (inject) {
      env <- new.env(parent = enclos)
      assign(".mice.data", data.i, envir = env)
    }
    analyses[[i]] <- eval(
      expr = ex,
      envir = data.i,
      enclos = env
    )
    if (is.expression(analyses[[i]])) {
      analyses[[i]] <- eval(
        expr = analyses[[i]],
        envir = data.i,
        enclos = env
      )
    }
  }

  # return the complete data analyses as a list of length nimp
  object <- mira(
    call = call,
    call1 = data$call,
    nmis = data$nmis,
    analyses = analyses
  )
  return(object)
}

# TRUE if `ex` is a call with a formula argument that contains a dot, has no
# data argument, and calls a function that has a data argument
needs.data.arg <- function(ex, enclos) {
  if (!is.call(ex) || !is.null(ex$data)) {
    return(FALSE)
  }
  is.dot.formula <- function(a) {
    is.call(a) && identical(a[[1L]], as.name("~")) && "." %in% all.names(a)
  }
  if (!any(vapply(as.list(ex)[-1L], is.dot.formula, logical(1)))) {
    return(FALSE)
  }
  fun <- tryCatch(eval(ex[[1L]], envir = enclos), error = function(e) NULL)
  if (!is.function(fun)) {
    return(FALSE)
  }
  "data" %in% names(formals(fun))
}
