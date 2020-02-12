# ------------------------------with.mids----------------------------

#'Evaluate an expression in multiple imputed datasets
#'
#'Performs a computation of each of imputed datasets in data.
#'
#'
#'@param data An object of type \code{mids}, which stands for 'multiply imputed
#'data set', typically created by a call to function \code{mice()}.
#'@param expr An expression with a formula object, with the response on the
#'left of a \code{~} operator, and the terms, separated by \code{+} operators,
#'on the right. See the documentation of \code{\link{lm}} and
#'\code{\link{formula}} for details.
#'@param \dots Additional parameters passed to \code{expr}
#'@return A list object of S3 class \code{mira}
# '@returnItem call The call that created the \code{mira} object.
# '@returnItem call1 The call that created the \code{mids} object that was used
# 'in \code{call}.
# '@returnItem nmis An array containing the number of missing observations per
# 'column.
# '@returnItem analyses A list of \code{m} components containing the individual
# 'fit objects from each of the \code{m} complete data analyses.
#'@author Karin Oudshoorn, Stef van Buuren 2009-2012
#'@seealso \code{\link[=mids-class]{mids}}, \code{\link[=mira-class]{mira}}, \code{\link{pool}},
#'\code{\link{D1}}, \code{\link{D3}}, \code{\link{pool.r.squared}}
#'@references van Buuren S and Groothuis-Oudshoorn K (2011). \code{mice}:
#'Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#'Statistical Software}, \bold{45}(3), 1-67.
#'\url{https://www.jstatsoft.org/v45/i03/}
#'@keywords multivariate
#'@examples
#'
#'
#'imp <- mice(nhanes2)
#'fit1 <- with(data=imp,exp=lm(bmi~age+hyp+chl))
#'fit2 <- with(data=imp,exp=glm(hyp~age+bmi+chl,family=binomial))
#'anova.imp <- with(data=imp,exp=anova(lm(bmi~age+hyp+chl)))
#'@method with mids
#'@export
with.mids <- function(data, expr, ...) {
    # General function to do repeated analyses.
    # Generalisation of lm.mids and glm.mids.
    # KO, 2009.
    #
    # repeated complete data regression on a mids data set.
    # Depending on 'expr' different types of regressions are preformed.
    # for 'expr' can be used: lm, lme, glm, etc.
    # SvB formula deleted, 13Aug09: expr can contain any executable expression
    # SvB: now works for both calls and expressions
    
    call <- match.call()
    if (!is.mids(data)) 
        stop("The data must have class mids")
    analyses <- as.list(seq_len(data$m))
    
    # do the repeated analysis, store the result.
    for (i in seq_along(analyses)) {
      data.i <- complete(data, i)
      analyses[[i]] <- eval(expr = substitute(expr), envir = data.i, enclos = parent.frame())
      if (is.expression(analyses[[i]])) 
        analyses[[i]] <- eval(expr = analyses[[i]], envir = data.i, enclos = parent.frame())
    }
    # return the complete data analyses as a list of length nimp
    object <- list(call = call, call1 = data$call, nmis = data$nmis, analyses = analyses)
    # formula=formula(analyses[[1]]$terms))
    oldClass(object) <- c("mira", "matrix")
    return(object)
}
