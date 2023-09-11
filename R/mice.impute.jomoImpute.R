#' Multivariate multilevel imputation using `jomo`
#'
#' This function is a wrapper around the `jomoImpute` function
#' from the `mitml` package so that it can be called to
#' impute blocks of variables in `mice`. The `mitml::jomoImpute`
#' function provides an interface to the `jomo` package for
#' multiple imputation of multilevel data
#' <https://CRAN.R-project.org/package=jomo>.
#' Imputations can be generated using `type` or `formula`,
#' which offer different options for model specification.
#'
#' @name mice.impute.jomoImpute
#' @inheritParams mitml::jomoImpute
#' @param data A data frame containing incomplete and auxiliary variables,
#' the cluster indicator variable, and any other variables that should be
#' present in the imputed datasets.
#' @param type An integer vector specifying the role of each variable
#' in the imputation model (see [mitml::jomoImpute()])
#' @param formula A formula specifying the role of each variable
#' in the imputation model. The basic model is constructed
#' by `model.matrix`, thus allowing to include derived variables
#' in the imputation model using `I()`. See
#' [mitml::jomoImpute()].
#' @param format A character vector specifying the type of object that should
#' be returned. The default is `format = "list"`. No other formats are
#' currently supported.
#' @param ... Other named arguments: `n.burn`, `n.iter`,
#' `group`, `prior`, `silent` and others.
#' @return A list of imputations for all incomplete variables in the model,
#' that can be stored in the the `imp` component of the `mids`
#' object.
#' @seealso [mitml::jomoImpute()]
#' @note The number of imputations `m` is set to 1, and the function
#' is called `m` times so that it fits within the `mice`
#' iteration scheme.
#'
#' This is a multivariate imputation function using a joint model.
#' @author Stef van Buuren, 2018, building on work of Simon Grund,
#' Alexander Robitzsch and Oliver Luedtke (authors of `mitml` package)
#' and Quartagno and Carpenter (authors of `jomo` package).
#' @references
#' Grund S, Luedtke O, Robitzsch A (2016). Multiple
#' Imputation of Multilevel Missing Data: An Introduction to the R
#' Package `pan`. SAGE Open.
#'
#' Quartagno M and Carpenter JR (2015).
#' Multiple imputation for IPD meta-analysis: allowing for heterogeneity
#' and studies with missing covariates. Statistics in Medicine,
#' 35:2938-2954, 2015.
#'
#' @family multivariate-2l
#' @keywords datagen
#' @examples
#' \dontrun{
#' # Note: Requires mitml 0.3-5.7
#' blocks <- list(c("bmi", "chl", "hyp"), "age")
#' method <- c("jomoImpute", "pmm")
#' ini <- mice(nhanes, blocks = blocks, method = method, maxit = 0)
#' pred <- ini$pred
#' pred["B1", "hyp"] <- -2
#' imp <- mice(nhanes, blocks = blocks, method = method, pred = pred, maxit = 1)
#' }
#' @export
mice.impute.jomoImpute <- function(data, formula, type, m = 1, silent = TRUE,
                                   format = "imputes", ...) {
  install.on.demand("mitml", ...)

  nat <- mitml::jomoImpute(
    data = data, formula = formula, type = type,
    m = m, silent = silent, ...
  )

  if (format == "native") {
    return(nat)
  }
  cmp <- mitml::mitmlComplete(nat, print = 1)[, names(data)]
  if (format == "complete") {
    return(cmp)
  }
  if (format == "imputes") {
    return(single2imputes(cmp, is.na(data)))
  }
  NULL
}
