#' Impute multilevel missing data using `pan`
#'
#' This function is a wrapper around the `panImpute` function
#' from the `mitml` package so that it can be called to
#' impute blocks of variables in `mice`. The `mitml::panImpute`
#' function provides an interface to the `pan` package for
#' multiple imputation of multilevel data (Schafer & Yucel, 2002).
#' Imputations can be generated using `type` or `formula`,
#' which offer different options for model specification.
#'
#' @name mice.impute.panImpute
#' @inheritParams mitml::panImpute
#' @param data A data frame containing incomplete and auxiliary variables,
#' the cluster indicator variable, and any other variables that should be
#' present in the imputed datasets.
#' @param type An integer vector specifying the role of each variable
#' in the imputation model (see [mitml::panImpute()])
#' @param formula A formula specifying the role of each variable
#' in the imputation model. The basic model is constructed
#' by `model.matrix`, thus allowing to include derived variables
#' in the imputation model using `I()`. See
#' [mitml::panImpute()].
#' @param format A character vector specifying the type of object that should
#' be returned. The default is `format = "list"`. No other formats are
#' currently supported.
#' @param ... Other named arguments: `n.burn`, `n.iter`,
#' `group`, `prior`, `silent` and others.
#' @return A list of imputations for all incomplete variables in the model,
#' that can be stored in the the `imp` component of the `mids`
#' object.
#' @seealso [mitml::panImpute()]
#' @note The number of imputations `m` is set to 1, and the function
#' is called `m` times so that it fits within the `mice`
#' iteration scheme.
#'
#' This is a multivariate imputation function using a joint model.
#' @author Stef van Buuren, 2018, building on work of Simon Grund,
#' Alexander Robitzsch and Oliver Luedtke (authors of `mitml` package)
#' and Joe Schafer (author of `pan` package).
#' @references
#' Grund S, Luedtke O, Robitzsch A (2016). Multiple
#' Imputation of Multilevel Missing Data: An Introduction to the R
#' Package `pan`. SAGE Open.
#'
#' Schafer JL (1997). Analysis of Incomplete Multivariate Data. London:
#' Chapman & Hall.
#'
#' Schafer JL, and Yucel RM (2002). Computational strategies for
#' multivariate linear mixed-effects models with missing values.
#' Journal of Computational and Graphical Statistics, 11, 437-457.
#' @family multivariate-2l
#' @keywords datagen
#' @examples
#' blocks <- make.blocks(list(c("bmi", "chl", "hyp"), "age"))
#' method <- c("panImpute", "pmm")
#' ini <- mice(nhanes, blocks = blocks, method = method, maxit = 0)
#' pred <- ini$pred
#' pred[c("bmi", "chl", "hyp"), "hyp"] <- -2
#' imp <- mice(nhanes, blocks = blocks, method = method, pred = pred, maxit = 1)
#' @export
mice.impute.panImpute <- function(data, formula, type, m = 1, silent = TRUE,
                                  format = "imputes", ...) {
  install.on.demand("mitml", ...)

  nat <- mitml::panImpute(
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
