#' Multivariate multilevel imputation using \code{jomo}
#'
#' This function is a wrapper around the \code{jomoImpute} function
#' from the \code{mitml} package so that it can be called to
#' impute blocks of variables in \code{mice}. The \code{mitml::jomoImpute}
#' function provides an interface to the \code{jomo} package for
#' multiple imputation of multilevel data
#' \url{https://CRAN.R-project.org/package=jomo}.
#' Imputations can be generated using \code{type} or \code{formula},
#' which offer different options for model specification.
#'
#' @name mice.impute.jomoImpute
#' @inheritParams mitml::jomoImpute
#' @param data A data frame containing incomplete and auxiliary variables,
#' the cluster indicator variable, and any other variables that should be
#' present in the imputed datasets.
#' @param type An integer vector specifying the role of each variable
#' in the imputation model (see \code{\link[mitml]{jomoImpute}})
#' @param formula A formula specifying the role of each variable
#' in the imputation model. The basic model is constructed
#' by \code{model.matrix}, thus allowing to include derived variables
#' in the imputation model using \code{I()}. See
#' \code{\link[mitml]{jomoImpute}}.
#' @param format A character vector specifying the type of object that should
#' be returned. The default is \code{format = "list"}. No other formats are
#' currently supported.
#' @param ... Other named arguments: \code{n.burn}, \code{n.iter},
#' \code{group}, \code{prior}, \code{silent} and others.
#' @return A list of imputations for all incomplete variables in the model,
#' that can be stored in the the \code{imp} component of the \code{mids}
#' object.
#' @seealso \code{\link[mitml]{jomoImpute}}
#' @note The number of imputations \code{m} is set to 1, and the function
#' is called \code{m} times so that it fits within the \code{mice}
#' iteration scheme.
#'
#' This is a multivariate imputation function using a joint model.
#' @author Stef van Buuren, 2018, building on work of Simon Grund,
#' Alexander Robitzsch and Oliver Luedtke (authors of \code{mitml} package)
#' and Quartagno and Carpenter (authors of \code{jomo} package).
#' @references
#' Grund S, Luedtke O, Robitzsch A (2016). Multiple
#' Imputation of Multilevel Missing Data: An Introduction to the R
#' Package \code{pan}. SAGE Open.
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
