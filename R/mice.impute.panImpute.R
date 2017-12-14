#'Impute multilevel missing data using \code{pan}
#'
#'This function is a wrapper around the \code{panImpute} function
#'from the \code{mitml} package so that it can be called to 
#'impute blocks of variables in \code{mice}. The \code{mitml::panImpute}
#'function provides an interface to the \code{pan} package for 
#'multiple imputation of multilevel data (Schafer & Yucel, 2002).
#'Imputations can be generated using \code{type} or \code{formula}, 
#'which offer different options for model specification.
#'
#'@name mice.impute.panImpute
#'@param data A data frame containing incomplete and auxiliary variables, 
#'the cluster indicator variable, and any other variables that should be 
#'present in the imputed datasets.
#'@param type An integer vector specifying the role of each variable 
#'in the imputation model (see \code{\link[mitml]{panImpute}})
#'@param formula A formula specifying the role of each variable 
#'in the imputation model. The basic model is constructed 
#'by \code{model.matrix}, thus allowing to include derived variables 
#'in the imputation model using \code{I()}. See 
#'\code{\link[mitml]{panImpute}}.
#'@param format A character vector specifying the type of object that should 
#'be returned. The default is \code{format = "list"}. No other formats are
#'currently supported.
#'@param ... Other named arguments: \code{n.burn}, \code{n.iter}, 
#'\code{group}, \code{prior}, \code{silent} and others.
#'@return A list of imputations for all incomplete variables in the model,
#'that can be stored in the the \code{imp} component of the \code{mids} 
#'object.
#'@seealso \code{\link[mitml]{panImpute}}
#'@note The number of imputations \code{m} is set to 1, and the function 
#'is called \code{m} times so that it fits within the \code{mice} 
#'iteration scheme.
#'
#'This is a multivatiate imputation function using a joint model.
#'@author Stef van Buuren, 2018, building on work of Simon Grund, 
#'Alexander Robitzsch and Oliver Luedtke (authors of \code{mitml} package) 
#'and Joe Schafer (author of \code{pan} package).
#'@references
#' Simon Grund, Oliver Luedtke, Alexander Robitzsch (2016). Multiple 
#' Imputation of Multilevel Missing Data: An Introduction to the R 
#' Package \code{pan}. SAGE Open.
#'
#'Schafer JL. Analysis of Incomplete Multivariate Data. London: 
#'Chapman & Hall, 1997.
#'@family multivariate \code{2l} functions
#'@keywords datagen
#'@export
mice.impute.panImpute <- function(data, formula, type, 
                                  format = "list", ...) {
  NULL
}