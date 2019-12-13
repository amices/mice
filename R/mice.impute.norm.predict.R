#'Imputation by linear regression through prediction
#'
#'Imputes the "best value" according to the linear regression model, also 
#'known as \emph{regression imputation}.
#'
#'@aliases mice.impute.norm.predict norm.predict
#'@inheritParams mice.impute.pmm
#'@return Vector with imputed data, same type as \code{y}, and of length 
#'\code{sum(wy)}
#'@details
#'Calculates regression weights from the observed data and returns predicted
#'values to as imputations. This
#'method is known as \emph{regression imputation}.
#'@section Warning: THIS METHOD SHOULD NOT BE USED FOR DATA ANALYSIS. 
#'This method is seductive because it imputes the most 
#'likely value according to the model. However, it ignores the uncertainty
#'of the missing values and artificially 
#'amplifies the relations between the columns of the data. Application of 
#'richer models having more parameters does not help to evade these issues. 
#'Stochastic regression methods, like \code{\link{mice.impute.pmm}} or 
#'\code{\link{mice.impute.norm}}, are generally preferred. 
#'
#'At best, prediction can give reasonable estimates of the mean, especially 
#'if normality assumptions are plausible. See Little and Rubin (2002, p. 62-64)
#'or Van Buuren (2012, p. 11-13, p. 45-46) for a discussion of this method. 
#'@author Gerko Vink, Stef van Buuren, 2018
#'@references 
#'Little, R.J.A. and Rubin, D.B. (2002). Statistical Analysis with Missing 
#'Data.  New York: John Wiley and Sons.
#'
#'Van Buuren, S. (2018). 
#'\href{https://stefvanbuuren.name/fimd/sec-linearnormal.html}{\emph{Flexible Imputation of Missing Data. Second Edition.}}
#'Chapman & Hall/CRC. Boca Raton, FL.
#'@family univariate imputation functions
#'@keywords datagen
#'@export
mice.impute.norm.predict <- function(y, ry, x, wy = NULL, ...) {
  if (is.null(wy)) wy <- !ry
  x <- cbind(1, as.matrix(x))
  p <- estimice(x[ry, , drop = FALSE], y[ry], ...)
  return(x[wy, , drop = FALSE] %*% p$c)
}
