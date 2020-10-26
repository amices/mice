#' NHANES example - mixed numerical and discrete variables
#'
#' A small data set with non-monotone missing values.
#'
#' A small data set with missing data and mixed numerical and discrete
#' variables. The data set \code{nhanes} is the same data set, but with all data
#' treated as numerical.
#'
#' @name nhanes2
#' @docType data
#' @format A data frame with 25 observations on the following 4 variables.
#' \describe{
#' \item{age}{Age group (1=20-39, 2=40-59, 3=60+)}
#' \item{bmi}{Body mass index (kg/m**2)}
#' \item{hyp}{Hypertensive (1=no,2=yes)}
#' \item{chl}{Total serum cholesterol (mg/dL)} }
#' @seealso \code{\link{nhanes}}
#' @source Schafer, J.L. (1997).  \emph{Analysis of Incomplete Multivariate
#' Data.} London: Chapman & Hall. Table 6.14.
#' @keywords datasets
#' @examples
#' # create 5 imputed data sets
#' imp <- mice(nhanes2)
#'
#' # print the first imputed data set
#' complete(imp)
NULL
