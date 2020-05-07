#' Imputation under MNAR mechanism by NARFCS
#'
#' Imputes univariate data under a user-specified MNAR mechanism by 
#' linear or logistic regression and NARFCS. Sensitivity analysis under 
#' different model specifications may shed light on the impact of 
#' different MNAR assumptions on the conclusions.
#'
#' @rdname mice.impute.mnar
#' @aliases mice.impute.mnar.norm mnar.norm 
#'          mice.impute.mnar.logreg mnar.logreg
#' @inheritParams mice.impute.pmm
#' @param ums A string containing the specification of the 
#' unidentifiable part of the imputation model (the *unidentifiable 
#' model specification”), that is, the desired \eqn{\delta}-adjustment 
#' (offset) as a function of other variables and values for the 
#' corresponding deltas (sensitivity parameters). See details.
#' @param umx An auxiliary data matrix containing variables that do 
#' not appear in the identifiable part of the imputation procedure 
#' but that have been specified via \code{ums} as being predictors 
#' in the unidentifiable part of the imputation model. See details.
#' @return Vector with imputed data, same type as \code{y}, and of length 
#' \code{sum(wy)}
#' @details
#' This function imputes data that are thought to be Missing Not at 
#' Random (MNAR) by the NARFCS method. The NARFCS procedure 
#' (Leacy, 2016; Tompsett et al, 2018) generalises the so-called 
#' \eqn{\delta}-adjustment sensitivity analysis method of Van Buuren, 
#' Boshuizen & Knook (1999) to the case with multiple incomplete
#' variables within the FCS framework. In practical terms, the 
#' NARFCS procedure shifts the imputations drawn at each
#' iteration of \code{mice} by a user-specified quantity that can 
#' vary across subjects, to reflect systematic departures of the 
#' missing data from the data distribution imputed under MAR.
#' 
#' Specification of the NARFCS model is done by the \code{blots} 
#' argument of \code{mice()}. The \code{blots} parameter is a named 
#' list. For each variable to be imputed by
#' \code{mice.impute.mnar.norm()} or \code{mice.impute.mnar.logreg()}
#' the corresponding element in \code{blots} is a list with 
#' at least one argument \code{ums} and, optionally, a second 
#' argument \code{umx}. 
#' For example, the high-level call might like something like 
#' \code{mice(nhanes[, c(2, 4)], method = c("pmm", "mnar.norm"), blots = list(chl = list(ums = "-3+2*bmi")))}.
#' 
#' The \code{ums} parameter is required, and might look like this: 
#' \code{"-4+1*Y"}. The \code{ums} specifcation must have the 
#' following characteristics:
#' \enumerate{
#' \item{A single term corresponding to the intercept (constant) term, 
#' not multiplied by any variable name, must be included in the 
#' expression;}
#' \item{Each term in the expression (corresponding to the intercept 
#' or a predictor variable) must be separated by either a \code{"+"} 
#' or \code{"-"} sign, depending on the sign of the sensitivity 
#' parameter;}
#' \item{Within each non-intercept term, the sensitivity parameter 
#' value comes first and the predictor variable comes second, and these 
#' must be separated by a \code{"*"} sign;}
#' \item{For categorical predictors, for example a variable \code{Z} 
#' with K + 1 categories \code{("Cat0","Cat1", ...,"CatK")}, K
#' category-specific terms are needed, and those not in \code{umx} 
#' (see below) must be specified by concatenating the variable name 
#' with the name of the category (e.g. \code{ZCat1}) as this is how 
#' they are named in the design matrix (argument \code{x}) passed 
#' to the univariate imputation function. An example is 
#' \code{"2+1*ZCat1-3*ZCat2"}.}
#' }
#' 
#' If given, the \code{umx} specification must have the following 
#' characteristics:
#' \enumerate{
#' \item{It contains only complete variables, with no missing values;}
#' \item{It is a numeric matrix. In particular, categorical variables 
#' must be represented as dummy indicators with names corresponding 
#' to what is used in \code{ums} to refer to the category-specific terms 
#' (see above);}
#' \item{It has the same number of rows as the \code{data} argument 
#' passed on to the main \code{mice} function;}
#' \item{It does not contain variables that were already predictors 
#' in the identifiable part of the model for the variable under 
#' imputation.}
#' }
#' 
#' Limitation: The present implementation can only condition on variables 
#' that appear in the identifiable part of the imputation model (\code{x}) or 
#' in complete auxiliary variables passed on via the \code{umx} argument. 
#' It is not possible to specify models where the offset depends on 
#' incomplete auxiliary variables.
#' 
#' For an MNAR alternative see also \code{\link{mice.impute.ri}}.
#' 
#' @author Margarita Moreno-Betancur, Stef van Buuren, Ian R. White, 2020.
#' @references 
#' Leacy, F.P. (2016). \emph{Multiple imputation under missing not at 
#' random assumptions via fully conditional specification}. 
#' Dissertation, University of Cambridge, UK.
#' 
#' Tompsett, D. M., Leacy, F., Moreno-Betancur, M., Heron, J., & 
#' White, I. R. (2018). On the use of the not-at-random fully 
#' conditional specification (NARFCS) procedure in practice. 
#' \emph{Statistics in Medicine}, \bold{37}(15), 2338-2353. 
#' \url{https://doi.org/10.1002/sim.7643}.
#' 
#' Van Buuren, S., Boshuizen, H.C., Knook, D.L. (1999) Multiple 
#' imputation of missing blood pressure covariates in survival analysis.
#' \emph{Statistics in Medicine}, \bold{18}, 681--694.
#' 
#' @family univariate imputation functions
#' @keywords datagen
#' @examples 
#' # 1: Example with no auxiliary data: only pass unidentifiable model specification (ums) 
#' 
#' # Specify argument to pass on to mnar imputation functions via "blots" argument
#' mnar.blot <- list(X = list(ums = "-4"), Y = list(ums = "2+1*ZCat1-3*ZCat2"))
#' 
#' # Run NARFCS by using mnar imputation methods and passing argument via blots
#' impNARFCS <- mice(mnar_demo_data, method = c("mnar.logreg", "mnar.norm", ""),
#'                   blots = mnar.blot, seed = 234235, print = FALSE)
#'                   
#' # Obtain MI results: Note they coincide with those from old version at 
#' # https://github.com/moreno-betancur/NARFCS
#' pool(with(impNARFCS,lm(Y ~ X + Z)))$pooled$estimate
#' 
#' # 2: Example passing also auxiliary data to MNAR procedure (umx)
#' # Assumptions: 
#' # - Auxiliary data are complete, no missing values
#' # - Auxiliary data are a numeric matrix
#' # - Auxiliary data have same number of rows as x
#' # - Auxiliary data have no overlapping variable names with x
#' 
#' # Specify argument to pass on to mnar imputation functions via "blots" argument
#' aux <- matrix(0:1, nrow = nrow(mnar_demo_data))
#' dimnames(aux) <- list(NULL, "even")
#' mnar.blot <- list(X = list(ums = "-4"), 
#'                   Y = list(ums = "2+1*ZCat1-3*ZCat2+0.5*even", umx = aux))
#'                   
#' # Run NARFCS by using mnar imputation methods and passing argument via blots
#' impNARFCS <- mice(mnar_demo_data, method = c("mnar.logreg", "mnar.norm", ""),
#'                   blots = mnar.blot, seed = 234235, print = FALSE)
#'                   
#' # Obtain MI results: As expected they differ (slightly) from those 
#' # from old version at https://github.com/moreno-betancur/NARFCS
#' pool(with(impNARFCS,lm(Y ~ X + Z)))$pooled$estimate
#' @export
mice.impute.mnar.norm <- function(y, ry, x, wy = NULL, 
                                  ums = NULL, umx = NULL, ...) {
  
  ## Undentifiable part:
  u <- parse.ums(x, ums = ums, umx = umx, ...)
  
  ## Identifiable part: exactly the same as mice.impute.norm
  if (is.null(wy)) wy <- !ry
  x <- cbind(1, as.matrix(x))
  parm <- .norm.draw(y, ry, x, ...)
  
  ## Draw imputations
  return(x[wy, ] %*% parm$beta + 
       u$x[wy, ] %*% u$delta + 
       rnorm(sum(wy)) * parm$sigma)
}

