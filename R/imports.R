#' @import              methods
#' @importFrom broom    glance tidy
#' @importFrom dplyr    %>% .data bind_cols bind_rows group_by lead 
#'                      mutate n pull select summarize 
#' @importFrom graphics abline axis box par plot plot.new plot.window 
#'                      points rect text
#' @importFrom lattice  bwplot densityplot stripplot xyplot
#' @importFrom stats    C aggregate as.formula binomial coef
#'                      complete.cases confint 
#'                      contr.treatment cor df.residual fitted
#'                      formula gaussian getCall 
#'                      glm is.empty.model lm lm.fit
#'                      median model.frame model.matrix
#'                      na.exclude na.omit na.pass
#'                      pf predict pt qt quantile 
#'                      rbinom rchisq reformulate rgamma rnorm runif 
#'                      summary.glm terms update var vcov
#' @importFrom utils    flush.console head install.packages methods
#'                      packageDescription packageVersion
#'                      tail write.table
#' @export bwplot
#' @export densityplot
#' @export stripplot
#' @export xyplot
#' @useDynLib mice
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  
  utils::globalVariables(c(
    "param", "term", "ubar", "f", "b", "m", "lambda", "dfold", "dfobs",
    "r", "df", "riv"))