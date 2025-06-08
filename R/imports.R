#' @importFrom broom    glance tidy
#' @importFrom dplyr    .data %>% any_of bind_cols bind_rows filter group_by lead
#'                      mutate n pull relocate row_number select summarize
#' @importFrom future   availableCores plan
#' @importFrom future.apply future_lapply
#' @importFrom glmnet   cv.glmnet glmnet
#' @importFrom graphics abline axis box par plot plot.new plot.window
#'                      points rect text
#' @importFrom lars     lars
#' @importFrom lattice  bwplot densityplot stripplot xyplot
#' @importFrom Matrix   nearPD
#' @importFrom mitml    jomoImpute mitmlComplete panImpute testModels
#' @importFrom nnet     multinom
#' @importFrom Rcpp     evalCpp
#' @importFrom rlang    .data env syms
#' @importFrom rpart    rpart rpart.control
#' @importFrom stats    C aggregate as.formula binomial cancor coef
#'                      complete.cases confint
#'                      contr.treatment cor df.residual fitted
#'                      formula gaussian getCall
#'                      glm is.empty.model lm lm.fit
#'                      median model.frame model.matrix
#'                      na.exclude na.omit na.pass plogis
#'                      pf predict pt qt quantile quasibinomial
#'                      rbinom rchisq reformulate rgamma rnorm runif
#'                      sd setNames summary.glm terms update var vcov
#' @importFrom tidyr    complete
#' @importFrom utils    askYesNo flush.console hasName head install.packages
#'                      methods packageDescription packageVersion
#'                      tail write.table
#' @export bwplot
#' @export densityplot
#' @export stripplot
#' @export xyplot
#' @export complete
NULL
