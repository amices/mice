#'@import methods
#'@importFrom splines   bs
#'@importFrom survival  basehaz coxph Surv
#'@importFrom grDevices dev.capabilities hcl palette
#'@importFrom graphics  abline points text plot
#'                      par plot.new plot.window rect
#'                      axis box
#'@importFrom stats     C aggregate as.formula binomial coef
#'                      contr.treatment cor df.residual fitted
#'                      formula gaussian model.frame model.matrix
#'                      glm lm lm.fit
#'                      na.exclude pf predict pt qt rbinom
#'                      rchisq rgamma rnorm runif summary.glm
#'                      quantile
#'                      update var vcov complete.cases na.omit na.pass
#'                      reformulate is.empty.model terms
#'                      confint getCall
#'@importFrom utils     flush.console write.table head tail
#'                      packageDescription methods packageVersion
#'@importFrom MASS      eqscplot lda mvrnorm polr truehist ginv
#'@importFrom nnet      multinom nnet
#'@importFrom rpart     rpart rpart.control
#'@importFrom Rcpp      sourceCpp
#'@importFrom lattice   bwplot densityplot xyplot stripplot
#'@importFrom broom     tidy glance
#'@importFrom rlang     .data
#'@importFrom mitml     panImpute mitmlComplete testModels
#'@importFrom dplyr     summarize bind_rows group_by %>% n select pull lead
#'                      bind_cols mutate
#'@useDynLib mice
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  
  utils::globalVariables(c(
    "param", "term", "ubar", "f", "b", "m", "lambda", "dfold", "dfobs",
    "r", "df", "riv"))