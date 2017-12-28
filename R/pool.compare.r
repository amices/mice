# --------------------------pool.compare----------------------------

#'Compare two nested models fitted to imputed data
#'
#'Compares two nested models after m repeated complete data analysis
#'
#'The function is based on the article of Meng and Rubin (1992). The
#'Wald-method can be found in paragraph 2.2 and the likelihood method can be
#'found in paragraph 3.  One could use the Wald method for comparison of linear
#'models obtained with e.g. \code{lm} (in \code{with.mids()}).  The likelihood
#'method should be used in case of logistic regression models obtaind with
#'\code{glm()} in \code{with.mids()}.  It is assumed that fit1 contains the
#'larger model and the model in \code{fit0} is fully contained in \code{fit1}.
#'In case of \code{method='Wald'}, the null hypothesis is tested that the extra
#'parameters are all zero.
#'
#'@param fit1 An object of class 'mira', produced by \code{with.mids()}.
#'@param fit0 An object of class 'mira', produced by \code{with.mids()}. The
#'model in \code{fit0} should be a submodel of \code{fit1}. Moreover, the
#'variables of the submodel should be the first variables of the larger model
#'and in the same order as in the submodel.
#'@param data In case of method 'likelihood' it is necessary to pass also the
#'original \code{mids} object to the \code{data} argument. Default value is
#'\code{NULL}, in case of method='Wald'.
#'@param method A string describing the method to compare the two models.  Two
#'kind of comparisons are included so far: 'Wald' and 'likelihood'.
#'@return A list containing several components. Component \code{call} is
#'that call to the \code{pool.compare} function. Component \code{call11} is
#'the call that created \code{fit1}. Component \code{call12} is the 
#'call that created the imputations. Component \code{call01} is the 
#'call that created \code{fit0}. Compenent \code{call02} is the 
#'call that created the imputations. Components \code{method} is the 
#'method used to compare two models: 'Wald' or 'likelihood'. Component
#'\code{nmis} is the number of missing entries for each variable.
#'Component \code{m} is the number of imputations. 
#'Component \code{qhat1} is a matrix, containing the estimated coeffients of the
#'\emph{m} repeated complete data analyses from \code{fit1}. 
#'Component \code{qhat0} is a matrix, containing the estimated coeffients of the
#'\emph{m} repeated complete data analyses from \code{fit0}.
#'Component \code{ubar1} is the mean of the variances of \code{fit1}, 
#'formula (3.1.3), Rubin (1987).
#'Component \code{ubar0} is the mean of the variances of \code{fit0}, 
#'formula (3.1.3), Rubin (1987).
#'Component \code{qbar1} is the pooled estimate of \code{fit1}, formula (3.1.2) Rubin
#'(1987).
#'Component \code{qbar0} is the pooled estimate of \code{fit0}, formula (3.1.2) Rubin
#'(1987).
#'Component \code{Dm} is the test statistic.
#'Component \code{rm} is the relative increase in variance due to nonresponse, formula
#'(3.1.7), Rubin (1987).
#'Component \code{df1}: df1 = under the null hypothesis it is assumed that \code{Dm} has an F
#'distribution with (df1,df2) degrees of freedom.
#'Component \code{df2}: df2. 
#'Component \code{pvalue} is the P-value of testing whether the larger model is
#'statistically different from the smaller submodel.
#'@author Karin Groothuis-Oudshoorn and Stef van Buuren, 2009
#'@seealso \code{\link{lm.mids}}, \code{\link{glm.mids}}, \code{\link{vcov}},
#'@references Li, K.H., Meng, X.L., Raghunathan, T.E. and Rubin, D. B. (1991).
#'Significance levels from repeated p-values with multiply-imputed data.
#'Statistica Sinica, 1, 65-92.
#'
#'Meng, X.L. and Rubin, D.B. (1992). Performing likelihood ratio tests with
#'multiple-imputed data sets.  Biometrika, 79, 103-111.
#'
#'van Buuren S and Groothuis-Oudshoorn K (2011). \code{mice}: Multivariate
#'Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical
#'Software}, \bold{45}(3), 1-67. \url{http://www.jstatsoft.org/v45/i03/}
#'@keywords htest
#'@examples
#'
#'### To compare two linear models:
#'imp <- mice(nhanes2, seed = 51009, print = FALSE)
#'mi1 <- with(data = imp, expr = lm(bmi ~ age + hyp + chl))
#'mi0 <- with(data = imp, expr = lm(bmi ~ age + hyp))
#'pc  <- pool.compare(mi1, mi0, method = 'Wald')
#'pc$pvalue
#'
#'### Comparison of two general linear models (logistic regression).
#'\dontrun{
#'imp  <- mice(boys, maxit = 2, print = FALSE)
#'fit1 <- with(imp, glm(gen > levels(gen)[1] ~ hgt + hc + reg, family = binomial))
#'fit0 <- with(imp, glm(gen > levels(gen)[1] ~ hgt + hc, family = binomial))
#'pool.compare(fit1, fit0, method = 'likelihood', data = imp)$pvalue
#'
#'# using factors
#'fit1 <- with(imp, glm(as.factor(gen > levels(gen)[1]) ~ hgt + hc + reg, family = binomial))
#'fit0 <- with(imp, glm(as.factor(gen > levels(gen)[1]) ~ hgt + hc, family = binomial))
#'pool.compare(fit1, fit0, method = 'likelihood', data = imp)$pvalue
#'}
#'@export
pool.compare <- function(fit1, fit0, data = NULL, method = "Wald") {
  # LLlogistic <- function(formula, data, coefs) {
  #   ### Calculates -2 loglikelihood of a model.
  #   logistic <- function(mu) exp(mu)/(1 + exp(mu))
  #   Xb <- model.matrix(formula, data) %*% coefs
  #   y <- model.frame(formula, data)[1][, 1]
  #   if (is.factor(y)) y <- (0:1)[y]
  #   p <- logistic(Xb)
  #   ## in case values of categorical var are other than 0 and 1.
  #   y <- (y - min(y))/(max(y) - min(y)) 
  #   term1 <- term2 <- rep(0, length(y))
  #   term1[y != 0] <- y[y != 0] * log(y[y != 0]/p[y != 0])
  #   term2[y == 0] <- (1 - y[y == 0]) * log((1 - y[y == 0])/(1 - p[y == 0]))
  #   -2 * sum(term1 + term2)
  # }
  
  # Check the arguments
  call <- match.call()
  meth <- match.arg(tolower(method), c("wald", "likelihood"))
  
  if (!is.mira(fit1) || !is.mira(fit0)) 
    stop("'fit1' and 'fit0' not of class 'mira'", call. = FALSE)
  m1 <- length(fit1$analyses)
  m0 <- length(fit0$analyses)
  if (m1 != m0)
    stop("Number of imputations differs between fit1 and fit0", call. = FALSE)
  if (m1 < 2) 
    stop("At least two imputations are needed", call. = FALSE)
  
  m <- m1
  est1 <- pool(fit1)
  est0 <- pool(fit0)
  dimQ1 <- length(est1$qbar)
  dimQ2 <- dimQ1 - length(est0$qbar)
  # Check: Only need the lm or lmer object
  formula1 <- formula(fit1$analyses[[1]])
  formula0 <- formula(fit0$analyses[[1]])
  vars1 <- est1$term
  vars0 <- est0$term
  
  if (is.null(vars1) || is.null(vars0)) 
    stop("coefficients do not have names", call. = FALSE)
  if (dimQ2 < 1) 
    stop("Model fit1 not larger than fit0", call. = FALSE)
  if (!setequal(vars0, intersect(vars0, vars1))) 
    stop("Model fit0 not contained in fit1", call. = FALSE)
  
  if (meth == "wald") {
    # Reference: paragraph 2.2, Article Meng & Rubin, 
    # Biometrika, 1992.  When two objects are to be compared 
    # we need to calculate matrix Q
    Q <- diag(dimQ1)
    where_new_vars = which(!(vars1 %in% vars0))
    Q <- Q[where_new_vars, , drop = FALSE]
    qbar <- Q %*% est1$qbar
    Ubar <- Q %*% diag(est1$ubar) %*% (t(Q))
    Bm <- Q %*% diag(est1$b) %*% (t(Q))
    rm <- (1 + 1/m) * sum(diag(Bm %*% (solve(Ubar))))/dimQ2
    Dm <- (t(qbar)) %*% (solve(Ubar)) %*% qbar/(dimQ2 * (1 + rm))
  }
  
  if (meth == "likelihood") {
    # Calculate for each imputed dataset the deviance between the two 
    # models with its estimated coefficients
    devM1 <- lapply(getfit(fit1), glance) %>% bind_rows() %>% pull(.data$deviance)
    devM0 <- lapply(getfit(fit0), glance) %>% bind_rows() %>% pull(.data$deviance)
    devM <- mean(devM1 - devM0)
    
    # Calculate for each imputed dataset the deviance between the two 
    # models with the pooled coefficients
    # FIXME: does not yet seem to update deviance
    qbar1 <- pool(getfit(fit1))$qbar
    mds1 <- lapply(getfit(fit1), fix.coef, beta = qbar1)
    devL1 <- lapply(mds1, glance) %>% bind_rows() %>% pull(.data$deviance)
    qbar0 <- pool(getfit(fit0))$qbar
    mds0 <- lapply(getfit(fit0), fix.coef, beta = qbar0)
    devL0 <- lapply(mds0, glance) %>% bind_rows() %>% pull(.data$deviance)
    devL <- mean(devL1 - devL0)
    
    rm <- ((m + 1)/(dimQ2 * (m - 1))) * (devM - devL)
    Dm <- devL / (dimQ2 * (1 + rm))
  }
  
  # Degrees of freedom for F distribution, same for both methods
  v <- dimQ2 * (m - 1)
  if (v > 4) 
    # according to Li 1991
    w <- 4 + (v - 4) * ((1 + (1 - 2 / v) * (1 / rm))^2)  
  else 
    w <- v * (1 + 1 / dimQ2) * ((1 + 1 / rm)^2) / 2
  
  statistic <- list(call = call, call11 = fit1$call, call12 = fit1$call1, 
                    call01 = fit0$call, call02 = fit0$call1, 
                    method = method, nmis = fit1$nmis, m = m, 
                    qbar1 = est1$qbar, qbar0 = est0$qbar, 
                    ubar1 = est1$ubar, ubar0 = est0$ubar, 
                    Dm = Dm, rm = rm, df1 = dimQ2, df2 = w, 
                    pvalue = 1 - pf(Dm, dimQ2, w))
  statistic
}

fix.coef <- function(model, beta = NULL) {
  coefm <- coef(model)
  if (is.null(beta)) beta <- coefm
  if (length(coefm) != length(beta)) 
    stop("incorrect length of 'beta'", call. = FALSE)
  if (is.null(names(beta))) names(beta) <- names(coefm)
  beta <- beta[!names(beta) %in% "(Intercept)"]
  if (length(beta) > 0)
    ff <- as.formula(paste0(". ~ I(", 
                            paste0(paste0(as.character(beta), 
                                          " * ", names(beta)), 
                                   collapse = " + "), ")"))
  else ff <- . ~ 1
  upd <- update(model, formula = ff)
  upd
}
