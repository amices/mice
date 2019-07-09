#'Imputation by a two-level normal model using \code{lmer}
#'
#'Imputes univariate systematically and sporadically missing data using a two-level normal model using \code{lme4::lmer()}
#'
#'Data are missing systematically if they have not been measured, e.g., in the 
#'case where we combine data from different sources. Data are missing sporadically
#'if they have been partially observed.
#'
#'While the method is fully Bayesian, it may fix parameters of the 
#'variance-covariance matrix or the random effects to their estimated
#'value in cases where creating draws from the posterior is not 
#'possible. The procedure throws a warning when this happens.
#'
#'@name mice.impute.2l.lmer
#'@inheritParams mice.impute.pmm
#'@param type Vector of length \code{ncol(x)} identifying random and class
#'variables.  Random variables are identified by a '2'. The class variable
#'(only one is allowed) is coded as '-2'. Fixed effects are indicated by 
#'a '1'.
#'@param intercept Logical determining whether the intercept is automatically
#'added.
#'@param \dots Arguments passed down to \code{lmer}
#'@return Vector with imputed data, same type as \code{y}, and of length 
#'\code{sum(wy)}
#'@author Shahab Jolani, 2017
#'@references
#'Jolani S. (2017) Hierarchical imputation of systematically and 
#'sporadically missing data: An approximate Bayesian approach using 
#'chained equations. Forthcoming.
#'
#'Jolani S., Debray T.P.A., Koffijberg H., van Buuren S., Moons K.G.M. (2015).
#'Imputation of systematically missing predictors in an individual 
#'participant data meta-analysis: a generalized approach using MICE. 
#'\emph{Statistics in Medicine}, 34:1841-1863.
#'
#'Van Buuren, S. (2011) Multiple imputation of multilevel data. In Hox, J.J.
#'and and Roberts, J.K. (Eds.), \emph{The Handbook of Advanced Multilevel
#'Analysis}, Chapter 10, pp. 173--196. Milton Park, UK: Routledge.
#'@family univariate-2l
#'@keywords datagen
#'@export
mice.impute.2l.lmer <- function(y, ry, x, type, wy = NULL, intercept = TRUE, ...) {
  
  if (!requireNamespace("lme4", quietly = TRUE))
    stop("Please install package 'lme4'", call. = FALSE)
  
  if (is.null(wy)) wy <- !ry
  
  if (intercept) {
    x <- cbind(1, as.matrix(x))
    type <- c(2, type)
    names(type)[1] <- colnames(x)[1] <- "(Intercept)"
  }
  
  clust <- names(type[type == -2])
  rande <- names(type[type == 2])
  fixe  <- names(type[type > 0])
  
  lev <- unique(x[, clust])

  X <- x[, fixe, drop = FALSE]
  Z <- x[, rande, drop = FALSE]
  xobs <- x[ry, , drop = FALSE]
  yobs <- y[ry]
  Xobs <- X[ry, , drop = FALSE]
  Zobs <- Z[ry, , drop = FALSE]

  # create formula
  fr <- ifelse(length(rande) > 1, 
               paste("+ ( 1 +", paste(rande[-1L], collapse = "+")), 
               "+ ( 1 ")
  randmodel <- paste("yobs ~ ", paste(fixe[-1L],  collapse = "+"), 
                     fr, "|", clust, ")")
  suppressWarnings(fit <- try(lme4::lmer(formula(randmodel), 
                                   data = data.frame(yobs, xobs), 
                                   ...),  
                              silent = TRUE))
  if(!is.null(attr(fit, "class"))) {
    if(attr(fit, "class") == "try-error") {
      warning("lmer does not run. Simplify imputation model")
      return(y[wy])
    }
  }
  
  # taken from lme4
  sigma <- function (object, ...) 
  {
    dc <- object@devcomp
    dd <- dc$dims
    if (dd[["useSc"]]) 
      dc$cmp[[if (dd[["REML"]]) 
        "sigmaREML"
        else "sigmaML"]]
    else 1
  }
  
  # draw sigma*
  sigmahat <- sigma(fit)
  df <- nrow(fit@frame) - length(fit@beta)
  sigma2star <- df * sigmahat^2 / rchisq(1, df)
  
  # draw beta*
  beta <- lme4::fixef(fit)
  RX <- lme4::getME(fit, "RX") 
  
  # cov-matrix, i.e., vcov(fit)
  covmat <- sigma2star * chol2inv(RX)
  rv <- t(chol(covmat))
  beta.star <- beta + rv %*% rnorm(ncol(rv))
  
  # draw psi*
  # applying the standard Wishart prior
  rancoef <- as.matrix(lme4::ranef(fit)[[1]]) 
  lambda <- t(rancoef) %*% rancoef
  df.psi <- nrow(rancoef)
  temp.psi.star <- stats::rWishart(1, df.psi, diag(nrow(lambda)))[, , 1]
  temp <- MASS::ginv(lambda)
  ev <- eigen(temp)
  if (sum(ev$values > 0) == length(ev$values)) {
    deco <- ev$vectors %*% diag(sqrt(ev$values), nrow = length(ev$values))
    psi.star <- MASS::ginv(deco %*% temp.psi.star %*% t(deco)) 
  } else {
    try(temp.svd <- svd(lambda))
    if (class(temp.svd) != "try-error") { 
      deco <- temp.svd$u %*% diag(sqrt(temp.svd$d), nrow = length(temp.svd$d))
      psi.star <- MASS::ginv(deco %*% temp.psi.star %*% t(deco))
    } else {
      psi.star <- temp
      warning("psi fixed to estimate")
    }
  }
  
  # Calculate myi, vyi and drawing bi per cluster
  for (jj in lev) {
    if(jj %in% unique(xobs[, clust])) {
      Xi <- as.matrix(Xobs[xobs[, clust] == jj, ])
      Zi <- as.matrix(Zobs[xobs[, clust] == jj, ])
      yi <- yobs[xobs[, clust] == jj]
      sigma2 <- diag(sigma2star, nrow = nrow(Zi))
      Mi <- psi.star %*% t(Zi) %*% MASS::ginv(Zi %*% psi.star %*% t(Zi) + sigma2)
      myi <- Mi %*% (yi - Xi %*% beta.star)
      vyi <- psi.star - Mi %*% Zi %*% psi.star
    } else {
      myi <- matrix(0, nrow = nrow(psi.star), ncol = 1)
      vyi <- psi.star
    }
    
    vyi <- vyi - upper.tri(vyi) * vyi + t(lower.tri(vyi) * vyi)
    # generating bi.star using eigenvalues 
    deco1 <- eigen(vyi)
    if (sum(deco1$values > 0) == length(deco1$values)){
      A <- deco1$vectors %*% sqrt(diag(deco1$values, nrow = length(deco1$values)))
      bi.star <- myi + A %*% rnorm(length(myi))
    } else {
      # generating bi.star using svd
      try(deco1 <- svd(vyi)) 
      if (class(deco1) != "try-error"){
        A <- deco1$u %*% sqrt(diag(deco1$d, nrow = length(deco1$d)))
        bi.star <- myi + A %*% rnorm(length(myi))
      } else {
        bi.star <- myi
        warning("b_", jj, " fixed to estimate")
      }
    }
    
    # imputation
    y[wy & x[, clust] == jj] <- as.vector(
      as.matrix(X[wy & x[, clust] == jj,, drop = FALSE]) %*% beta.star + 
        as.matrix(Z[wy & x[, clust] == jj,, drop = FALSE]) %*% as.matrix(bi.star) + 
        rnorm(sum(wy & x[, clust] == jj)) * sqrt(sigma2star))
  }
  return(y[wy])
}
