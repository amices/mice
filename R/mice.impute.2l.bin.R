#'Imputation by a two-level logistic model using \code{glmer}
#'
#'Imputes univariate systematically and sporadically missing data 
#'using a two-level logistic model using \code{lme4::glmer()}
#'
#'Data are missing systematically if they have not been measured, e.g., in the 
#'case where we combine data from different sources. Data are missing sporadically
#'if they have been partially observed.
#'
#'@inheritParams mice.impute.2l.lmer
#'@param intercept Logical determining whether the intercept is automatically
#'added.
#'@param \dots Arguments passed down to \code{glmer}
#'@return Vector with imputed data, same type as \code{y}, and of length 
#'\code{sum(wy)}
#'@author Shahab Jolani, 2015; adapted to mice, SvB, 2018
#'@references
#'Jolani S., Debray T.P.A., Koffijberg H., van Buuren S., Moons K.G.M. (2015).
#'Imputation of systematically missing predictors in an individual 
#'participant data meta-analysis: a generalized approach using MICE. 
#'\emph{Statistics in Medicine}, 34:1841-1863.
#'@family univariate \code{2l} functions
#'@keywords datagen
#'@examples
#'library(tidyr)
#'library(dplyr)
#'data("toenail", package = "HSAUR3")
#'data <- tidyr::complete(toenail, patientID, visit) %>% 
#'  tidyr::fill(treatment) %>% 
#'  dplyr::select(-time) %>%
#'  dplyr::mutate(patientID = as.integer(patientID))
#'
#'\dontrun{
#'pred <- mice(data, print = FALSE, maxit = 0, seed = 1)$pred
#'pred["outcome", "patientID"] <- -2
#'imp <- mice(data, method = "2l.bin", pred = pred, maxit = 1, m = 1, seed = 1)
#'}
#'@export
mice.impute.2l.bin <- function(y, ry, x, type, 
                               wy = NULL, intercept = TRUE, ...) {
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

  X <- x[, fixe, drop = FALSE]
  Z <- x[, rande, drop = FALSE]
  xobs <- x[ry, , drop = FALSE]
  yobs <- y[ry]

  # create formula, use [-1] to remove intercept
  fr <- ifelse(length(rande) > 1, 
               paste("+ ( 1 +", paste(rande[-1L], collapse = "+")), 
               "+ ( 1 ")
  randmodel <- paste("yobs ~ ", paste(fixe[-1L],  collapse = "+"), 
                     fr, "|", clust, ")")
  
  suppressWarnings(fit <- try(lme4::glmer(formula(randmodel), 
                                          data = data.frame(yobs, xobs), 
                                          family = binomial, ... ), 
                              silent = TRUE))
  if(!is.null(attr(fit, "class"))) {
    if(attr(fit, "class") == "try-error") {
      warning("glmer does not run. Simplify imputation model")
      return(y[wy])
    }
  }
  
  # draw beta*
  beta <- lme4::fixef(fit)
  rv <- t(chol(vcov(fit)))
  beta.star <- beta + rv %*% rnorm(ncol(rv))
  
  # calculate psi*
  psi.hat <- matrix(lme4::VarCorr(fit)[[1L]], 
                    nrow = dim(lme4::VarCorr(fit)[[1L]])[1L])
  s <- nrow(psi.hat) * psi.hat
  rancoef <- as.matrix(lme4::ranef(fit)[[1L]])
  lambda <- t(rancoef) %*% rancoef
  temp <- lambda + s
  if (attr(suppressWarnings(chol(temp, pivot = TRUE)), "rank") != nrow(temp)) 
    warning("The cov matrix is not full rank")
  temp <- MASS::ginv(temp)
  ev <- eigen(temp)
  if (mode(ev$values) == "complex") {
    ev$values <- suppressWarnings(as.numeric(ev$values))
    ev$vectors <- suppressWarnings(matrix(as.numeric(ev$vectors),
                                          nrow = length(ev$values)))
    warning("The cov matrix is complex")
  }
  if(sum(ev$values < 0) > 0) {
    ev$values[ev$values < 0] <- 0
    temp <- ev$vectors %*% diag(ev$values, nrow = length(ev$values)) %*% t(ev$vectors)
  }
  deco <- ev$vectors %*% diag(sqrt(ev$values), nrow = length(ev$values))
  temp.psi.star <- stats::rWishart(1, 
                                   nrow(rancoef) + nrow(psi.hat), 
                                   diag(nrow(psi.hat)))[, , 1L]
  psi.star <- MASS::ginv(deco %*% temp.psi.star%*%t(deco)) 
  
  #### psi.star positive definite?
  if (!isSymmetric(psi.star)) psi.star <- (psi.star + t(psi.star)) / 2
  valprop <- eigen(psi.star)
  if(sum(valprop$values < 0) > 0)
  {
    valprop$values[valprop$values < 0] <- 0
    psi.star <- valprop$vectors %*% diag(valprop$values) %*% t(valprop$vectors)
  }
  
  # find clusters for which we need imputes
  clmis <- x[wy, clust]
  
  # the main imputation task
  for (i in clmis) {
    bi.star <- t(MASS::mvrnorm(n = 1L, mu = rep(0, nrow(psi.star)), 
                                       Sigma = psi.star))
    idx <- wy & (x[, clust] == i)
    logit <- X[idx, , drop = FALSE] %*% beta.star + 
      Z[idx, , drop = FALSE] %*% matrix(bi.star, ncol = 1)
    vec <- rbinom(nrow(logit), 1, as.vector(1/(1 + exp(-logit))))
    if (is.factor(y)) {
      vec <- factor(vec, c(0, 1), levels(y))
    }
    y[idx] <- vec
  }
  return(y[wy])
}
