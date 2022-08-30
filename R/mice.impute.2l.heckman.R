#' Imputation based on Heckman model for multilevel data.
#'
#' Imputes outcome and predictor variables that follow an MNAR mechanism
#' according to Heckman's model and come from a multilevel database such as
#' individual participant data.
#' @aliases mice.impute.2l.heckman 2l.heckman
#' @param y Vector to be imputed
#' @param ry Logical vector of length \code{length(y)} indicating the
#' the subset \code{y[ry]} of elements in \code{y} to which the imputation
#' model is fitted. The \code{ry} generally distinguishes the observed
#' (\code{TRUE}) and missing values (\code{FALSE}) in \code{y}.
#' @param x Numeric design matrix with \code{length(y)} rows with predictors for
#' \code{y}. Matrix \code{x} may have no missing values.
#' @param wy Logical vector of length \code{length(y)}. A \code{TRUE} value
#' indicates locations in \code{y} for which imputations are created.
#' @param type type of the variable in the prediction model {0: No predictor,
#' 1: Predictor in both the outcome and selection,-2: Cluster id (study id),
#' -3: Predictor only in the selection model, -4: Predictor only in the outcome 
#' model}
#' @param pmm  predictive mean matching can be applied only for for missing continuous variables: "TRUE","FALSE"
#' @param meta_method meta_analysis estimation method for random effects :
#' "ml" (maximum likelihood), "reml" (restricted maximum likelihood) or "mm"
#' method of moments.
#' @param ... Other named arguments. Not used.
#' @name mice.impute.2l.heckman
#' @return Vector with imputed data, of type binary or continuous
#' @details Imputation of binary and continuous variables by Heckman model.
#' This function uses information at the marginal and cluster level to impute
#' both outcome and predictor variables that are sporadically or systematically
#' missing in a cluster.
#' Initially, the function estimates the parameters of the Heckman model for
#' each cluster in the database using the copula method, and then estimates the
#' parameters at the marginal level with the help of a random intercept
#' meta-analysis model.
#' The individual-level parameters are drawn and missing values are imputed
#' using Heckman's conditional expectation.
#'
#' @note
#' Binary missing variables should be included as two-level factor type variables,
#' numerical missing variables will be assumed normally distributed.
#' In case the Heckman model cannot be estimated at the study level, the 
#' imputation model is based on the Heckman model without taking into account 
#' the cluster variable.
#' Added:
#' @author Julius Center Methods Group UMC, 2022
#' @family univariate imputation functions
#' @keywords datagen
#'
#' @export
#'



mice.impute.2l.heckman <-function(y,ry,x,wy = NULL, type, pmm = FALSE, meta_method ="reml",...) {

  install.on.demand("GJRM", ...)
  install.on.demand("Matrix", ...)
  install.on.demand("mgcv", ...)
  install.on.demand("mixmeta", ...)
  install.on.demand("mvtnorm", ...)
  install.on.demand("pbivnorm", ...)
  
  
   # 1. Define variables and dataset----
  
  # Rename covariates
  colnames(x) <- paste0("x_", 1:length(colnames(x))) #change the covariates name for avoiding conflicts when y is covariate
  bos_name <- colnames(x)[type ==  1] # names of variables present in both outcome and selection model
  sel_name <- colnames(x)[type == -3] # names of variables in selection model alone
  out_name <- colnames(x)[type == -4] # names of variables in outcome model alone
  
  # # Define y type
  if (class(y) == "factor" & nlevels(y) == 2){
    message("the missing variable is assumed to be binomially distributed")
    family <- "binomial"
  }else{
    message("the missing variable is assumed to be normally distributed")
    family <- "gaussian"
  }
  
  
  # Check if group variable is defined
  if (length(colnames(x)[type == -2]) == 0) {
    message("No group variable has been provided, the Heckman imputation model will be applied globally to the dataset.")
    Grp_est <- 0 # Group indicator 0: Heckman on full dataset, 1: Heckman at cluster level
  } else {
    group_name <- colnames(x)[type == -2]
    names.clust <- as.character(unique(x[, group_name]))
    Grp_est <- 1
  }
  
  #Define position of selection and outcome in coefficient vector
  order <- c(sel_name, bos_name, out_name)
  send <- length(sel_name) + length(bos_name) + 1 # Index where selection equation ends in coefficient vector
  oend <- send + length(bos_name) + length(out_name) + 1 # Index where outcome equation ends in coefficient vector
  
  # Define outcome and selection equation
  out <- as.formula(paste0("y", "~", paste(c(bos_name,out_name), collapse = "+")))
  sel <- as.formula(paste0("ry", "~", paste(c(sel_name, bos_name), collapse = "+")))
  
  # Define data & prediction matrix
  data <- data.frame(ry, y, x[, order])
  X <- data.frame(cbind(Int=rep(1, nrow(x)), x[, order]))
  
  # 2. Step 1: Get the theta estimators for each study ----
  Syst_nest <- Heck_est <- 1
  if (Grp_est == 1) { #Heckman at cluster level
    df_list <- split(data, x[, group_name]) 
    res_total <- suppressWarnings( lapply( df_list, copulaIPD,
                                           sel = sel, out = out, send = send, family = family)) #Calculate parameters for each study
    studytype <- sapply(res_total, function(x) x[[2]]) # Specify missing pattern in studies
    fit_list  <- lapply(res_total, function(x) x[[1]]) # Get the parameter estimates
    coef_list <- lapply(fit_list[studytype != 0], `[[`, c('coefficients')) # Get effect vector for each study
    Vb_list   <- lapply(fit_list[studytype != 0], `[[`, c('Vb')) # Get covariance matrix for each study
    coef_mat_s <- do.call(rbind, coef_list)
    
    varnam <- colnames(coef_mat_s)
    selnam <- varnam[grepl("*_s" , varnam)]
    outnam <- varnam[!varnam %in% c(selnam, "sigma.star", "theta.star")]
    
    # 3. Step 2: Get marginal theta and var(theta) ----
    
    if( length(studytype[studytype != 0]) < 2 ){
      Grp_est <- 0
    }else{
      Heck_mod <- get_marginal(coef_mat_s, Vb_list, selnam, outnam, meta_method)
      Heck_est <- Heck_mod$Mvma_est}
    Sys.nest <-as.numeric(length(studytype[studytype == 0])==0)
    
  } 
  
  if (Grp_est == 0 | Heck_est == 0){ # Heckman on full dataset or Heckman model no estimable
    message("The Heckman model cannot be estimated marginally, so systematically missing groups will be imputed with the Heckman model based on the full dataset.")
    Heck_mod <- copulaIPD( data = data, sel = sel, out = out, family = family, send = send)
    
    if(Heck_mod[[2]] == 0 &(Grp_est==0|Syst_nest==0)){
      stop("There is insufficient information to impute the Heckman model at the marginal or study level.")
    }
  }
  
  
  # 4. Get theta_k and var(theta_k) from full conditional distribution ----
  if (Grp_est == 1) { # Applies imputation at cluster level
    for (i in names.clust) { #Loop across studies
      
      if (studytype[[i]] == 0) { #systematically missing
        star <- star_systematic( Heck_mod, send, oend, family)
        
      } else { # sporadically missing
        star <- star_sporadic( Heck_mod,
                               coef_list_i = coef_list[[i]],
                               Vb_list_i = Vb_list[[i]], selnam, outnam,family)
      }
      
      Xm <- X[!ry & x[, group_name] == as.numeric(i),]
      
      if (nrow(Xm) != 0) { #Cluster with at least one missing value in outcome equation
        y.star <- gen_y_star( Xm= Xm, sel_name = sel_name, bos_name = bos_name,
                              out_name =out_name, beta_s_star = star$beta_s_star,
                              beta_o_star = star$beta_o_star, sigma_star = star$sigma_star,
                              rho_star = star$rho_star, pmm = pmm, y = y, ry = ry)
        
        y[!ry & x[, group_name] == as.numeric(i)] <- y.star
      }
    }
  }else{ # Applies imputation on full dataset
    
    star<-star_systematic(Heck_mod, send, oend, family)
    Xm <- X[!ry,]
    
    if (nrow(Xm) != 0) { 
      y.star <- gen_y_star( Xm= Xm, sel_name = sel_name, bos_name = bos_name,
                            out_name =out_name, beta_s_star = star$beta_s_star,
                            beta_o_star = star$beta_o_star, sigma_star = star$sigma_star,
                            rho_star = star$rho_star, pmm = pmm, y = y, ry = ry)
      y[!ry] <- y.star
    }
  }
  
  return(y[!ry])
}


# 0. Define additional functions ----

# F 0.1. CopulaIPD: Apply Binomial or Gaussian model depending on y type

copulaIPD <- function(data, sel, out, family, send) {
  
  fit_ind <- 0 # None model estimable for the cluster
  # A. Estimate Heckman model
  fit <- try(GJRM::gjrm( formula = list(sel, out),
                         data = data,
                         margins = c("probit", ifelse(family=="binomial","probit","N")),
                         Model = "BSS",
                         gamlssfit = TRUE,
                         extra.regI = "sED",
                         parscale = TRUE),
             silent = TRUE)
  
  
  
  if (!any(inherits(fit, "try-error"))) {
    # model is estimable
    ev <- eigen(fit$fit$hessian, symmetric = TRUE, only.values = TRUE)$values
    convh <- min(ev) > 0 # convergence based on hessian positive definiteness
    convg <- max(abs(fit$fit$gradient)) < 10 # convergence based on abs max gradient
    
    #MAR indication 
    CIcon<-summary(fit)$ CItheta 
    MNAR_ind<-!(abs(CIcon[[1]]-CIcon[[2]])<0.001&CIcon[[1]]<0&CIcon[[2]]>0) # exclusion of cases that blow up variance
    
    if (MNAR_ind){
      fit_ind <- 2
      if (convh & convg) {
        # MNAR estimable
        fit_ind <- 1 # Heckman model estimable for the cluster
      }
    }
  }
  
  if( fit_ind ==2){
    fit<-NULL
    gam1 <- try(mgcv::gam(formula=sel,data = data,family = "binomial", method ="REML"))
    gam2 <- try(mgcv::gam(formula=out,data = data,family = family, method ="REML"))
    
    if(!any(inherits(gam1, "try-error"))&!any(inherits(gam2, "try-error"))){
      coefficients <- c(gam1$coefficients,gam2$coefficients)
      if(all(!is.na(coefficients))){
        s     <- ifelse(family != "binomial",1,0) 
        ncol1 <- ncol(gam1$Vp)
        ncol2 <- ncol(gam2$Vp)
        Vb    <- matrix(0,ncol = ncol1+ncol2+1+s, nrow = ncol1+ncol2+1+s)
        Vb[1:ncol1,1:ncol1] <- gam1$Vp
        Vb[(ncol1+1):(ncol1+ncol2),(ncol1+1):(ncol1+ncol2)] <- gam2$Vp
        
        if (family != "binomial") {
          coefficients <- c(coefficients, sigma.star = log(sqrt(gam2$scale)))
          Vb[(ncol1+ncol2+1),(ncol1+ncol2+1)] <- gam2$V.sp}
        
        fit$coefficients <- c(coefficients,theta.star=0)
        fit$Vb  <- Vb
        fit_ind <- 2}
    }
    
  }
  
  if (fit_ind != 0) {
    names <- c(paste0(names(fit$coefficients)[1:send], "_s"),
               names(fit$coefficients[(send + 1):length(names(fit$coefficients))]))
    names(fit$coefficients) <- names
    colnames(fit$Vb) <- names
    rownames(fit$Vb) <- names
  }else{
    fit <- NA}  
  
  return(list(fit, fit_ind))
  
}

# F 0.2. cov_mat_vector: Transform covariance matrix in a ordered vector
cov_mat_vector <- function(cov_mat, vnames) {
  cov_mat[upper.tri(cov_mat)] <- "Up"
  cov_vec <- as.vector(cov_mat[vnames, vnames])
  cov_vec <- as.numeric(cov_vec[cov_vec != "Up"])
  return(cov_vec)
  
}
# F 0.3. draw_theta_psi_mar: Estimate true effect size and draw a marginal theta and psi=var(theta) .

draw_theta_psi_mar <- function(coef_mat_s, Vb_list, meta_method, Mvma_est, vnames = NULL) {
  
  theta_star <- NA
  psi_star <- NA
  
  if (is.null(vnames)) { #use all set of parameters
    vnames <- colnames(coef_mat_s)
  }
  
  # Get covariance matrix
  coef_mat_s <- coef_mat_s[, vnames]
  cov_mat_s <- do.call("rbind", lapply(Vb_list, cov_mat_vector, vnames = vnames))
  
  # Apply multivariate random-effects meta-analysis
  mvma <- suppressWarnings(try(mixmeta::mixmeta(coef_mat_s,cov_mat_s, method = meta_method,
                                                control = list(hessian = TRUE)), silent = TRUE))
  
  if (inherits(mvma,"try-error")) { # Use mm instead
    meta_method = "mm"
    mvma <- suppressWarnings(try(mixmeta::mixmeta(coef_mat_s, cov_mat_s, method = meta_method,
                                                  control = list(hessian = TRUE)),silent = TRUE))
    
    if (inherits(mvma,"try-error")) { # MA can not be estimated
      Mvma_est <- 0}
  }
  
  
  if (Mvma_est == 1) {
    # Draw effects theta_star
    theta_star <- MASS::mvrnorm(n = 1, mu = coef(mvma), Sigma = vcov(mvma))
    
    if (meta_method != "mm") {
      # Draw random effect, psi_star
      if (length(vnames) == 1) {
        qrsh <- 1 / mvma$hessian
      } else {
        Hes <- as.matrix(Matrix::forceSymmetric(mvma$hessian))
        qrsh <- as.matrix(Matrix::nearPD(MASS::ginv(-Hes))$mat)
      }
      
      rpar <- mvtnorm::rmvnorm(1, mean = mvma$par, sigma = qrsh, method = "svd")
      
      if (length(vnames) == 1) {
        psi_star <- rpar ^ 2
      } else {
        psi <- matrix(0, ncol(mvma$Psi), ncol(mvma$Psi))
        psi[lower.tri(psi, diag = TRUE)] <- rpar
        psi_star <- Matrix::tcrossprod(psi)
        
      }
      
    } else # meta_method== reml OR ml
      psi_star <- mvma$Psi
  }
  
  colnames(psi_star) <- names(theta_star)
  rownames(psi_star) <- names(theta_star)
  
  return(list(theta_star, psi_star, Mvma_est))
  
}

# F 0.4 Conditional posterior distribution
draw_cond_theta <- function(theta_mar, theta_k, var_theta_k, vnames) {
  W_m <- MASS::ginv(theta_mar[[2]])
  W_k <- MASS::ginv(var_theta_k[vnames, vnames])
  S <- MASS::ginv(W_m + W_k)
  mu <- S %*% (W_k %*% as.vector(theta_k[vnames]) + W_m %*% as.vector(theta_mar[[1]]))
  theta_star_i <- MASS::mvrnorm(n = 1, mu = as.vector(mu), Sigma = S)
  return(theta_star_i)
}


# F 0.5 Get marginal draws
get_marginal <- function(coef_mat_s, Vb_list, selnam, outnam, meta_method ){
  
  beta_s = beta_o = rho_t = sigma_t = NA
  Mvma_est <- 1
  
  # separate the set of parameters in beta_out, beta_s and rho
    
    beta_o <- draw_theta_psi_mar( coef_mat_s = coef_mat_s,
                                  Vb_list = Vb_list,
                                  vnames = outnam,
                                  meta_method = meta_method,
                                  Mvma_est = Mvma_est)
    beta_s <- draw_theta_psi_mar( coef_mat_s = coef_mat_s,
                                  Vb_list = Vb_list,
                                  vnames = selnam,
                                  meta_method = meta_method,
                                  Mvma_est = beta_o[[3]])
    rho_t <- draw_theta_psi_mar( coef_mat_s = coef_mat_s,
                                 Vb_list = Vb_list,
                                 vnames = "theta.star",
                                 meta_method = meta_method,
                                 Mvma_est = beta_s[[3]]) #copula package calls atanh(rho) as theta.star
    
    if ("sigma.star"%in%colnames(coef_mat_s)) {
      sigma_t <- draw_theta_psi_mar( coef_mat_s = coef_mat_s,
                                     Vb_list = Vb_list,
                                     vnames = "sigma.star",
                                     meta_method = meta_method,
                                     Mvma_est = rho_t[[3]]) #copula package calls log(sigma) as sigma.star
      Mvma_est <- sigma_t[[3]]
    }
    
  
  
  return (list ( beta_s = beta_s,
                 beta_o = beta_o,
                 rho_t = rho_t,
                 sigma_t = sigma_t,
                 Mvma_est = Mvma_est))
}



# F 0.6 Get draw from systematically missing groups
star_systematic <- function(Heck_mod, send, oend, family){
  
  if (is.null(Heck_mod$Mvma_est)){ # From total data model
    star <- mvtnorm::rmvnorm( 1, mean = Heck_mod[[1]]$coefficients,
                              sigma = Heck_mod[[1]]$Vb, method = "svd")
    
    beta_o_star <- star[(send + 1):oend]
    beta_s_star <- star[1:send]
    rho_star <- tanh(star[, "theta.star"])
    sigma_star <- ifelse(family=="gaussian",
                         exp(star[, "sigma.star"]), NA)
    
    
  } else { # From meta model
    
    beta_o_star <- MASS::mvrnorm(n = 1, mu = Heck_mod$beta_o[[1]],Sigma =Heck_mod$beta_o[[2]])
    beta_s_star <- MASS::mvrnorm(n = 1, mu = Heck_mod$beta_s[[1]],Sigma = Heck_mod$beta_s[[2]])
    rho_star <- tanh(MASS::mvrnorm( n = 1, mu = Heck_mod$rho_t[[1]],Sigma = Heck_mod$rho_t[[2]]))
    if (family =="gaussian") {
      sigma_star <- exp(MASS::mvrnorm( n = 1, mu = Heck_mod$sigma_t[[1]],
                                       Sigma = Heck_mod$sigma_t[[2]])) #copula package calls log(sigma) as sigma.star
    } else { #binomial
      sigma_star <- NA}
  }
  
  return (list (beta_s_star = beta_s_star,
                beta_o_star = beta_o_star,
                rho_star = rho_star,
                sigma_star = sigma_star))}


## F 0.7 Get draw from sporadically missing groups
star_sporadic <- function(Heck_mod, coef_list_i, Vb_list_i, selnam, outnam, family){
  
  beta_s_star = beta_o_star = sigma_star = rho_star = NA
  cond <- ifelse(is.null(Heck_mod$Mvma_est),0,Heck_mod$Mvma_est)
  
  if (cond == 1){ # Draw study parameters from conditional distribution
    beta_o_star <- draw_cond_theta( theta_mar = Heck_mod$beta_o, theta_k = coef_list_i,
                                    var_theta_k = Vb_list_i, vnames = outnam)
    beta_s_star <- draw_cond_theta( theta_mar = Heck_mod$beta_s,theta_k = coef_list_i,
                                    var_theta_k = Vb_list_i, vnames = selnam)
    
    rho_star <- tanh( draw_cond_theta( theta_mar = Heck_mod$rho_t,
                                       theta_k = coef_list_i,
                                       var_theta_k = Vb_list_i,
                                       vnames = "theta.star")) #copula package calls atanh(rho) as theta.star
    
    if (family == "gaussian") {
      sigma_star <- exp(draw_cond_theta( theta_mar = Heck_mod$sigma_t,
                                         theta_k = coef_list_i,
                                         var_theta_k = Vb_list_i,
                                         vnames = "sigma.star"))#copula package calls log(sigma) as sigma.star
    }
    
    
  } else { # Draw from study parameters from study distribution
    
    beta_o_star <- MASS::mvrnorm(n = 1,mu = coef_list_i[outnam],Sigma = Vb_list_i[outnam, outnam])
    beta_s_star <- MASS::mvrnorm(n = 1,mu = coef_list_i[selnam],Sigma = Vb_list_i[selnam, selnam])
    rho_star <- tanh(MASS::mvrnorm( n = 1, mu = coef_list_i["theta.star"],
                                    Sigma = Vb_list_i["theta.star", "theta.star"]))
    if (family == "gaussian") {
      sigma_star <- exp(MASS::mvrnorm( n = 1, mu = coef_list_i["sigma.star"],
                                       Sigma = Vb_list_i["sigma.star", "sigma.star"]))#copula package calls log(sigma) as sigma.star
    }
    
  }
  
  return (list ( beta_s_star = beta_s_star,
                 beta_o_star = beta_o_star,
                 rho_star = rho_star,
                 sigma_star = sigma_star))
}


# F 0.8 Generate the imputation values

gen_y_star <- function(Xm, sel_name, bos_name, out_name, beta_s_star, beta_o_star,
                       sigma_star,rho_star, pmm, y, ry) {
  
  XOBO <- data.matrix(Xm[,colnames(Xm) %in% c("Int",bos_name,out_name)]) %*% as.vector(beta_o_star)
  XSBS <- data.matrix(Xm[,colnames(Xm) %in% c("Int",sel_name,bos_name)]) %*% as.vector(beta_s_star)
  
  if (!is.na(sigma_star)) { # normal missing variable
    
    Ratio <- (-stats::dnorm(XSBS) / (stats::pnorm(-XSBS)))
    Ratio[is.na(Ratio) | is.infinite(Ratio)] <- 0.0
    y.star <- XOBO + as.numeric(sigma_star) * as.numeric(rho_star) * Ratio +
      rnorm(nrow(XSBS), 0, sd = sigma_star)
    
    if (pmm == TRUE) {
      idx <- mice::matchindex(y[ry == 1], y.star)
      y.star <- y[ry == 1][idx]
    }
    
  } else { #binomial missing variable
    
    p.star <- pbivnorm::pbivnorm(as.vector(XOBO),-as.vector(XSBS),
                                 -as.numeric(rho_star)) / stats::pnorm(-XSBS)
    p.star[is.na(p.star) | (is.infinite(p.star) & p.star < 0) |p.star < 0.0 |
             p.star == "0"] <- 0.0
    p.star[p.star > 1.0 | p.star == "1" |(is.infinite(p.star) & p.star > 0)] <- 1.0
    
    y.star <-rep(levels(y)[1],nrow(XOBO))
    y.star[runif(nrow(XOBO)) < p.star]<-levels(y)[2]
    
  }
  
  return(y.star)
}
