#' Imputation based on Heckman model for Individual Patient Data.
#'
#' Imputes outcome and predictor variables that follow an MNAR mechanism
#' according to Heckman's model and come from a clustered database such as
#' individual participant data.
#' @aliases mice.impute.heckman.ipd heckman.ipd
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
#' @param full If full=TRUE all parameters are fully correlated otherwise the
#' correlation is given within each of the following parameter sets: selection
#' coefficient parameters, outcome coefficient parameters, rho and sigma
#' @param family family of the missing variable {"normal","probit"}
#' @param pmm for gaussian variables if applies predictive mean matching to
#' imputed result: "TRUE","FALSE"
#' @param meta_method meta_analysis estimation method for random effects :
#' "ml" (maximum likelihood), "reml" (restricted maximum likelihood) or "mm"
#' method of moments.
#' @param ... Other named arguments. Not used.
#' @name mice.impute.heckman.ipd
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
#' In case the Heckman model cannot be estimated for a given cluster, missing
#' values in this cluster will be imputed without using the selection bias
#' correction, i.e. a MAR mechanism is assumed for this cluster.
#' Added:
#' @author Julius Center Methods Group UMC, 2022
#' @family univariate imputation functions
#' @keywords datagen
#'
#' @export
#'

mice.impute.heckman.ipd <-
  function(y,ry,x,wy = NULL, type, family, full = FALSE, pmm = FALSE, meta_method,...) {
    install.on.demand("gam", ...)
    install.on.demand("GJRM", ...)
    install.on.demand("Matrix", ...)
    install.on.demand("mixmeta", ...)
    install.on.demand("mvtnorm", ...)
    install.on.demand("pbivnorm", ...)
    
    
    # 1. Define variables and dataset----
    
    # Rename covariates
    colnames(x) <- paste0("x_", 1:length(colnames(x))) #change the covariates name for avoiding conflicts when y is covariate
    bos_name   <-  colnames(x)[type == 1] # names of variables present in both outcome and selection model
    sel_name    <- colnames(x)[type == -3] # names of variables in selection model alone
    out_name    <- colnames(x)[type == -4] # names of variables in outcome model alone
    
    
    # Check if group variable is defined
    if (length(colnames(x)[type == -2]) == 0) { 
      warning("No cluster variable has been provided, a Heckman model will be applied to the whole data set, i.e. without considering the cluster level. ")
      Marg_ind <- 0 # Indicator that indicates if it is applied 0: Heckman full dataset, 1: Heckman at cluster level
    } else {
      group_name  <- colnames(x)[type == -2]
      names.clust <- as.character(unique(x[, group_name]))
      Marg_ind    <- 1
    }
    
    #Define position of selection and outcome in coefficient vector
    order <- c(sel_name, bos_name, out_name)
    send  <- length(sel_name) + length(bos_name) + 1 # Index where selection equation ends in coefficient vector
    oend  <- send + length(out_name) + 1  # Index where outcome equation ends in coefficient vector
    
    # Define outcome and selection equation
    out   <- as.formula(paste0("y", "~", paste(c(bos_name,out_name), collapse = "+")))
    sel   <- as.formula(paste0("ry", "~", paste(c(sel_name, bos_name), collapse = "+")))
    
    # Define data & prediction matrix
    data  <- data.frame(ry, y, x[, order])
    X     <- data.frame(cbind(Int=rep(1, nrow(x)), x[, order]))
    
    # 2. Step 1: Get the theta estimators for each study ----
    
    if (Marg_ind == 1) { #Heckman at cluster level
      df_list   <- split.data.frame(data, x[, group_name]) # Separate data frame by group
      res_total <- suppressWarnings( lapply( df_list, copulaIPD,
                                             sel = sel, out = out, send = send, family = family)) #Calculate parameters for each study
      studytype <- sapply(res_total, function(x) x[[2]]) # Specify missing pattern in studies
      sporadical<- studytype[studytype != 0] # List sporadically missing studies
      fit_list  <- lapply(res_total, function(x) x[[1]]) # Get the parameter estimates
      Marg_ind  <- ifelse(length(fit_list[studytype == 1]) >= 2, 1, 0) # Proceed Heckman at cluster level if there are at least 2 clusters with an estimable Heckman model
    }
    
    # 3. Step 2: Get marginal theta and var(theta) ----
    if (Marg_ind == 1) {
      coef_list  <- lapply(fit_list[studytype != 0], `[[`, c('coefficients')) # Get effect vector for each study
      Vb_list    <- lapply(fit_list[studytype != 0], `[[`, c('Vb')) # Get covariance matrix for each study
      dcoef_list <- lapply(coef_list[sporadical == 1], function(x) as.data.frame(t(x)))
      coef_mat_s <- as.matrix(Reduce(rbind,     # this do the same that rbindlist but without calling data.table package
                                     Map(function(x) {x[, setdiff(unique(unlist(lapply(dcoef_list, colnames))), names(x))] <- NA
                                     return(x)}, dcoef_list)))
      
      # Get the name of variables on each equation
      varnam <- colnames(coef_mat_s)
      selnam <- varnam[grepl("*_s" , varnam)]
      outnam <- varnam[!varnam %in% c(selnam, "sigma.star", "theta.star")]
      
      
      # Draw thetas from the universe of populations
      if (full == TRUE) { # use the entire set of parameters 
        total_mar <- draw_theta_psi_mar( coef_mat_s  = coef_mat_s,
                                         Vb_list     = Vb_list[sporadical == 1],
                                         meta_method = meta_method,
                                         Marg_ind   = Marg_ind)
        Marg_ind <- total_mar[[3]] # if marginal parameters where estimated proceed with Heckman cluster estimation otherwise opt for the Heckman full model 
        
        if (Marg_ind == 1) {
          beta_s_mar <- list(total_mar[[1]][selnam], total_mar[[2]][selnam, selnam])
          beta_o_mar <- list(total_mar[[1]][outnam], total_mar[[2]][outnam, outnam])
          rho_t_mar  <- list(total_mar[[1]]["theta.star"], 
                             total_mar[[2]]["theta.star", "theta.star"])
          if (family == "normal") {
            sigma_t_mar <- list(total_mar[[1]]["sigma.star"], 
                                total_mar[[2]]["sigma.star", "sigma.star"])
          }
        }
        
      } else{ # separate the set of parameters in beta_out, beta_s and rho
        beta_s_mar <- draw_theta_psi_mar( coef_mat_s = coef_mat_s,
                                          Vb_list = Vb_list[sporadical == 1],
                                          vnames = selnam,
                                          meta_method = meta_method,
                                          Marg_ind = Marg_ind)
        beta_o_mar <- draw_theta_psi_mar( coef_mat_s = coef_mat_s,
                                          Vb_list = Vb_list[sporadical == 1],
                                          vnames = outnam,
                                          meta_method = meta_method,
                                          Marg_ind = beta_s_mar[[3]])
        rho_t_mar <- draw_theta_psi_mar( coef_mat_s = coef_mat_s,
                                         Vb_list = Vb_list[sporadical == 1],
                                         vnames = "theta.star",
                                         meta_method = meta_method,
                                         Marg_ind = beta_o_mar[[3]])  #copula package calls atanh(rho) as theta.star
        Marg_ind <- rho_t_mar[[3]]
        if (family == "normal") {
          sigma_t_mar <- draw_theta_psi_mar( coef_mat_s = coef_mat_s,
                                             Vb_list = Vb_list[sporadical == 1],
                                             vnames = "sigma.star",
                                             meta_method = meta_method,
                                             Marg_ind = rho_t_mar[[3]]) #copula package calls log(sigma) as sigma.star
          Marg_ind <- sigma_t_mar[[3]]
        }
        
      }
    }
    
    # 4. Get theta_k and var(theta_k) from full conditional distribution ----
    if (Marg_ind == 1) {  # Applies the Heckman model by cluster
      for (i in names.clust) {  #Loop across studies
        
        if (studytype[[i]] == 0) { #systematically missing
          
          beta_s_star <- MASS::mvrnorm(n = 1, mu = beta_s_mar[[1]],Sigma = beta_s_mar[[2]])
          beta_o_star <- MASS::mvrnorm(n = 1, mu = beta_o_mar[[1]],Sigma = beta_o_mar[[2]])
          rho_star <- tanh(MASS::mvrnorm( n = 1, mu = rho_t_mar[[1]],Sigma = rho_t_mar[[2]]))
          
          if (family == "normal") {
            sigma_star <- exp(MASS::mvrnorm( n = 1, mu = sigma_t_mar[[1]],
                                             Sigma = sigma_t_mar[[2]]))#copula package calls log(sigma) as sigma.star
             } else { #binomial
               sigma_star <- NA
               }
          
        } else if (studytype[[i]] == 2) {  # Only the outcome model is estimable (MAR)
          beta_o_star <- MASS::mvrnorm(n = 1,mu = coef_list[[i]][outnam],Sigma = Vb_list[[i]][outnam, outnam])
          if (family == "normal") {
            sigma_star <- exp(MASS::mvrnorm( n = 1, mu = coef_list[[i]]["sigma.star"],
                                             Sigma = Vb_list[[i]]["sigma.star", "sigma.star"]))#copula package calls log(sigma) as sigma.star
             } else { #binomial
               sigma_star <- NA
               }
          rho_star <- 0
          
          
         } else {# sporadically missing
       
          beta_s_star <- draw_cond_theta( theta_mar = beta_s_mar,theta_k = coef_list[[i]],
                                          var_theta_k = Vb_list[[i]], vnames = selnam)
          beta_o_star <- draw_cond_theta( theta_mar = beta_o_mar, theta_k = coef_list[[i]],
                                          var_theta_k = Vb_list[[i]], vnames = outnam)
          if (family == "normal") {
            sigma_star <- exp(draw_cond_theta( theta_mar = sigma_t_mar,
                                               theta_k = coef_list[[i]],
                                               var_theta_k = Vb_list[[i]],
                                               vnames = "sigma.star"))#copula package calls log(sigma) as sigma.star
             } else{ # binomial
               sigma_star <- NA
               }
          rho_star <- tanh( draw_cond_theta( theta_mar = rho_t_mar,
                                             theta_k = coef_list[[i]],
                                             var_theta_k = Vb_list[[i]], 
                                             vnames = "theta.star")) #copula package calls atanh(rho) as theta.star
        }
        
        
        Xm <- X[!ry & x[, group_name] == as.numeric(i),]
        
        if (nrow(Xm) != 0) { #Cluster with at least one missing value in outcome equation
          y.star <- gen_y_star( Xm= Xm, sel_name = sel_name, bos_name = bos_name,
                                out_name =out_name, beta_s_star = beta_s_star, 
                                beta_o_star = beta_o_star, sigma_star = sigma_star,
                                rho_star = rho_star, studytype = studytype[[i]],
                                family = family, pmm = pmm, y = y, ry = ry)
          
          y[!ry & x[, group_name] == as.numeric(i)] <- y.star
        }
      }
    }
    
    if (Marg_ind == 0) { # Applies the Heckman model on entire dataset
      
      warning("There is not enough information to impute at the study level, so the imputation is based on the total database without study specification.")
      res <- copulaIPD( data = data, sel = sel, out = out, family = family, send = send)
      
      if (res[[2]] != 1){   # model is not estimable
       
        stop("There is not enough infomation to impute the Heckman model at marginal or study level")
      }
      
      mar.star <- mvtnorm::rmvnorm( 1, mean = res[[1]]$coefficients, 
                                    sigma = res[[1]]$Vb, method = "svd")
      beta_s_star <- mar.star[1:send]
      beta_o_star <- mar.star[(send + 1):oend]
      sigma_star <- ifelse("sigma.star" %in% colnames(mar.star), 
                           exp(mar.star[, "sigma.star"]), NA)
      rho_star <- tanh(mar.star[, "theta.star"])
      Xm <- X[!ry,]
      y.star <- gen_y_star(Xm= Xm, sel_name = sel_name, bos_name = bos_name , 
                           out_name =out_name, beta_s_star = beta_s_star, 
                           beta_o_star = beta_o_star, sigma_star = sigma_star,
                           rho_star = rho_star, studytype = studytype, 
                           family = family, pmm = pmm, y = y, ry = ry)
      y [!ry] <- y.star
    }
    return(y[!ry])
  }



# 0. Define additional functions ----

# F 0.1. CopulaIPD: Apply Binomial or Gaussian model depending on y type

copulaIPD <- function(data, sel, out, family, send) {
  family <- ifelse(family=="probit","probit","N") # According to GJRM family specfication 
  fit_ind <- 0 # None model estimable for the cluster
  
  # A. Estimate Heckman model 
  fit <- try(GJRM::gjrm( formula = list(sel, out),
                         data = data,
                         margins = c("probit", family),
                         Model = "BSS",
                         gamlssfit = TRUE,
                         extra.regI = "sED",
                         parscale = TRUE),
             silent = TRUE)
  
  if (!any(inherits(fit, "try-error"))) {
    # model is estimable
    ev <- eigen(fit$fit$hessian, symmetric = TRUE, only.values = TRUE)$values
    convh <- min(ev) > 0  # convergence based on hessian positive definiteness
    convg <- max(abs(fit$fit$gradient)) < 0.01 # convergence based on abs max gradient
    
    if (convh & convg) {
      # MNAR estimable
      fit_ind <- 1 # Heckman model estimable for the cluster
      names <- c(paste0(names(fit$coefficients)[1:send], "_s"), 
                 names(fit$coefficients[(send + 1):length(names(fit$coefficients))]))
      names(fit$coefficients) <- names
      colnames(fit$Vb) <- names
      rownames(fit$Vb) <- names
    }
  }
  
  #B. If Heckamn is not estimable, estimate outcome model alone
  if (fit_ind == 0) { 
    fit <- try(gam::gam(data = data, formula = out,
                        family = ifelse(family == "probit","binomial","gaussian")), # According to gam family specfication 
               silent = TRUE)
    
    if (!any(is.na(fit$coefficients))) {
      fit_ind <- 2 # Only outcome model estimable for the cluster
      fit$Vb  <- stats::vcov(fit)
      
      if (family != "probit") {
        fit$coefficients["sigma.star"] <- log(stats::sigma(fit))
        fit$Vb <- cbind(fit$Vb, sigma.star = rep(0, nrow(fit$Vb)))
        fit$Vb <- rbind(fit$Vb, sigma.star = c(rep(0, nrow(fit$Vb)),
                                               2 / (fit$df.residual - 4)))
      }
    }else{ # not enough info to fit any model fit_ind = 0
      fit <- NA
    }
  }
  
 
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

draw_theta_psi_mar <- function(coef_mat_s, Vb_list, meta_method, vnames = NULL, Marg_ind) {
  
  theta_star <- NA
  psi_star <- NA
  
  if (is.null(vnames)) { #use all set of parameters
    vnames <- colnames(coef_mat_s) 
  }
  
  # Get covariance matrix
  coef_mat_s <- coef_mat_s[, vnames]
  cov_mat_s  <- do.call("rbind", lapply(Vb_list, cov_mat_vector, vnames = vnames))
  
  # Apply multivariate random-effects meta-analysis
  
  if (Marg_ind == 1) {
    mvma <- try(mixmeta::mixmeta(coef_mat_s,cov_mat_s, method = meta_method,
                                 control = list(hessian = TRUE)), silent = TRUE)
    
  
    if (inherits(mvma,"try-error")) {  # Use mm instead
      meta_method = "mm"
      mvma <- try(mixmeta::mixmeta(coef_mat_s, cov_mat_s, method = meta_method, 
                                   control = list(hessian = TRUE)),silent = TRUE)
      
    
      if (inherits(mvma,"try-error")) {  # MA can not be estimated
        Marg_ind <- 0
      }
    }
  }
  
  if (Marg_ind == 1) {
    # Draw effects theta_star
    theta_star <- MASS::mvrnorm(n = 1, mu = coef(mvma), Sigma = vcov(mvma))
    
    if (meta_method != "mm") {
      # Draw random effect, psi_star
      if (length(vnames) == 1) {
        qrsh <- 1 / mvma$hessian
      } else {
        Hes <- as.matrix(Matrix::forceSymmetric(mvma$hessian))
        qrsh <- as.matrix(Matrix::nearPD(qr.solve(-Hes))$mat)
      }
      rpar <- mvtnorm::rmvnorm(1, mean = mvma$par, sigma = qrsh, method = "svd")
      
      if (length(vnames) == 1) {
        psi_star <- rpar ^ 2
      } else {
        psi <- matrix(0, ncol(mvma$Psi), ncol(mvma$Psi))
        psi[lower.tri(psi, diag = TRUE)] <- rpar
        psi_star <- Matrix::tcrossprod(psi)
      }
      
    } else  # meta_method== reml OR ml
     
      psi_star <- mvma$Psi
  }
  
  return(list(theta_star, psi_star, Marg_ind))
  
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

# F 0.5 Generate the imputed values

gen_y_star <- function(Xm, sel_name, bos_name, out_name, beta_s_star, beta_o_star,
                       sigma_star,rho_star, studytype, family, pmm, y, ry) {
  
  XOBO <- data.matrix(Xm[,colnames(Xm) %in% c("Int",bos_name,out_name)]) %*% as.vector(beta_o_star)
  
  if (studytype != 2) { #No MAR
    XSBS <- data.matrix(Xm[,colnames(Xm) %in% c("Int",sel_name,bos_name)]) %*% as.vector(beta_s_star)
  }
  
  if (family == "normal") { # normal missing variable
    if (studytype != 2) {    # Heckman
      Ratio <- (-stats::dnorm(XSBS) / (stats::pnorm(-XSBS)))
      Ratio[is.na(Ratio) | is.infinite(Ratio)] <- 0.0
      y.star <- XOBO + as.numeric(sigma_star) * as.numeric(rho_star) * Ratio + 
        rnorm(nrow(XSBS), 0, sd = sigma_star)
      
    } else { # MAR
      y.star <- XOBO + rnorm(nrow(XOBO), 0, sd = sigma_star)
    }
    
    if (pmm == TRUE) {
      idx <- mice::matchindex(y[ry == 1], y.star)
      y.star <- y[ry == 1][idx]
    }
    
  } else { #binomial missing varible
    if (studytype != 2) { # Heckman
  
      p.star <- pbivnorm::pbivnorm(as.vector(XOBO),-as.vector(XSBS),
                                   -as.numeric(rho_star)) / stats::pnorm(-XSBS)
      p.star[is.na(p.star) | (is.infinite(p.star) & p.star < 0) |p.star < 0.0 | 
               p.star == "0"] <- 0.0
      p.star[p.star > 1.0 | p.star == "1" |(is.infinite(p.star) & p.star > 0)] <- 1.0
      
    } else { # MAR
      expXOBO <- exp(XOBO)
      p.star <- expXOBO / (1 + expXOBO)
      p.star[(is.infinite(expXOBO) & expXOBO < 0)] <- 0
      p.star[(is.infinite(expXOBO) & expXOBO > 0)] <- 1
    }
    
    y.star <- rbinom(n = nrow(XOBO), size = 1, prob = p.star)
    
  }
  
  return(y.star)
}