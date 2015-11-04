#'Conditional imputation helper
#'
#'Sorry, the \code{ifdo()} function is not yet implemented. 
#'@aliases ifdo
#'@param cond
#'@param action
#'@return Currently returns an error message.
#'@author Stef van Buuren, 2012
#'@keywords internal

ifdo <- function(cond, action){
  cat("Function ifdo() not yet implemented.\n")
}

#'Appends specified break to the data
#'
#'A custom function to insert rows in long data with new pseudo-observations 
#'that are being done on the specified break ages. There should be a 
#'column called \code{first} in \code{data} with logical data that codes whether
#'the current row is the first for subject \code{id}. Furthermore,
#'the function assumes that columns \code{age}, \code{occ},
#'\code{hgt.z}, \code{wgt.z} and 
#'\code{bmi.z} are available. This function is used on the \code{tbc} 
#'data in FIMD chapter 9. Check that out to see it in action.
#'@aliases appendbreak
#'@param data A data frame in the long long format
#'@param brk A vector of break ages
#'@param warp.model A time warping model
#'@param id The subject identifier
#'@param typ Label to signal that this is a newly added observation
#'@return A long data frame with additional rows for the break ages
#'@export
appendbreak <- function(data, brk, warp.model = warp.model, id=NULL, typ="pred"){
  k <- length(brk)
  app <- data[data$first,]
  if (!is.null(id)) {
    idx <- app$id %in% id
    app <- app[idx,]
    }
  nap <- nrow(app)

  ## update administrative variables 
  app$first <- FALSE
  app$typ <- typ
  app$occ <- NA
  app <- app[rep(1:nap,length(brk)),]
  
  ## update age variables
  app$age <- rep(brk,each=nap)
  app$age2 <- predict(warp.model,newdata=app)
  X <- bs(app$age,
        knots = brk,
        Boundary.knots = c(brk[1],brk[k]+0.0001),
        degree = 1)
  X <- X[,-(k+1)]
  app[,paste("x",1:ncol(X),sep="")] <- X

  ## update outcome variable (set to missing)
  app[,c("hgt.z","wgt.z","bmi.z")] <- NA
  app <- rbind(data, app)
  data <- app[order(app$id, app$age),]
  return(data)
}

#'Extract broken stick estimates from a \code{lmer} object
#'
#'@param fit An object of class \code{lmer}
#'@return A matrix containing broken stick estimates
#'@author Stef van Buuren, 2012
#'@export
extractBS <- function(fit) {
  siz <- t(lme4::ranef(fit)[[1]]) + lme4::fixef(fit)
  bs <- matrix(siz, nrow=nrow(siz)*ncol(siz), ncol=1)
  return(bs)
}


