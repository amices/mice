### auxiliary.r
### functions needed for FIMD


long2mids <- function(x){
  cat("Function long2mids() not yet implemented.\n")
}

ifdo <- function(cond, action){
  cat("Function ifdo() not yet implemented.\n")
}

### append break ages
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

###
extractBS <- function(fit) {
  siz <- t(ranef(fit)[[1]]) + fixef(fit)
  bs <- matrix(siz, nrow=nrow(siz)*ncol(siz), ncol=1)
  return(bs)
}


