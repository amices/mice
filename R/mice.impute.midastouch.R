mice.impute.midastouch <- function(y, ry, x, ridge = 1e-05, midas.kappa = NULL, outout = TRUE, neff = NULL, debug = NULL, ...) {

  #+ auxiliaries +#
  if(!is.null(debug)){midastouch.inputlist <- list(y = y, ry = ry, x = x, omega = NULL)}
  sminx <- .Machine$double.eps^(1/4)

  #+ ensure data format +#
  x <- data.matrix(x)
  storage.mode(x) <- "numeric"
  X <- cbind(1, x)
  y <- as.numeric(y)

  #+ get data dimensions +#
  nobs <- sum(ry) ; nmis <- sum(!ry) ; n <- length(ry)
  obsind <- which(ry) ; misind <- which(!ry)
  m <- ncol(X)
  yobs <- y[obsind]
  Xobs <- X[obsind,,drop=FALSE]
  Xmis <- X[misind,,drop=FALSE]

  #+ P-Step +#
  ##++++ bootstrap
  omega <- bootfunc.plain(nobs)
  if(!is.null(debug)){
    midastouch.inputlist$omega <- omega
    assign(x = "midastouch.inputlist",value = midastouch.inputlist,envir = get(debug))
  }

  ##++++ beta estimation
  CX <- omega * Xobs
  XCX <- crossprod(Xobs,CX)
  if(ridge > 0){    diag(XCX) <- diag(XCX) * (1+c(0,rep(ridge,m-1)))  }

  #= check if any diagonal element is exactly zero  ===========#
  diag0 <- which(diag(XCX) == 0)                            #==#
  if(length(diag0)>0){diag(XCX)[diag0] <- max(sminx,ridge)} #==#
  #============================================================#

  Xy <- crossprod(CX,yobs)
  beta <- solve(XCX,Xy)
  yhat.obs <- c(Xobs %*% beta)

  ##++++ kappa estimation
  if(is.null(midas.kappa)){
    mean.y <- crossprod(yobs,omega)/nobs
    eps <- yobs - yhat.obs
    r2 <- 1 - c(crossprod(omega, eps^2) / crossprod(omega,(yobs - mean.y)^2))
    ##slight deviation from the paper to ensure real results
    ##   paper: a tiny delta is added to the denominator
    ##   R Code: min function is used, note that this correction gets active for r2>.999 only
    midas.kappa <- min((50*r2 / (1-r2))^(3/8),100)
    ##if r2 cannot be determined (eg zero variance in yhat), use 3 as suggested by Siddique / Belin
    if(is.na(midas.kappa)){midas.kappa <- 3}
  }

  #+ I-Step +#
  if(outout){
    ##++++ P-step if out of sample predictions for donors
    ## estimate one model per donor by leave-one-out
    XXarray_pre <- t(t(apply(X = Xobs,MARGIN = 1,FUN = tcrossprod)) * omega)
    ridgeind <- c(1:(m-1))*(m+1)+1
    if(ridge > 0){
      XXarray_pre[ridgeind,] <- XXarray_pre[ridgeind,] * (1+ridge)
    }
    XXarray <- c(XCX) - XXarray_pre

    #= check if any diagonal element is exactly zero =======================#
    diag0 <- which(XXarray[ridgeind,] == 0)                              #==#
    if(length(diag0) > 0){XXarray[ridgeind,][diag0] <- max(sminx,ridge)} #==#
    #=======================================================================#

    Xyarray <- c(Xy) - t(Xobs * yobs * omega)
    BETAarray <- apply(rbind(XXarray,Xyarray),2,function(x,m){solve(a = matrix(head(x,m^2),m),b = tail(x,m))},m=m)
    YHATdon <- rowSums(Xobs * t(BETAarray))
    ## each recipient has nobs different yhats
    YHATrec <- Xmis %*% BETAarray
    ##++++ distance calculations
    dist.mat <- YHATdon - t(YHATrec)
  }else{
    yhat.mis <- c(Xmis %*% beta)
    dist.mat <- yhat.obs - matrix(data = yhat.mis,nrow = nobs,ncol = nmis,byrow = TRUE)
  }

  ##++++ convert distances to drawing probs // ensure real results
  delta.mat <- 1/((abs(dist.mat))^midas.kappa)
  delta.mat <- minmax(delta.mat)
  probs <- delta.mat * omega
  csums <- minmax(colSums(probs,na.rm = TRUE))
  probs <- t(t(probs)/csums)

  #+ calculate neff +#
  if(!is.null(neff)){
    if(!exists("midastouch.neff",envir = get(neff))){assign(x = "midastouch.neff",value = list(),envir = get(neff))}
    midastouch.neff <- get("midastouch.neff",envir = get(neff))
    midastouch.neff[[length(midastouch.neff)+1]] <- mean(1/rowSums((t(delta.mat)/csums)^2))
    assign(x = "midastouch.neff",value = midastouch.neff,envir = get(neff))
  }

  #+ return result +#
  index <- apply(probs,2,sample,x = nobs,size = 1, replace = FALSE)
  yimp <- y[obsind][index]
  return(yimp)
}
