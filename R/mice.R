# MICE V2.15 (mar2013)
#
#    R package MICE: Multivariate Imputation by Chained Equations
#    Copyright (c) 1999-2013 TNO, Leiden
#	
#	 This file is part of the R package MICE.
#
#    MICE is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#   
#   Authors:    
#           S. van Buuren, C.G.M. Groothuis-Oudshoorn 
#           TNO Quality of Life, Leiden
#           The Netherlands
#   with contributions of John Fox, Frank E. Harrell, Roel de Jong, 
#                         Jason Turner, Martijn Heijmans, Peter Malewski,
#                         Gerko Vink, Alexander Robitzsch
##
#   ## SVB: changes by Stef van Buuren
#   ## FEH: changes by Frank E. Harrell 
#   ## PM:  changes by Peter Malewski
#   ## RJ:  changes by Roel de Jong
# 
#   Please report any bugs to stef.vanbuuren@tno.nl
#


# Define settings

#---------------------- MICE ------------------------------------------------

mice <- function(data,
    m = 5,
    method = vector("character",length=ncol(data)),
    predictorMatrix = (1 - diag(1, ncol(data))),
    visitSequence = (1:ncol(data))[apply(is.na(data),2,any)],
    post = vector("character",length=ncol(data)),
    defaultMethod = c("pmm","logreg","polyreg","polr"),
    maxit = 5,
    diagnostics = TRUE,
    printFlag = TRUE,
    seed = NA,
    imputationMethod = NULL,
    defaultImputationMethod = NULL,
    data.init = NULL,
    ...
)

{
#   MICE - Multivariate Imputation by Chained Equations
#
#   Main algorithm for imputing datasets.
#   Authors: K. Groothuis-Oudshoorn and S. van Buuren
#            TNO Quality of Life, Leiden
#            The Netherlands
##
#   Copyright (c) 2010 TNO Quality of Life, Leiden
#
  ##------------------------------CHECK.VISITSEQUENCE------------------------
  check.visitSequence<-function(setup){
    nmis <- setup$nmis
    nvar <- setup$nvar
    visitSequence <- setup$visitSequence
    if(!is.numeric(visitSequence)) {
      code <- pmatch(visitSequence,c("roman","arabic","monotone","revmonotone"))
      if(!is.na(code) && code==1) visitSequence <- (1:nvar)[nmis>0]
      if(!is.na(code) && code==2) visitSequence <- rev((1:nvar)[nmis>0])
      if(!is.na(code) && code==3) visitSequence <- order(nmis)[(sum(nmis==0)+1):length(nmis)]  # SvB sept 2011
      if(!is.na(code) && code==4) visitSequence <- rev(order(nmis)[(sum(nmis==0)+1):length(nmis)])
      if(is.na(code)) stop("Argument visitSequence not recognized.\n")
    }
    if(all(nmis[visitSequence] == 0)) stop(paste("No missing values found."))
    flags <- nmis==0 & is.element(1:nvar,visitSequence)
    if (any(flags)) visitSequence <- visitSequence[!flags]
    visitSequence <- visitSequence[visitSequence <= nvar]
    visitSequence <- visitSequence[visitSequence >= 1]
    if (length(visitSequence)==0) stop(paste("No missing values found."))
    setup$visitSequence <- visitSequence
    return(setup)
  }

  ##------------------------------CHECK.predictorMatrix-------------------------------
  check.predictorMatrix <- function(setup){
    ##  checks the predictorMatrix
    ##  makes consistency edits of the predictormatrix
    ##
    pred <- setup$predictorMatrix
    varnames <- setup$varnames
    nmis <- setup$nmis
    nvar <- setup$nvar
    vis  <- setup$visitSequence
    method <- setup$method
    post <- setup$post
      
    if(!is.matrix(pred)) stop("Argument 'predictorMatrix' not a matrix.")
    if(nvar != nrow(pred) | nvar != ncol(pred))
      stop(paste("The predictorMatrix has",nrow(pred),"rows and",
                 ncol(pred),"columns. Both should be",nvar,"."))
    dimnames(pred) <- list(varnames, varnames)
    diag(pred) <- 0
    for(j in 1:nvar) {
      if(method[j]=="" & any(pred[,j]!=0) & nmis[j]>0) {
        out <- varnames[j]
        updateLog(out=out)
        pred[,j] <- 0
        vis <- vis[vis!=j]
        post[j] <- ""
      }
      if(nmis[j]==0 & any(pred[j,]!=0))
        pred[j,] <- 0
    }
    
    setup$predictorMatrix <- pred
    setup$visitSequence <- vis
    setup$post <- post
    return(setup)
  }
  
  ##------------------------------CHECK.method-------------------------------
  
  check.method <- function(setup, data) {
    ##
    ##  check method, set defaults if appropriate
    ##
    method <- setup$method
    defaultMethod <- setup$defaultMethod
    visitSequence <- setup$visitSequence
    nmis <- setup$nmis
    nvar <- setup$nvar
    
    if(all(method == "")) { # the default argument
      for(j in visitSequence) {
        y <- data[,j]
        if(is.numeric(y)) method[j] <- defaultMethod[1]
        else if(nlevels(y) == 2) method[j] <- defaultMethod[2]
        else if(is.ordered(y) & nlevels(y) > 2) method[j] <- defaultMethod[4]
          else if(nlevels(y)  > 2) method[j] <- defaultMethod[3]
          else if(is.logical(y)) method[j] <- defaultMethod[2]  # SvB 18/1/2010
          else method[j] <- defaultMethod[1]
      }
    }
    ##
    ##   expand user's imputation method to all visited columns
    ##
    ## single string supplied by user (note implicit assumption of two columns)
    if(length(method) == 1) {
      if(is.passive(method)) stop("Cannot have a passive imputation method for every column.")
      method <- rep(method, nvar)
    }
    ##
    ##  if user specifies multiple methods, check the length of the argument
    ##
    if(length(method)!=nvar)
      stop(paste("The length of method (",length(method),
                 ") does not match the number of columns in the data (",nvar,").",sep=""))
    ##
    ##   check whether the elementary imputation methods are actually available on the search path
    ##
    active <- !is.passive(method) & nmis > 0 & !(method=="")
    ## BEGIN patch by Gerko Vink, 22sep2011
    passive.check <- is.passive(method) & nmis > 0 & !(method == "")     
    check <- all(active==FALSE) & any(passive.check!=FALSE)                
    if (check) fullNames <- rep("mice.impute.passive", length(method[passive.check]))
    else fullNames <- paste("mice.impute", method[active], sep = ".")
    ## END patch
    notFound <- !sapply(fullNames, exists, mode="function", inherit=TRUE) ## SVB 6 Feb 2004
    if (any(notFound)) stop(paste("The following functions were not found:",
                                  paste(fullNames[notFound],collapse=", ")))
    ##
    ##   type checks on built-in imputation methods
    ##   Added SvB June 2009
    
    for(j in visitSequence) {
      y <- data[,j]
      vname <- dimnames(data)[[2]][j]
      mj <- method[j]
      mlist <- list(
        m1 = c("logreg","logreg.boot","polyreg","lda","polr"),
        m2 = c("norm","norm.nob","norm.predict","norm.boot","mean",
               "2l.norm","2L.norm","2l.pan","2L.pan","2lonly.pan",
               "quadratic"),
        m3 = c("norm","norm.nob","norm.predict","norm.boot","mean",
               "2l.norm","2L.norm","2l.pan","2L.pan","2lonly.pan",
               "quadratic","logreg","logreg.boot")
      )
      
      if(is.numeric(y) & (mj %in% mlist$m1))
        warning("Type mismatch for variable ", vname,
                "\nImputation method ",mj," is for categorical data.",
                "\nIf you want that, turn variable ", vname," into a factor,",
                "\nand store your data in a data frame.",
                call.=FALSE)
      
      else if(is.factor(y) & nlevels(y)==2 & (mj %in% mlist$m2))
        warning("Type mismatch for variable ", vname,
                "\nImputation method ",mj," is not for factors.",
                call.=FALSE)
      
      else if(is.factor(y) & nlevels(y)>2 & (mj %in% mlist$m3))
        warning("Type mismatch for variable ", vname,
                "\nImputation method ",mj," is not for factors with three or more levels.",
                call.=FALSE)
    }
      
    setup$method <- method
    return(setup)
  }
  ##------------------------------CHECK.data-------------------------------
  
  check.data <- function(setup, data, allow.na=FALSE, ...){
    
    pred <- setup$predictorMatrix
    nvar <- setup$nvar
    varnames <- setup$varnames
    meth <- setup$method
    vis <- setup$visitSequence

    ## remove constant variables
    ## but leave passive variables untouched
    for(j in 1:nvar) {
      if (!is.passive(meth[j])){
        v <- var(data[,j], na.rm=TRUE)
        if (allow.na) {
          constant <- FALSE                                            # SvB 10/3/2011
          if (!is.na(v)) constant <- (v < 1000 * .Machine$double.eps)  # SvB 10/3/2011
        } else constant <- is.na(v) | v < 1000 * .Machine$double.eps
        didlog <- FALSE
        if (constant & any(pred[,j]!=0)) {
          out <- varnames[j]
          pred[,j] <- 0
          updateLog(out=out, meth="constant")
          didlog <- TRUE
        }
        if (constant & meth[j]!=""){
          out <- varnames[j]
          pred[j,] <- 0
          if (!didlog) updateLog(out=out, meth="constant")
          meth[j] <- ""
          vis <- vis[vis!=j]
          post[j] <- ""
        }
      }
    }

    ## remove collinear variables
    ispredictor <- apply(pred!=0, 2, any)     # SvB 16/3/11
    if (any(ispredictor)) droplist <- find.collinear(data[,ispredictor,drop=FALSE], ...)
    else droplist <- NULL
    if (length(droplist)>0) {
      for (k in 1:length(droplist)) {
        j <- which(varnames %in% droplist[k])
        didlog <- FALSE
        if (any(pred[,j]!=0)) { # remove as predictor
          out <- varnames[j]
          pred[,j] <- 0
          updateLog(out=out, meth="collinear")
          didlog <- TRUE
        }
        if (meth[j]!=""){
          out <- varnames[j]
          pred[j,] <- 0
          if (!didlog) updateLog(out=out, meth="collinear")
          meth[j] <- ""
          vis <- vis[vis!=j]
          post[j] <- ""
        }
      }
    }
        
    setup$predictorMatrix <- pred
    setup$visitSequence <- vis
    setup$post <- post
    setup$meth <- meth
    return(setup)
  }

  ##   Start with some preliminary calculations and error checks
  call <- match.call()
  if(!is.na(seed)) set.seed(seed)  ## FEH 1apr02
  if(!(is.matrix(data) | is.data.frame(data)))
    stop("Data should be a matrix or data frame")
  if ((nvar <- ncol(data)) < 2)
    stop ("Data should contain at least two columns")
  data <- as.data.frame(data)
  nmis <- apply(is.na(data),2,sum)
  if (sum(nmis)==0) stop("No missing values found")
  varnames <- dimnames(data)[[2]]

  ## list for storing current computational state
  state <- list(it = 0, im = 0, co = 0, dep = "", meth = "", log = FALSE)
  
  ## data frame for storing the event log
  loggedEvents <- data.frame(it=0, im=0, co=0, dep="", meth="", out="")
  
  ##   Perform various validity checks on the specified arguments
  if (!is.null(imputationMethod)) method <- imputationMethod
  if (!is.null(defaultImputationMethod)) defaultMethod <- defaultImputationMethod    

  setup <- list(visitSequence = visitSequence,
                method = method,
                defaultMethod = defaultMethod,
                predictorMatrix = predictorMatrix,
                post = post,
                nvar = nvar,
                nmis = nmis,
                varnames = varnames)
  setup <- check.visitSequence(setup)
  setup <- check.method(setup, data)
  setup <- check.predictorMatrix(setup)
  setup <- check.data(setup, data, ...)

  ##   Pad the imputation model with dummy variables for the factors
  method <- setup$method
  predictorMatrix <- setup$predictorMatrix
  visitSequence <- setup$visitSequence
  post <- setup$post
  p <- padModel(data, method, predictorMatrix, visitSequence, post,
                nmis, nvar)
  if(sum(duplicated(names(p$data))) > 0) stop("Column names of padded data should be unique")
  
  ##   Initialize response matrix r, imputation array imp, as well as some other stuff
  r <- (!is.na(p$data))
  imp <- vector("list", ncol(p$data))
  if(m > 0) {
    
    ## Initializes the imputed values
    for(j in visitSequence) {
      imp[[j]] <- as.data.frame(matrix(NA, nrow = sum(!r[,j]), ncol = m))
      dimnames(imp[[j]]) <- list(row.names(data)[r[,j] == FALSE], 1:m)
      y <- data[,j]
      ry <- r[,j]
      if (method[j]!="") {     # for incomplete variables that are imputed
        for(i in 1:m) {
          if (nmis[j]<nrow(data)) {  
              # begin 25jun2012
              if ( is.null(data.init) ){ 
                  imp[[j]][,i] <- mice.impute.sample(y, ry, ...)
              } else {
                  imp[[j]][,i] <- data.init[ ! ry , j ]
              }
              # end 25jun2012
          }
          else imp[[j]][,i] <- rnorm(nrow(data))
        }
      }
    }
  }

  # OK. Iterate.
  from <- 1
  to <- from + maxit - 1
  q <- sampler(p, data, m, imp, r, visitSequence, c(from,to), printFlag,...)
  
  ## restore the original NA's in the data
  for(j in p$visitSequence) p$data[(!r[,j]),j] <- NA
  
  ## delete data and imputations of automatic dummy variables
  imp <- q$imp[1:nvar]
  names(imp) <- varnames
  names(method) <- varnames
  names(post) <- varnames
  names(visitSequence) <- varnames[visitSequence]
  if (!state$log) loggedEvents <- NULL
  if (state$log) row.names(loggedEvents) <- 1:nrow(loggedEvents)
  
  ## save, and return
  midsobj <- list(call = call, data = as.data.frame(p$data[,1:nvar]),
                  m = m, nmis = nmis, imp = imp,
                  method = method,
                  predictorMatrix = predictorMatrix,
                  visitSequence = visitSequence,
                  post = post,
                  seed = seed,
                  iteration = q$iteration,
                  lastSeedValue = .Random.seed,
                  chainMean = q$chainMean, chainVar = q$chainVar,
                  loggedEvents = loggedEvents)
  if (diagnostics) midsobj <- c(midsobj, list(pad = p))
  oldClass(midsobj) <- "mids"
  return(midsobj)
} # END OF MICE FUNCTION

#---------------------- MICE.MIDS -------------------------------------------

mice.mids <- function(obj, maxit=1, diagnostics = TRUE, printFlag = TRUE, ...)
{
# MICE.MIDS - 
# MICE algorithm that takes mids object as input, iterates maxit 
# iteration and produces another mids object as output.
  if (!is.mids(obj)) stop("Object should be of type mids.")
  if (maxit < 1) return(obj)

  call <- match.call()
  nvar <- ncol(obj$data)
  sumIt <- obj$iteration + maxit
  varnames <- dimnames(obj$data)[[2]]

  from <- obj$iteration + 1
  to <- from + maxit - 1

  loggedEvents <- obj$loggedEvents   
  state <- list(it = 0, im = 0, co = 0, dep = "", meth = "",
                log = !is.null(loggedEvents))
  if (is.null(loggedEvents))
    loggedEvents <- data.frame(it=0, im=0, co=0, dep="", meth="", out="")

  ## make a suitable comment here
  if (is.null(obj$pad)) p <- padModel(obj$data,
                                      obj$method,
                                      obj$predictorMatrix,
                                      obj$visitSequence,
                                      obj$post,
                                      obj$nmis,
                                      nvar)
  else p <- obj$pad
  r <- (!is.na(p$data))
  imp <- vector("list", ncol(p$data))
  for(j in obj$visitSequence) imp[[j]] <- obj$imp[[j]]
  assign(".Random.seed", obj$lastSeedValue, pos=1) ##pm 04/02
 
  ## OK. Iterate.
  q <- sampler(p, obj$data, obj$m, imp, r, obj$visitSequence, c(from, to), printFlag, ...)
  
  ## restore the original NA's in the data
  for(j in p$visitSequence) p$data[(!r[,j]),j] <- NA
  
  ##   delete data and imputations of automatic dummy variables
  data <- p$data[,1:nvar]
  imp <- q$imp[1:nvar]
  names(imp) <- varnames

  ## combine with previous chainMean and chainVar
  nvis <- length(obj$visitSequence)
  vnames <- varnames[obj$visitSequence]
  chainMean <- chainVar <-
    array(0, dim=c(nvis, to, obj$m),
          dimnames=list(vnames, 1:to, paste("Chain",1:obj$m)))
  for(j in 1:nvis) {
    if (obj$iteration==0) {
      chainMean[j,,] <- q$chainMean[j,,]
      chainVar[j,,] <- q$chainVar[j,,]
    } else {
      chainMean[j,1:obj$iteration,] <- obj$chainMean[j,,]
      chainVar[j,1:obj$iteration,] <- obj$chainVar[j,,]
      chainMean[j,from:to,] <- q$chainMean[j,,]   
      chainVar[j,from:to,] <- q$chainVar[j,,]
    }
  }
  
  if (!state$log) loggedEvents <- NULL
  if (state$log) row.names(loggedEvents) <- 1:nrow(loggedEvents)
  
  ## save, and return  
  midsobj <- list(call = call, data = as.data.frame(data), 
                  m = obj$m, nmis = obj$nmis, imp = imp, 
                  method = obj$method,
                  predictorMatrix = obj$predictorMatrix, 
                  visitSequence = obj$visitSequence, 
                  post = obj$post,
                  seed = obj$seed, 
                  iteration = sumIt, 
                  lastSeedValue = .Random.seed,
                  chainMean = chainMean, chainVar = chainVar,
                  loggedEvents = loggedEvents) 
  if (diagnostics) midsobj <- c(midsobj, list(pad = p))
  oldClass(midsobj) <- "mids"
  return(midsobj)
} # END OF MICE.MIDS FUNCTION


#------------------------------PadModel-------------------------------

padModel <- function(data, method, predictorMatrix, visitSequence, post,
                 nmis, nvar)
{
#   Called by mice().
#   Augments the imputation model by including dummy variables. Adapts data, predictorMatrix,
#   method and visitSequence.
#   Returns a list whose components make up the padded model.
#
   categories<-data.frame(
      yes.no.categorical=factor(rep(FALSE,nvar),levels=c("TRUE","FALSE")),number.of.dummies=rep(0,nvar),
      yes.no.dummy=factor(rep(FALSE,nvar),levels=c("TRUE","FALSE")),corresponding.column.dummy=rep(0,nvar))

    # zero is default in corresponding.column.dummy for no dummy variable
   for(j in 1:nvar) {
       # if(is.factor(data[,j]) && any(predictorMatrix[,j]==1)) {
       if(is.factor(data[,j]) && any(predictorMatrix[,j]!=0)) {   ## SvB 11mar2013 account other roles for multilevel data
       categories[j,1]<-TRUE
       ## data[,j]<-C(data[,j],treatment) #assign contrast-attribute, choice treatment, to factor
       data[,j]<-C(data[,j],contr.treatment) #assign contrast-attribute, choice treatment, to factor   SvB 14/12/08
       n.dummy <- length(levels(data[,j])) - 1
       categories[j,2]<-n.dummy
       predictorMatrix <- rbind(predictorMatrix, matrix(0, ncol = ncol(predictorMatrix), nrow = n.dummy))
       predictorMatrix <- cbind(predictorMatrix, matrix(rep(predictorMatrix[,j],times=n.dummy), ncol = n.dummy))
       predictorMatrix[1:nvar,j] <- rep(0, times = nvar)
       ## if (any(predictorMatrix[j,]==1)){  condition outcommented June 30, 2009 SvB to allow for intercept imputation of categorical variables
       if (any(visitSequence==j)){  # if j is imputed, changed June 30, 2009 SvB
         predictorMatrix[(ncol(predictorMatrix)-n.dummy+1):ncol(predictorMatrix),j]<-rep(1,times=n.dummy)
         ## changed to a loop to correct error in append function when a cat pred is visited more than once SvB Aug 2009
         newcol <- ncol(predictorMatrix) - n.dummy + 1
         nloops <- sum(visitSequence==j)
         for (ii in 1:nloops) {
           idx <- (1:length(visitSequence))[visitSequence==j][ii]
           visitSequence <- append(visitSequence, newcol, idx)
         }
       }
       data<-(cbind(data,matrix(0,ncol=n.dummy,nrow=nrow(data))))
       data[is.na(data[,j]),(ncol(predictorMatrix)-n.dummy+1):ncol(predictorMatrix)]<-NA
       ## PM
       cat.column <- data[!is.na(data[, j]), j]
       data[!is.na(data[,j]),(ncol(predictorMatrix)-n.dummy+1):ncol(predictorMatrix)]<-model.matrix(~cat.column-1)[,-1]
       names(data)[(ncol(predictorMatrix)-n.dummy+1):ncol(predictorMatrix)]<-paste(attr(data,"names")[j],(1:n.dummy),sep=".")
       method <- c(method, rep("dummy", n.dummy))
       post <- c(post, rep("", n.dummy))   # added SvB Aug 2009
       categories<-rbind(categories,data.frame(yes.no.categorical=rep(FALSE,n.dummy),
                         number.of.dummies=rep(0,n.dummy),
                         yes.no.dummy=rep(TRUE,n.dummy),
                         corresponding.column.dummy=rep(j,n.dummy)))
     }
   }

   varnames <- dimnames(data)[[2]]
   dimnames(predictorMatrix) <- list(varnames,varnames)
   names(method) <- varnames
   names(post)   <- varnames
   names(visitSequence) <- varnames[visitSequence]
   dimnames(categories)[[1]] <- dimnames(data)[[2]]
   return(list(data=as.data.frame(data),predictorMatrix=predictorMatrix,method=method,visitSequence=visitSequence,post=post,categories=categories))
}


#------------------------------sampler-------------------------------

sampler <- function(p, data, m, imp, r, visitSequence, fromto, printFlag, ...)
#
#   The sampler controls the actual Gibbs sampling iteration scheme
#   This function is called by mice and mice.mids
#
#   Authors: S van Buuren, K Groothuis-Oudshoorn
#   Copyright (c) 1999-2008 TNO Quality of Life
{
  ## set up array for convergence checking
  from <- fromto[1]
  to <- fromto[2]
  maxit <- to - from + 1
  if (maxit>0) chainVar <- chainMean <- array(0, dim=c(length(visitSequence),maxit,m),dimnames=list(dimnames(data)[[2]][visitSequence],1:maxit,paste("Chain",1:m)))
  else chainVar <- chainMean <- NULL

  ## THE ITERATION MAIN LOOP: GIBBS SAMPLER
  if (maxit<1) iteration <- 0
  else {
    if (printFlag) cat("\n iter imp variable")
    for(k in from:to) {  #begin k loop : iteration loop
      iteration <- k
      for(i in 1:m) { #begin i loop    : repeated imputation loop
        if (printFlag) cat("\n ",iteration," ",i)
        
        ## fill the data with the last set of imputations
        for (j in visitSequence) p$data[!r[,j],j] <- imp[[j]][,i]
            
        ## augment the data with the actual dummy variables
        for (j in setdiff(p$visitSequence,visitSequence)){
          cat.columns <- p$data[, p$categories[j, 4]]
          p$data[,(j:(j+p$categories[p$categories[j,4],2]-1))] <- matrix((model.matrix(~cat.columns-1)[,-1]),ncol=p$categories[p$categories[j,4],2],nrow=nrow(p$data)) 
        }
        
        ## iterate once over the variables of the augmented model
            
        for(j in p$visitSequence) {
          theMethod <- p$method[j]
          vname <- dimnames(p$data)[[2]][j]

          ## store current state
          oldstate <- get("state", pos=parent.frame())
          newstate <- list(it = k,
                     im = i,
                     co = j,
                     dep = vname,
                     meth = theMethod,
                     log = oldstate$log)
          assign("state", newstate, pos=parent.frame(), inherits=TRUE)
          
          if(printFlag & theMethod!="dummy") cat(" ", vname)
          if(theMethod!="" & (!is.passive(theMethod)) & theMethod!="dummy"){  # for a true imputation method
            if (substring(tolower(theMethod), 1, 2) != "2l") {	# RJ: for an non-multilevel imputation method 
              x <- p$data[, p$predictorMatrix[j, ] == 1, drop = FALSE]
              y <- p$data[, j]
              ry <- r[, j]
              nam <- vname
              if (k == 1) check.df(x, y, ry, ...)  # added 31/10/2012, throw warning for n(obs) < p case
              f <- paste("mice.impute", theMethod, sep = ".")
              keep <- remove.lindep(x, y, ry, ...)
              x <- x[, keep, drop = FALSE]
              imp[[j]][, i] <- do.call(f, args = list(y, ry, x, ...))
            }
            else { # for a multilevel imputation method
              predictors <- p$predictorMatrix[j, ] != 0
              x <- p$data[, predictors, drop = FALSE]
              y <- p$data[, j]
              ry <- r[, j]
              type <- p$predictorMatrix[j, predictors]
              nam <- vname
              if (k == 1) check.df(x, y, ry, ...)  # added 31/10/2012, throw warning for n(obs) < p case
              f <- paste("mice.impute", theMethod, sep = ".")
              keep <- remove.lindep(x, y, ry, ...)
              x <- x[, keep, drop = FALSE]
              type <- type[keep]
              imp[[j]][, i] <- do.call(f, args = list(y, 
                                            ry, x, type, ...))
            }
            p$data[!r[,j],j] <- imp[[j]][,i]
          }
          else if (is.passive(theMethod)) {
            imp[[j]][,i] <- model.frame(as.formula(theMethod), p$data[!r[,j],])	#RJ - FIXED passive imputation: as.formula()
            p$data[!r[,j],j] <- imp[[j]][,i]
          }
          else if (theMethod== "dummy") {
            ## FEH
            cat.columns <- p$data[,p$categories[j,4]] 
            p$data[,(j:(j+p$categories[p$categories[j,4],2]-1))] <-
              matrix((model.matrix(~cat.columns-1)[,-1]),
                     ncol=p$categories[p$categories[j,4],2],
                     nrow=nrow(p$data))
            remove("cat.columns")
          }

          ## optional post-processing
          cmd <- p$post[j]          # SvB Aug 2009
          if (cmd != "") {
            eval(parse(text = cmd))
            p$data[!r[,j],j] <- imp[[j]][,i]
          }  
        } # end j loop 
      }   # end i loop
      k2 <- k - from + 1
      for(j in 1:length(visitSequence)) {
        jj <- visitSequence[j]
        if (!is.factor(data[,jj])){
          chainVar[j, k2, ] <- apply(imp[[jj]],2,var) 
          chainMean[j, k2, ] <- colMeans(as.matrix(imp[[jj]])) ##pm 04/02
        }
        if (is.factor(data[,jj])){
          for (mm in 1:m) {
            nc <- as.integer(factor(imp[[jj]][,mm], levels=levels(data[,jj])))
            chainVar[j, k2, mm] <- var(nc)
            chainMean[j, k2, mm] <- mean(nc)
          }
        }
      }
    } # end iteration loop
    if (printFlag) cat("\n")
}
    return(list(iteration=maxit, imp=imp, chainMean = chainMean, chainVar = chainVar))
}


check.df <- function(x, y, ry, ...) {
    # if needed, writes the df warning message to the log 
    df <- sum(ry) - ncol(x)
    mess <- paste("df set to 1. # observed cases:",sum(ry)," # predictors:",ncol(x))
    if (df < 1) updateLog(out=mess, frame=3)
}

remove.lindep <- function(x, y, ry, eps = 0.0001, 
    maxcor = 0.99, allow.na=FALSE, ...) {
  if (ncol(x)==0) return(NULL) 
  if (eps <= 0) 
    stop("\n Argument 'eps' must be positive.")
  xobs <- x[ry, , drop=FALSE]
  if (allow.na){
    if (sum(ry)==0) {  # escape for columns with only missing data  SvB 10/3/2011
      updateLog(out="No observed cases, predictor removal skipped", frame=3)
      return(rep(TRUE,ncol(x)))   
    }
  }
  yobs <- as.numeric(y[ry])
  keep <- unlist(apply(xobs, 2, var) > eps)
  keep[is.na(keep)] <- FALSE
  keep <- keep & suppressWarnings((unlist(apply(xobs, 2, cor, yobs)) < maxcor))
  if (all(!keep))
    warning("All predictors are constant or have too high correlation.")
  k <- sum(keep)
  cx <- cor(xobs[, keep, drop=FALSE], use = "all.obs")
  eig <- eigen(cx, symmetric = TRUE)
  ncx <- cx
  while (eig$values[k]/eig$values[1] < eps) {
    j <- (1:k)[order(abs(eig$vectors[, k]), decreasing = TRUE)[1]]
    keep[keep][j] <- FALSE
    ncx <- cx[keep[keep], keep[keep], drop = FALSE]
    k <- k - 1
    eig <- eigen(ncx)
  }
  if (!all(keep)) {
    out <- paste(dimnames(x)[[2]][!keep], collapse = ", ")
    updateLog(out=out, frame=3)
  }
  return(keep)
}


## make list of collinear variables to remove
find.collinear <- function(x, threshold=0.999, ...) {
  nvar <- ncol(x)
  x <- data.matrix(x)
  r <- !is.na(x)
  nr <- apply(r, 2, sum, na.rm=TRUE)
  ord <- order(nr, decreasing=TRUE)
  xo <- x[, ord, drop=FALSE]     ## SvB 24mar2011
  varnames <- dimnames(xo)[[2]]
  z <- suppressWarnings(cor(xo, use="pairwise.complete.obs"))
  hit <- outer(1:nvar,1:nvar,"<") & (abs(z) >= threshold)
  out <- apply(hit,2,any,na.rm=TRUE)
  return(varnames[out])
}


updateLog <- function(out=NULL, meth=NULL, frame=2){
  s <- get("state", parent.frame(frame))
  r <- get("loggedEvents", parent.frame(frame))

  rec <- data.frame(it = s$it,
                    im = s$im,
                    co = s$co,
                    dep = s$dep,
                    meth = ifelse(is.null(meth), s$meth, meth),
                    out = ifelse(is.null(out), "", out)
                    )

  if (s$log) rec <- rbind(r, rec)
  s$log <- TRUE
  assign("state", s, pos=parent.frame(frame), inherits=TRUE)
  assign("loggedEvents", rec, pos=parent.frame(frame), inherits=TRUE)
  return()
}


#
# Standard collection of mice univariate imputation functions.
#
#--------------------MICE.IMPUTE.NORM----------------------------------

mice.impute.norm <- function(y, ry, x, ...)
{
# Bayesian normal imputation of y given x according to Rubin p. 167
# x is complete.
#
# TNO Quality of Life
# authors: S. van Buuren and K. Groothuis-Oudshoorn
#
    x <- cbind(1, as.matrix(x))
    parm <- .norm.draw(y, ry, x, ...)
    return(x[!ry,  ] %*% parm$beta + rnorm(sum(!ry)) * parm$sigma)
}


#------------------------MICE.IMPUTE.NORM.NOB-----------------------
mice.impute.norm.nob <- function(y, ry, x, ...)
{
# mice.impute.norm.nob
# Regression imputation of y given x, with a fixed regression
# line, and with random draws of the residuals around the line.
# x is complete.
#
    x <- cbind(1, as.matrix(x))
    parm <- .norm.fix(y, ry, x, ...)
    return(x[!ry,  ] %*% parm$beta + rnorm(sum(!ry)) * parm$sigma)
}

###------------------------MICE.IMPUTE.NORM.PREDICT--------------------

mice.impute.norm.predict <- function(y, ry, x, ridge=0.00001, ...)
{
# mice.impute.norm.predict
# Regression imputation of y given x, with a fixed regression
# line, takes imputation from the regression line
# prediction method
###

  x <- cbind(1, as.matrix(x))
  xobs <- x[ry,]
  yobs <- y[ry]
  xtx <- t(xobs) %*% xobs
  pen <- ridge * diag(xtx)
  if (length(pen)==1) pen <- matrix(pen)
  v <- solve(xtx + diag(pen))
  coef <- t(yobs %*% xobs %*% v)
  return(x[!ry,  ] %*% coef)
}


###------------------------MICE.IMPUTE.NORM.BOOT----------------------

mice.impute.norm.boot <- function(y, ry, x, ridge=0.00001, ...)
{
# mice.impute.norm.boot
# Regression imputations of y given x, with a fixed regression
# line, and with random draws of the residuals around the line.
# bootstrap version
#

  x <- cbind(1, as.matrix(x))
  xobs <- x[ry,]
  yobs <- y[ry]
  n1 <- sum(ry)
  n0 <- sum(!ry)
  s <- sample(n1, n1, replace=TRUE)
  dotxobs <- xobs[s,]
  dotyobs <- yobs[s]
  xtx <- t(dotxobs) %*% dotxobs
  pen <- ridge * diag(xtx)
  if (length(pen)==1) pen <- matrix(pen)
  v <- solve(xtx + diag(pen))
  coef <- t(dotyobs %*% dotxobs %*% v)
  residuals <- dotyobs - dotxobs %*% coef
  sigma <- sqrt((sum(residuals^2))/(n1-ncol(x)-1))
  parm <- list(coef, sigma)
  names(parm) <- c("beta", "sigma")
  return(x[!ry,  ] %*% parm$beta + rnorm(n0) * parm$sigma)
}



#--------------------------------.NORM.FIX-----------------------------
.norm.fix <- function(y, ry, x, ridge=0.00001, ...)
{
# .norm.fix
# Calculates regression coefficients + error estimate
#
# TNO Quality of Life
# authors: S. van Buuren and K. Groothuis-Oudshoorn
#
  xobs <- x[ry,]
  yobs <- y[ry]
  xtx <- t(xobs) %*% xobs
  pen <- ridge * diag(xtx)
  if (length(pen)==1) pen <- matrix(pen)
  v <- solve(xtx + diag(pen))
  coef <- t(yobs %*% xobs %*% v)
  residuals <- yobs - xobs %*% coef
  sigma <- sqrt((sum(residuals^2))/(sum(ry)-ncol(x)-1))
  parm <- list(coef, sigma)
  names(parm) <- c("beta", "sigma")
  return(parm)
}

#--------------------------------.NORM.DRAW-----------------------------
.norm.draw <- function(y, ry, x, ridge=0.00001, ...)
{
# .norm.draw
# Draws values of beta and sigma for Bayesian linear regrssion imputation 
# of y given x according to Rubin p. 167
# x is complete.
#
# TNO Quality of Life
# authors: S. van Buuren and K. Groothuis-Oudshoorn
#
# adapted 17/12 nrow(x) should be sum(ry)
  xobs <- x[ry,]
  yobs <- y[ry]
  xtx <- t(xobs) %*% xobs
  pen <- ridge * diag(xtx)
  if (length(pen)==1) pen <- matrix(pen)
  v <- solve(xtx + diag(pen))
  coef <- t(yobs %*% xobs %*% v)
  residuals <- yobs - xobs %*% coef
#  sigma.star <- sqrt(sum((residuals)^2)/rgamma(1, sum(ry) - ncol(x)))
  df <- max(sum(ry) - ncol(x), 1)  # SvB 31/10/2012
  sigma.star <- sqrt(sum((residuals)^2)/rchisq(1, df))  # SvB 01/02/2011
  beta.star <- coef + (t(chol((v + t(v))/2)) %*% rnorm(ncol(x))) * sigma.star
  parm <- list(coef, beta.star, sigma.star)      # SvB 10/2/2010
  names(parm) <- c("coef","beta", "sigma")       # SvB 10/2/2010
  return(parm)
}



###-----------------------------MICE.IMPUTE.PMM-------------------------

mice.impute.pmm <- function (y, ry, x, ...)
    # Imputation of y by predictive mean matching, based on
    # Rubin (p. 168, formulas a and b).
    # The procedure is as follows:
    # 1. Draw beta and sigma from the proper posterior
    # 2. Compute predicted values for yobs and ymis
    # 3. For each ymis, find the three observations with closest predicted value, 
    #    sample one randomly, and take its observed y as the imputation.
    # NOTE: The matching is on yhat, NOT on y, which deviates from formula b.
    # ry=TRUE if y observed, ry=FALSE if y missing
    #
    # Authors: S. van Buuren and K. Groothuis-Oudshoorn
# Version 10/2/2010: yhatobs is calculated using the estimated 
#                    rather than the drawn regression weights
#                    this creates between imputation variability 
#                    for the one-predictor case
# Version 06/12/2010 A random draw is made from the closest THREE donors.
# Version 25/04/2012 Extended to work with factors
# version 31/10/2012 Using faster pmm2
{
    x <- cbind(1, as.matrix(x))
    ynum <- y
    if (is.factor(y)) ynum <- as.integer(y)  ## added 25/04/2012
    parm <- .norm.draw(y, ry, x, ...)
    yhatobs <- x[ry, ] %*% parm$coef
    yhatmis <- x[!ry, ] %*% parm$beta
    return(apply(as.array(yhatmis), 1, .pmm.match, yhat = yhatobs, 
                 y = y[ry], ...))
}

#-------------------------.PMM.MATCH--------------------------------
# faster .pmm.match2() from version 2.12 renamed to default .pmm.match() 
.pmm.match <- function (z, yhat = yhat, y = y, donors = 3, ...) 
{
    d <- abs(yhat - z)
    f <- d > 0
    a1 <- ifelse(any(f), min(d[f]), 1)
    d <- d + runif( length(d) , 0 , a1 / 10^10 )
    if (donors == 1) return(y[which.min(d)])
    ds <- sort.int(d, partial = donors)
    m <- sample(y[d <= ds[donors]], 1)
    return(m)
}


###-----------------------------MICE.IMPUTE.PMM2------------------------
### A faster version of mice.impute.pmm()
mice.impute.pmm2 <-
    function (y, ry, x, ...) 
    {
        mess <- "Method 'pmm2' is replaced by method 'pmm'" 
        stop(mess)
    }




#-------------------------MICE.IMPUTE.LOGREG-------------------------

mice.impute.logreg <- function(y, ry, x, ...)
{
# mice.impute.logreg 
#
# Imputation for binary response variables by the Bayesian 
# logistic regression model. See Rubin (1987, p. 169-170) for
# a description of the method.
#
# The method consists of the following steps:
# 1. Fit a logit, and find (bhat, V(bhat))
# 2. Draw BETA from N(bhat, V(bhat))
# 3. Compute predicted scores for m.d., i.e. logit-1(X BETA)
# 4. Compare the score to a random (0,1) deviate, and impute.
#
# Authors: Stef van Buuren, Karin Groothuis-Oudshoorn, 1998
#
#   added statements May 2009
    aug <- augment(y, ry, x, ...)
    x <- aug$x
    y <- aug$y
    ry <- aug$ry
    w <- aug$w
#   end added statements May 2009

    x <- cbind(1, as.matrix(x))
    expr <- expression(glm.fit(x[ry, ], y[ry],
                       family = binomial(link = logit),
                       weights=w[ry]))
    fit <- suppressWarnings(eval(expr))
    fit.sum <- summary.glm(fit)
    beta <- coef(fit)
    rv <- t(chol(fit.sum$cov.unscaled))
    beta.star <- beta + rv %*% rnorm(ncol(rv))  
    p <- 1/(1 + exp(-(x[!ry,] %*% beta.star)))
    vec <- (runif(nrow(p)) <= p)
    vec[vec] <- 1
    if (is.factor(y)) {
        vec<-factor(vec,c(0,1),levels(y))}
    return(vec)
}

#-------------------------MICE.IMPUTE.LOGREG.BOOT--------------------

mice.impute.logreg.boot <- function(y, ry, x, ...)
{
# mice.impute.logreg.boot 
#
# Bootstrap version of mice.impute.logreg.
#
# Author: Stef van Buuren, 2011
#
  ## draw a bootstrap sample for yobs and xobs
  xobs <- x[ry,]
  yobs <- y[ry]
  n1 <- sum(ry)
  n0 <- sum(!ry)
  s <- sample(n1, n1, replace=TRUE)
  doty <- y
  doty[ry] <- yobs[s]
  dotx <- x
  dotx[ry,] <- xobs[s,]

  x <- dotx
  y <- doty
  
  x <- cbind(1, as.matrix(x))
  expr <- expression(glm.fit(x[ry, ], y[ry],
      family = binomial(link = logit)))
  fit <- suppressWarnings(eval(expr))
  beta.star <- beta <- coef(fit)
  p <- 1/(1 + exp(-(x[!ry,] %*% beta.star)))
  vec <- (runif(nrow(p)) <= p)
  vec[vec] <- 1
  if (is.factor(y)) {
    vec<-factor(vec,c(0,1),levels(y))}
  return(vec)
}



augment <- function(y, ry, x, maxcat=50, ...){
    # define augmented data for stabilizing logreg and polyreg
    # by the ad hoc procedure of White, Daniel & Royston, CSDA, 2010
    # This function will prevent augmented data beyond the min and
    # the max of the data
    # Input:
    # x: numeric data.frame (n rows)
    # y: factor or numeric vector (lengt n)
    # ry: logical vector (length n)
    # Output:
    # return a list with elements y, ry, x, and w with length n+2*(ncol(x))*length(levels(y))
    # SvB May 2009
    icod <- sort(unique(unclass(y)))
    k <- length(icod)
    if (k>maxcat) stop(paste("Maximum number of categories (",maxcat,") exceeded", sep=""))
    p <- ncol(x)

    # skip augmentation if there are no predictors
    if (p==0) return(list(y=y,ry=ry,x=x,w=rep(1,length(y))))

    ## skip augmentation if there is only 1 missing value  12jul2012
    ## this need to be fixed 12jul2011
    if (sum(!ry)==1) return(list(y=y,ry=ry,x=x,w=rep(1,length(y))))
    
    # calculate values to augment
    mean <- apply(x,2,mean)
    sd <- sqrt(apply(x,2,var))
    minx <- apply(x,2,min)
    maxx <- apply(x,2,max)
    nr <- 2 * p * k
    a <- matrix(mean, nrow=nr, ncol=p, byrow=TRUE)
    b <- matrix(rep(c(rep(c(0.5,-0.5),k),rep(0,nr)),length=nr*p), nrow=nr, ncol=p, byrow=FALSE)
    c <- matrix(sd, nrow=nr, ncol=p, byrow=TRUE)
    d <- a + b * c
    d <- pmax(matrix(minx,nrow=nr,ncol=p, byrow=TRUE), d)
    d <- pmin(matrix(maxx,nrow=nr,ncol=p, byrow=TRUE), d)
    e <- rep(rep(icod, each=2), p)

    dimnames(d) <- list(paste("AUG",1:nrow(d),sep=""),dimnames(x)[[2]])
    xa  <- rbind.data.frame(x, d)
    # beware, concatenation of factors
    if (is.factor(y)) ya <- as.factor(levels(y)[c(y,e)]) else ya  <- c(y, e)
    rya <- c(ry, rep(TRUE, nr))
    wa  <- c(rep(1,length(y)),rep((p+1)/nr,nr))

    return(list(y=ya, ry=rya, x=xa, w=wa))
}


#--------------------MICE.IMPUTE.POLYREG-----------------------------   
    
mice.impute.polyreg <- function(y, ry, x, nnet.maxit=100, nnet.trace=FALSE, nnet.maxNWts=1500, ...)
{
# mice.impute.polyreg
#
# Imputation for categorical response variables by the Bayesian
# polytomous regression model. See J.P.L. Brand (1999), Chapter 4,
# Appendix B.
#
# The method consists of the following steps:
# 1. Fit categorical response as a multinomial model
# 2. Compute predicted categories
# 3. Add appropriate noise to predictions.
#
# This algorithm uses the function multinom from the libraries nnet and MASS
# (Venables and Ripley).
#
# Authors: Stef van Buuren, Karin Groothuis-Oudshoorn, 2000
#
# SvB May 2009   # augmented data added for stability
# SvB Dec 2010   # assign(data) hack removed
    x <- as.matrix(x)
    aug <- augment(y, ry, x, ...)
    x <- aug$x
    y <- aug$y
    ry <- aug$ry
    w <- aug$w
#
    xy <- cbind.data.frame(y=y, x=x)  # fixed SvB 6/12/2010
    fit <- multinom(formula(xy), data=xy[ry,],
                    weights=w[ry],
                    maxit=nnet.maxit, trace=nnet.trace, maxNWts=nnet.maxNWts, ...)
    post <- predict(fit, xy[!ry,], type = "probs")
    if (sum(!ry)==1) post <- matrix(post, nrow=1, ncol=length(post))   # SvB 14 sept 2009
    fy <- as.factor(y)
    nc <- length(levels(fy))
    un <- rep(runif(sum(!ry)),each=nc)

    if (is.vector(post)) post <- matrix(c(1-post,post),ncol=2)
    draws <- un>apply(post,1,cumsum)
    idx <- 1+apply(draws,2,sum)
    return(levels(fy)[idx])
}

#--------------------MICE.IMPUTE.POLR-----------------------------

mice.impute.polr <- function (y, ry, x, nnet.maxit=100, nnet.trace=FALSE, nnet.maxNWts=1500, ...)
{
  ### added 08/12/2010
  x <- as.matrix(x)
  aug <- augment(y, ry, x, ...)
  x <- aug$x
  y <- aug$y
  ry <- aug$ry
  w <- aug$w
  xy <- cbind.data.frame(y = y, x = x)

  ## polr may fail on sparse data. We revert to multinom in such cases. 
  fit <- try(suppressWarnings(polr(formula(xy), data = xy[ry, ], weights=w[ry],...)), silent=TRUE)
  if (inherits(fit, "try-error")) {
    fit <- multinom(formula(xy), data=xy[ry,],
                    weights=w[ry],
                    maxit=nnet.maxit, trace=nnet.trace, maxNWts=nnet.maxNWts, ...)
    updateLog(meth="multinom", frame=3)
  }
  post <- predict(fit, xy[!ry, ], type = "probs")
  if (sum(!ry) == 1) 
    post <- matrix(post, nrow = 1, ncol = length(post))
  fy <- as.factor(y)
  nc <- length(levels(fy))
  un <- rep(runif(sum(!ry)), each = nc)
  if (is.vector(post)) 
    post <- matrix(c(1 - post, post), ncol = 2)
  draws <- un > apply(post, 1, cumsum)
  idx <- 1 + apply(draws, 2, sum)
  return(levels(fy)[idx])
}

    
#--------------------MICE.IMPUTE.LDA-----------------------------

mice.impute.lda <- function(y, ry, x, ...)
{
# mice.impute.lda
#
# Imputation of categorical response variables by linear discriminant
# analysis. This function uses the Venables/Ripley functions
# lda and predict.lda to compute posterior probabilities for
# each incomplete case, and draws the imputations from this
# posterior.
#
# Authors: Stef van Buuren, Karin Groothuis-Oudshoorn, 11 okt 1999
# V2: Adapted SvB June 2009 to include bootstrap - disabled since
# bootstrapping it may easily to constant variables within groups.
# which will be difficult to detect when bootstrapped

    fy <- as.factor(y)
    nc <- length(levels(fy))

#   SvB June 2009 - take bootstrap sample of training data
#   idx <- sample((1:length(y))[ry], size=sum(ry), replace=TRUE)
#   x[ry,] <- x[idx,]
#   y[ry] <- y[idx]
#   end bootstrap

    fit <- lda(x, fy, subset=ry)
    post <- predict(fit, x[!ry,,drop=FALSE])$posterior
    un <- rep(runif(sum(!ry)),each=nc)
    idx <- 1+apply(un>apply(post,1,cumsum),2,sum)
    return(levels(fy)[idx])
}

#-------------------MICE.IMPUTE.MEAN------------------------------

mice.impute.mean<-function(y, ry, x=NULL, ...)
# mice.impute.mean
#
# Unconditional mean imputation 
#
{
    return(rep(mean(y[ry]), times = sum(!ry)))
}

#-------------------MICE.IMPUTE.SAMPLE----------------------------

mice.impute.sample<-function(y, ry, x=NULL, ...)
# mice.impute.sample
#
# Generates random sample from the observed y's
#
{   
    yry <- y[ry]   ## bug fixed 24jun2012
    if (length(yry) <= 1) return(rnorm(sum(!ry)))
    return(sample(yry, size = sum(!ry), replace = TRUE))
}
#-------------------MICE.IMPUTE.PASSIVE----------------------------

mice.impute.passive <- function(data, func)
#
# Special elementary imputation method for transformed data.
{
    return(model.frame(func, data))
}


#-------------------MICE.IMPUTE.2L.NORM----------------------------

mice.impute.2L.norm <- function(y, ry, x, type, intercept = TRUE, ...)
{
  rwishart <- function(df, p = nrow(SqrtSigma), SqrtSigma = diag(p)) {
    ## rwishart, written by Bill Venables
    Z <- matrix(0, p, p)
    diag(Z) <- sqrt(rchisq(p, df:(df-p+1)))
    if(p > 1) {
      pseq <- 1:(p-1)
      Z[rep(p*pseq, pseq) + unlist(lapply(pseq, seq))] <- rnorm(p*(p-1)/2)
    }
    crossprod(Z %*% SqrtSigma)
  }

  force.chol <- function(x, warn=TRUE) {
    z <- 0
	
    repeat {
      lambda <- 0.1 * z
      XT <- x + diag(x=lambda, nrow=nrow(x))
      s <- try(expr=chol(XT), silent=TRUE)
      if (class(s) != "try-error") break
      z <- z+1
    }
	
    attr(s,"forced") <- (z>0)
    if (warn && z>0) warning("Cholesky decomposition had to be forced", call.=FALSE)
	
    return (s)
  }
  ## written by Roel de Jong

  ## append intercept
  if (intercept) {
    x <- cbind(1, as.matrix(x))
    type <- c(2, type)
  }
    
  ## Initialize
  n.iter <- 100
  nry <- !ry
  n.class <- length(unique(x[, type==(-2)]))
  gf.full <- factor(x[,type==(-2)], labels=1:n.class)
  gf <- gf.full[ry]
  XG <- split.data.frame(as.matrix(x[ry, type == 2]), gf)
  X.SS <- lapply(XG, crossprod)
  yg <- split(as.vector(y[ry]), gf)
  n.g <- tabulate(gf)
  n.rc <- ncol(XG[[1]])
  
  bees <- matrix(0, nrow=n.class, ncol=n.rc)
  ss <- vector(mode="numeric", length=n.class)
  mu <- rep(0, n.rc)
  inv.psi <- diag(1, n.rc, n.rc)
  inv.sigma2 <- rep(1, n.class)
  sigma2.0 <- 1
  theta <- 1	

  ## Execute Gibbs sampler
  for (iter in 1:n.iter) {	
    ## Draw bees	
    for (class in 1:n.class) { 
      bees.var <- chol2inv(chol(inv.sigma2[class]*X.SS[[class]] + inv.psi))
      bees[class,] <- drop(bees.var %*% (crossprod(inv.sigma2[class]*XG[[class]], yg[[class]]) + inv.psi %*% mu)) + drop(rnorm(n=n.rc) %*% chol(bees.var))
      ss[class] <- crossprod(yg[[class]] - XG[[class]] %*% bees[class,])
    }
		
    ## Draw mu		
    mu <- colMeans(bees) + drop(rnorm(n=n.rc) %*% chol(chol2inv(chol(inv.psi))/n.class))
    
    ## Draw psi
    inv.psi <- rwishart(df=n.class-n.rc-1, SqrtSigma=chol(chol2inv(chol(crossprod(t(t(bees) - mu))))))	
    
    ## Draw sigma2
    inv.sigma2 <- rgamma(n.class, n.g/2 + 1/(2*theta), scale=2*theta / (ss*theta+sigma2.0))
    
    ## Draw sigma2.0
    H <- 1/mean(inv.sigma2)			# Harmonic mean
    sigma2.0 <- rgamma(1, n.class/(2*theta) + 1, scale= 2 * theta * H / n.class)

    ## Draw theta
    G <- exp(mean(log(1/inv.sigma2)))		# Geometric mean
    theta <- 1/rgamma(1, n.class/2-1, scale=2/(n.class*(sigma2.0/H-log(sigma2.0)+log(G)-1)))
  }
	

  ## Generate imputations
  imps <- rnorm(n=sum(nry), sd=sqrt(1/inv.sigma2[gf.full[nry]])) + rowSums(as.matrix(x[nry,type==2]) * bees[gf.full[nry],])
  return(imps)
}

mice.impute.2l.norm <- mice.impute.2L.norm   # for backward compatibility


#------------------------MICE.IMPUTE.2lonly.mean---------------------------------

mice.impute.2lonly.mean <- function(y, ry, x, type, ...){
    ## Gerko Vink, Stef van Buuren mar2013
    ## class mean imputation of second level variables
    if(all(ry)) stop("No missing data found")
    yobs <- y[ry]
    class <- x[, type == -2]
    classobs <- class[ry]
    classmis <- class[!ry]
    
    # deal with empty classes (will be NaN)
    empty.classes <- class[!class %in% classobs]
    classobs <- c(classobs, empty.classes)
    yobs <- c(yobs, rep(NA, length(empty.classes)))
    
    # return the means per class
    ymean <- aggregate(yobs, list(classobs), mean, na.rm = TRUE)
    return(apply(as.matrix(classmis), 1, function(z, y) y[z == y[ , 1], 2], y = ymean, ...))
}



#------------------------------SQUEEZE------------------------------------

squeeze <- function(x, bounds=c(min(x[r]),max(x[r])), r=rep(TRUE,length(x)), ...){
    if (length(r) != length(x)) stop("Different length of vectors x and r")
    x[x<bounds[1]] <- bounds[1]
    x[x>bounds[2]] <- bounds[2]
    return(x)
}



#------------------------------MD.PATTERN-------------------------------

md.pattern <- function(x)
{
# md.pattern
#
# computes the missing data pattern in the data
# x can be a vector, matrix or data frame
# NA's indicate missing data
# based on Schafer's prelim.norm function
# SvB, 13-7-99
# SvB, 32 columns bug removed - 8mar2012
#
    if(!(is.matrix(x) | is.data.frame(x)))
        stop("Data should be a matrix or dataframe")
    if(ncol(x) < 2) stop("Data should have at least two columns")
    # if(is.data.frame(x)) x <- data.frame.to.matrix(x)
    if(is.data.frame(x)) x <- data.matrix(x)       # SvB use standard R function > V2.5
    n <- nrow(x)
    p <- ncol(x)
    mode(x) <- "single" # find missingness patterns
    r <- 1 * is.na(x)
    nmis <- as.integer(apply(r, 2, sum))
    names(nmis) <- dimnames(x)[[2]] # index the missing data patterns
    mdp <- (r %*% (2^((1:ncol(x)) - 1))) + 1    # do row sort  SvB 8mar2012
    ro <- order(mdp)
    x <- matrix(x[ro,  ], n, p) ##pm 04/02
    mdp <- mdp[ro]
    r <- matrix(r[ro,  ], n, p) ##pm 04/02
    ro <- order(ro) # compress missing data patterns
    mdpst <- as.integer(seq(along = mdp)[!duplicated(mdp)])
    mdp <- unique(mdp)
    npatt <- length(mdpst)  # create r-matrix for display purposes
    r <- 1 - r
    r <- matrix(r[mdpst,  ], npatt, p)
    if(npatt == 1)
        tmp <- format(n)
    if(npatt > 1)
        tmp <- format(c(mdpst[2:npatt], n + 1) - mdpst)
    dimnames(r) <- list(tmp, dimnames(x)[[2]])
    storage.mode(r) <- "integer"    # center and scale the columns of x
#
    if(npatt > 1)
        nmdp <- as.integer(c(mdpst[-1], n + 1) - mdpst)
    if(npatt == 1) nmdp <- n    # 
# sort the rows and columns according to the marginals
    co <- order(nmis)
    ro2 <- order(nmis.row <- p - as.integer(apply(r, 1, sum)))
    r <- rbind(r[ro2, co], nmis[co])
    r <- cbind(r, c(nmis.row[ro2], sum(nmis)))
    r
}

#------------------------------md.pairs---------------------------------

md.pairs <- function(data){
    # calculates pairwise missing data statistics
    # rr:  response-response pairs
    # rm:  response-missing pairs
    # mr:  missing-response pairs
    # mm:  missing-missing pairs
    if(!(is.matrix(data) | is.data.frame(data))) stop("Data should be a matrix or dataframe")
    if(ncol(data) < 2) stop("Data should have at least two columns")

    r <- !is.na(data)
    rr <- t(r)%*%r
    mm <- t(!r)%*%!r
    mr <- t(!r)%*%r
    rm <- t(r)%*%!r
    return(list(rr=rr, rm=rm, mr=mr, mm=mm))
}

#--------------------------------QUICKPRED------------------------------------
    

quickpred <- function(data, mincor=0.1, minpuc=0, include="", exclude="", method="pearson") {
    # automatic predictor selection according to Van Buuren et al (1999)

    # argument checking
    if(!(is.matrix(data) | is.data.frame(data))) stop("Data should be a matrix or data frame")
    if ((nvar <- ncol(data)) < 2) stop ("Data should contain at least two columns")

    # initialize
    predictorMatrix <- matrix(0, nrow=nvar, ncol=nvar, dimnames = list(names(data),names(data)))
    x <- data.matrix(data)
    r <- !is.na(x)
    
    # include predictors with
    #  1) pairwise correlation among data
    #  2) pairwise correlation of data with response indicator
    # higher than mincor
    suppressWarnings(v <- abs(cor(x, use="pairwise.complete.obs", method=method)))
    v[is.na(v)] <- 0
    suppressWarnings(u <- abs(cor(y=x, x=r, use="pairwise.complete.obs", method=method)))
    u[is.na(u)] <- 0
    maxc <- pmax(v,u)
    predictorMatrix[maxc>mincor] <- 1
    
    # exclude predictors with a percentage usable cases below minpuc
    p <- md.pairs(data)
    puc <- p$mr/(p$mr+p$mm)
    predictorMatrix[puc<minpuc] <- 0

    # exclude predictors listed in the exclude argument
    yz <- pmatch(exclude, names(data))
    predictorMatrix[,yz] <- 0

    # include predictors listed in the include argument
    yz <- pmatch(include, names(data))
    predictorMatrix[,yz] <- 1

    # some final processing
    diag(predictorMatrix) <- 0
    predictorMatrix[colSums(!r)==0,] <- 0
    
    return(predictorMatrix)
}

#-----------------------------NELSONAALEN-------------------------------------

nelsonaalen <- function(data, timevar, statusvar){
  require(survival)
  if (!is.data.frame(data)) stop("Data must be a data frame")
  timevar <- as.character(substitute(timevar))
  statusvar <- as.character(substitute(statusvar))
  time <- data[,timevar]
  status <- data[,statusvar]
  
  hazard <- basehaz(coxph(Surv(time,status)~1,data=data))
  idx <- match(time, hazard[,"time"])
  return(hazard[idx,"hazard"])
}


#--------------------------------IS.MIDS--------------------------------------

is.mids<-function(x)
{
    inherits(x,"mids")
}


#--------------------------------IS.MIRA--------------------------------------

is.mira <- 
function(x)
{
    inherits(x, "mira")
}

#--------------------------------AS.MIRA--------------------------------------

as.mira <- function(fitlist) {
  call <- match.call()
  if (!is.list(fitlist)) 
    stop("Argument 'fitlist' is not a list")
  m <- length(fitlist)
  object <- list(call = call, call1 = NULL, nmis = NULL, 
                 analyses = fitlist)
  oldClass(object) <- c("mira", "matrix")
  return(object)
}


#--------------------------------IS.MIPO--------------------------------------

is.mipo <- 
function(x)
{
    inherits(x, "mipo")
}

#------------------------------is.passive------------------------------------

is.passive <- function(string)
{
    return("~" == substring(string, 1, 1))
}



#--------------------------------PRINT.MIDS--------------------------------------
setMethod("print", "mids",
   function( x, ...) {
     print.mids( x, ...)
   }
)

print.mids <- function(x, ...){
    if (is.mids(x)) {
        cat("Multiply imputed data set")
        cat("\nCall:\n")
        print(x$call)
        cat("Number of multiple imputations: ",x$m)
        cat("\nMissing cells per column:\n")
        print(x$nmis)
        cat("Imputation methods:\n")
        print(x$method)
        cat("VisitSequence:\n")
        print(x$visitSequence)
        cat("PredictorMatrix:\n")
        print(x$predictorMatrix)
        cat("Random generator seed value: ",x$seed,"\n")
    }
    else print(x)
    invisible()
}
#------------------------------print.mira-------------------------------

setMethod("print",signature(x = "mira"),
   function( x ) {
     print.mira( x )
   }
)

print.mira <- function(x )
{
 ## prints the mira object; A mira object
 ## is in fact a list, so it will be printed as such.
 ## KO, 3/2/00

    if(is.mira(x))
        print.listof(x) ##PM 4/02
    else  print(x) 
    invisible()

}

#------------------------------summary.mira-------------------------------
setMethod("summary",signature(object = "mira"),
   function( object ) {
     summary.mira( object )
   }
)

summary.mira<-function(object)
{
# This summary function is for a mira object. 
# Then the seperate analyses are of class lm (glm), it calls
# sequentially summary.lm (summary.glm) for all analyses. 
# KO, 4/2/00

    for (i in (1:length(object$analyses))){ 
        cat("\n","## summary of imputation",i,":\n")
        print(summary(object$analyses[[i]]))
    }
}

#------------------------------print.mipo-------------------------------
setMethod("print",signature(x = "mipo"),
   function( x, ...) {
     print.mipo( x, ...)
   }
)

print.mipo <- function(x, ...)
{
    if(!is.null(x$call)) {
        cat("Call: ")
        dput(x$call)
    }
    cat("\nPooled coefficients:\n")
    print(x$qbar, ...)
#   cat("Relative increase in variance due to nonresponse per parameter:", "\n")
#   print(x$r)
    cat("\nFraction of information about the coefficients missing due to nonresponse:", "\n")
    print(x$f)
    invisible(x)
}


#------------------------------summary.mipo-------------------------------
setMethod("summary",signature(object = "mipo"),
   function( object, ...) {
     summary.mipo( object, ...)
   }
)

summary.mipo <- function(object, ...){
# 
# summary method for the pooled analysis results
#
# object: object of class mipo
#
    x <- object
    table <- array(x$qbar, dim = c(length(x$qbar), 10))
    dimnames(table) <- list(labels(x$qbar), c("est", "se", "t", "df", "Pr(>|t|)", "lo 95","hi 95", "nmis", "fmi","lambda"))
    table[, 2] <- sqrt(diag(x$t))
    table[, 3] <- table[,1] / table[,2]
    table[, 4] <- x$df
    table[, 5] <- if(all(x$df > 0)) 2 * (1 - pt(abs(table[, 3]), x$df)) else NA
    table[, 6] <- table[,1] - qt(0.975, x$df) * table[, 2]
    table[, 7] <- table[,1] + qt(0.975, x$df) * table[, 2]
    if (is.null(x$nmis)) table[, 8] <- NA
    else table[, 8] <- x$nmis[names(x$qbar)]
    table[, 9] <- x$fmi
    table[,10] <- x$lambda
    return(table)
}


#--------------------------------SUMMARY.MIDS--------------------------------------
setMethod("summary",signature(object = "mids"),
   function( object, ...) {
     summary.mids( object, ...)
   }
)

summary.mids<-function(object, ...)
{
    print(object)
    invisible()
}


setMethod("plot",signature(x = "mids", y="ANY"),
   function( x, y, ...) {
     plot.mids( x, y, ...)
   }
)

plot.mids <- function(x, y=NULL, theme=mice.theme(),
                        layout=c(2,3), type="l", col=1:10, lty=1,
                        ...){
  strip.combined <- function(which.given, which.panel, factor.levels, ...){
    if (which.given == 1){
      panel.rect(0,0,1,1,col=theme$strip.background$col,border=1)
      panel.text(x=0, y=0.5, pos=4,
                 lab = factor.levels[which.panel[which.given]])
    }
    if (which.given == 2){
      panel.text(x=1, y=0.5, pos=2,
                 lab = factor.levels[which.panel[which.given]])
    } 
  }
  
  call <- match.call()
  if (!is.mids(x)) stop("Argument 'x' must be a 'mids' object")
  
  mn <- x$chainMean
  sm <- sqrt(x$chainVar)
  varlist <- dimnames(mn[,,1,drop=FALSE])[[1]]   # SvB 25jun2012
  
  ## create formula if not given in y
  if (missing(y)) {
    formula <- as.formula(paste(paste(varlist,collapse="+",sep=""),"~.it|.ms",sep=""))
  } else {
    formula <- NULL
    if (is.null(y)) formula <- as.formula(paste(paste(varlist,collapse="+",sep=""),"~.it|.ms",sep=""))
    if (class(y)=="character") {
      if (length(y)==1) formula <- as.formula(paste(y,"~.it|.ms",sep=""))
      else formula <- as.formula(paste(paste(y,collapse="+",sep=""),"~.it|.ms",sep=""))
    }
    if (class(y)=="integer" | class(y)=="logical") {
      vars <- varlist[y]
      if (length(vars)==1) formula <- as.formula(paste(vars,"~.it|.ms",sep=""))
      else formula <- as.formula(paste(paste(vars,collapse="+",sep=""),"~.it|.ms",sep=""))
    }
    if(is.null(formula)) formula <- as.formula(y)
  }
  
  m <- x$m
  it <- x$iteration
  mn <- matrix(aperm(mn,c(2,3,1)), nrow=m*it)
  sm <- matrix(aperm(sm,c(2,3,1)), nrow=m*it)
  
  adm <- expand.grid(1:it, 1:m, c("mean","sd"))
  data <- cbind(adm,rbind(mn, sm))
  colnames(data) <- c(".it",".m",".ms",varlist)
  .m <- NULL; rm(.m)  ## Dummy to trick R CMD check 
  
  tp <- xyplot(x=formula,
               data=data,
               groups=.m,
               type=type,
               lty=lty,
               col=col,
               layout = layout,
               scales=list(y=list(relation="free"),
                 x=list(alternating=FALSE)),
               as.table=TRUE,
               xlab="Iteration",
               ylab="",
               strip=strip.combined,
               par.strip.text = list(lines=0.5),
               ...)

  tp <- update(tp, par.settings = theme)
  return(tp)
}

#------------------------------cbind.mids--------------------------------
#setMethod("cbind",signature(x = "mids", y="ANY"),
#   function( x, y, ...) {
#     cbind.mids( x, y, ...)
#   }
#)


cbind.mids<-function(x,y,...){
  # This function combines x and y columnwise into a single mids object.
  # x must be a midsobject
  # y can be a vector, matrix, factor, dataframe or also a mids object.
  # It is allowed to combine more than two objects when y is not a mids object.
  # KO 08/09.
  
  call<-match.call()
  if (!is.mids(y)) y<-cbind.data.frame(y,...)
  
  # The data in y is converted to a dataframe.
  if (is.matrix(y)) y<-as.data.frame(y)
  if (is.vector(y)) y<-as.data.frame(y)
  if (is.factor(y)) y<-as.data.frame(y)

  if (is.data.frame(y))
  {
    if (nrow(y)!=nrow(x$data)) stop("The two datasets do not have the same length\n")

    varnames<-c(dimnames(x$data)[[2]],dimnames(y)[[2]])
    # Call is a vector, with first argument the mice statement and second argument the call to cbind.mids.
    call<-c(x$call,call)
  
    # The data in x (x$data) and y are combined together.
    data<-cbind(x$data,y)
    
    # The number of imputations in the new midsobject is equal to that in x.
    m<-x$m
    
    # count the number of missing data in y 
    nmis<-c(x$nmis,colSums(is.na(y)))

    # The original data of y will be copied into the multiple
    # imputed dataset, including the missing values of y.
    r <- (!is.na(y))
    imp <- vector("list", ncol(y))
    for (j in 1:ncol(y)){
      imp[[j]] <- as.data.frame(matrix(NA, nrow = sum(!r[,j]), ncol =x$m))
      dimnames(imp[[j]]) <- list(row.names(y)[r[,j] == FALSE], 1:m)
    }
    imp<-c(x$imp,imp)
    names(imp)<-varnames

    # The imputation method for (columns in) y will be set to "".
    method<-c(x$method,rep("",ncol(y)))
    names(method)<-c(names(x$method),colnames(y))

    # The variable(s) in y are included in the predictorMatrix.
    # y is not used as predictor as well as not imputed.
    predictorMatrix<-rbind(x$predictorMatrix, matrix(0, ncol = ncol(x$predictorMatrix), nrow = ncol(y)))
    predictorMatrix<-cbind(predictorMatrix, matrix(0, ncol = ncol(y), nrow = nrow(x$predictorMatrix)+ncol(y)))
    dimnames(predictorMatrix) <- list(varnames,varnames)

    # The visitSequence is taken as in x$visitSequence.
    visitSequence<-x$visitSequence
    
    # The post vector for (columns in) y will be set to "".
    post<-c(x$post,rep("",ncol(y)))
    names(post)<-c(names(x$post),colnames(y))
    
    # seed, lastSeedvalue, number of iterations, chainMean and chainVar is taken as in mids object x.
    seed<-x$seed
    lastSeedvalue<-x$lastSeedvalue
    iteration<-x$iteration
    chainMean=x$chainMean
    chainVar=x$chainVar
    
    # padModel for the data to be binded with x.
    # Remark, if a column of y is categorical this is ignored in padModel since that column
    # is not used as predictor for another column.
    
    pad<-padModel(data,  method, predictorMatrix, visitSequence, post, nmis, nvar=ncol(data))


    x<-list(call=call,data=data,m=m,nmis=nmis,imp=imp,method=method,predictorMatrix=predictorMatrix,
        visitSequence=visitSequence,post=post,seed=seed,iteration=iteration,lastSeedvalue=lastSeedvalue,
        chainMean=chainMean,chainVar=chainVar,pad=pad)
  }

  if (is.mids(y))
  {

    if (nrow(y$data)!=nrow(x$data)) stop("The two datasets do not have the same length\n")
    if (x$m != y$m) stop("The two mids objects should have the same number of imputations\n")
  
    # Call is a vector, with first argument the mice statement and second argument the call to cbind.mids.
    call<-c(x$call,call)
    
    # The data in x$data and y$data are combined together.
    data<-cbind(x$data,y$data)
    varnames<-c(dimnames(x$data)[[2]],dimnames(y$data)[[2]])
    
    m<-x$m
    nmis<-c(x$nmis,y$nmis)
    imp<-c(x$imp,y$imp)
    method<-c(x$method,y$method)
    
    # The predictorMatrices of x and y are combined with zero matrices on the off diagonal blocks.
    predictorMatrix<-rbind(x$predictorMatrix, matrix(0, ncol = ncol(x$predictorMatrix), nrow = nrow(y$predictorMatrix)))
    predictorMatrix<-cbind(predictorMatrix, rbind(matrix(0, ncol = ncol(y$predictorMatrix), nrow = nrow(x$predictorMatrix)),y$predictorMatrix))
    dimnames(predictorMatrix) <- list(varnames,varnames)
    
    # As visitSequence is taken first the order for x and after that from y.
    visitSequence<-c(x$visitSequence,y$visitSequence+max(x$visitSequence))
    
    post<-c(x$post,y$post)
    # For the elements seed, lastSeedvalue and iteration the values from midsobject x are copied.
    seed<-x$seed
    lastSeedvalue<-x$lastSeedvalue
    iteration<-x$iteration

    # The padModel is defined by just combining both padModels as defined above.
    padData<-cbind(x$pad$data,y$pad$data)
    varnamesPad<-c(dimnames(x$pad$predictorMatrix)[[1]],dimnames(y$pad$predictorMatrix)[[1]])
    padPredictorMatrix<-rbind(x$pad$predictorMatrix, matrix(0, ncol = ncol(x$pad$predictorMatrix), nrow = nrow(y$pad$predictorMatrix)))
    padPredictorMatrix<-cbind(padPredictorMatrix, rbind(matrix(0, ncol = ncol(y$pad$predictorMatrix), nrow = nrow(x$pad$predictorMatrix)),y$pad$predictorMatrix))
    dimnames(padPredictorMatrix) <- list(varnamesPad,varnamesPad)

    padMethod<-c(x$pad$method,y$pad$method)
    padVisitSequence<-c(x$pad$visitSequence,y$pad$visitSequence+max(x$pad$visitSequence))
    padPost<-c(x$pad$post,y$pad$post)
    padCategories<-rbind(x$pad$categories,y$pad$categories)
    pad<-list(data=padData,predictorMatrix=padPredictorMatrix,method=padMethod,
            visitSequence=padVisitSequence,post=padPost,categories=padCategories)

    # the chainMean and chainVar vectors for x and y are combined.
    chainMean<- array(data=NA, dim=c(dim(x$chainMean)[1]+dim(y$chainMean)[1],iteration,m),
      dimnames=list(c(dimnames(x$chainMean)[[1]],dimnames(y$chainMean)[[1]]),
      dimnames(x$chainMean)[[2]],dimnames(x$chainMean)[[3]]))
    chainMean[1:dim(x$chainMean)[1],,] <- x$chainMean
    if (iteration <= dim(y$chainMean)[2]) 
      chainMean[(dim(x$chainMean)[1]+1):dim(chainMean)[1],,] <- y$chainMean[,1:iteration,]
    else 
      chainMean[(dim(x$chainMean)[1]+1):dim(chainMean)[1],1:dim(y$chainMean)[2],] <- y$chainMean

    chainVar<- array(data=NA, dim=c(dim(x$chainVar)[1]+dim(y$chainVar)[1],iteration,m),
      dimnames=list(c(dimnames(x$chainVar)[[1]],dimnames(y$chainVar)[[1]]),
      dimnames(x$chainVar)[[2]],dimnames(x$chainVar)[[3]]))
    chainVar[1:dim(x$chainVar)[1],,] <- x$chainVar
    if (iteration <= dim(y$chainVar)[2]) 
      chainVar[(dim(x$chainVar)[1]+1):dim(chainVar)[1],,] <- y$chainVar[,1:iteration,]
    else 
      chainVar[(dim(x$chainVar)[1]+1):dim(chainVar)[1],1:dim(y$chainVar)[2],] <- y$chainVar

    x<-list(call=call,data=data,m=m,nmis=nmis,imp=imp,method=method,predictorMatrix=predictorMatrix,
        visitSequence=visitSequence,post=post,seed=seed,iteration=iteration,lastSeedvalue=lastSeedvalue,
        chainMean=chainMean,chainVar=chainVar,pad=pad)
  }

  oldClass(x) <- "mids"
  return(x)
}

#setMethod("rbind",signature(x = "mids", y = "ANY"),
#   function( x, y, ...) {
#     rbind.mids( x, y, ...)
#   }
#)

rbind.mids<-function(x,y,...){
  # This function combines x and y rowwise into a single midsobject.
  # x is a midsobject; y should be
  # 1) a dataframe with the same (number of) columns as x$data; in this case y 
  # is combined with x$data with rbind() and the list elements of x are adjusted.
  # 2) y is a midsobject with the same underlying multiple imputation model as x but based on a
  # different data subset (with exactly the same variable(names) as in x$data). In this case the data is
  # combined with rbind and the other listelements of the midsobject are adjusted. Beware that 
  # imputations in y are generated independently of x and by combining them could be dangerous. 
  #
  # It is allowed to combine more than two objects when y is not a midsobject.
  # KO 08/09.
  
  call<-match.call()
  if (!is.mids(y)) y<-rbind.data.frame(y,...)
  
  # Then y is matrix, y is converted into a dataframe.
  if (is.matrix(y)) y<-as.data.frame(y)
  
  if (is.data.frame(y))
  {
    if (ncol(y)!=ncol(x$data)) stop("The two datasets do not have the same number of columns\n")

    varnames<-c(dimnames(x$data)[[2]])

    # Call is a vector, with first argument the mice statement and second argument the call to cbind.mids.
    call<-c(x$call,call)
  
    # The data in x (x$data) and y are combined together.
    data<-rbind(x$data,y)
    
    # The number of imputations in the new midsobject is equal to that in x.
    m<-x$m
    
    # count the number of missing data in y and add them to x$nmis.
    nmis<-x$nmis+colSums(is.na(y))

    # The listelements method, post, predictorMatrix, visitSequence will be copied from x.
    method<-x$method
    post<-x$post
    predictorMatrix<-x$predictorMatrix
    visitSequence<-x$visitSequence
    
    # The original data of y will be copied into the multiple
    # imputed dataset, including the missing values of y.
    r <- (!is.na(y))
    imp <- vector("list", ncol(y))
    for (j in visitSequence){
      imp[[j]] <- rbind(x$imp[[j]],as.data.frame(matrix(NA, nrow = sum(!r[,j]), ncol =x$m,dimnames=list(row.names(y)[r[,j] == FALSE],1:m))))
    }
    names(imp)<-varnames
        
    # seed, lastSeedvalue, number of iterations, chainMean and chainVar is taken as in mids object x.
    seed<-x$seed
    lastSeedvalue<-x$lastSeedvalue
    iteration<-x$iteration
    chainMean=x$chainMean
    chainVar=x$chainVar
    
    # padModel for the data rbind(x$data,y). 
    
    pad<-padModel(data,  method, predictorMatrix, visitSequence, post, nmis, nvar=ncol(data))
    
    x<-list(call=call,data=data,m=m,nmis=nmis,imp=imp,method=method,predictorMatrix=predictorMatrix,
        visitSequence=visitSequence,post=post,seed=seed,iteration=iteration,lastSeedvalue=lastSeedvalue,
        chainMean=chainMean,chainVar=chainVar,pad=pad)
  }
  
  if (is.mids(y))
  {
    if (ncol(y$data)!=ncol(x$data)) stop("The two datasets do not have the same number of columns.\n")
    if (!all(c(dimnames(x$data)[[2]])==c(dimnames(y$data)[[2]]))) stop("The two datasets do not have the same variable(names).\n")
    if (!(x$m==y$m)) stop("The number of imputations differ between x and y.\n")

    if (!all(x$method==y$method))   warning("Methods vector is not equal in x and y; y$method is ignored.\n")
    if (!all(x$predictorMatrix==y$predictorMatrix))     warning("Predictormatrix is not equal in x and y; y$predictorMatrix is ignored\n.")
    if (!all(x$visitSequence==y$visitSequence))     warning("Visitsequence is not equal in x and y; y$visitSequence is ignored\n.")
    if (!all(x$post==y$post))      warning("The post vector is not equal in x and y; y$post is ignored\n")
    if (!all(x$pad$categories==y$pad$categories))   warning("The categories in the padmodels are not equal in x and y; y$pad is ignored.\n")

    varnames<-c(dimnames(x$data)[[2]])

    # Call is a vector, with first argument the mice statement and second argument the call to cbind.mids.
    call<-match.call()
    call<-c(x$call,call)

    # The data in x (x$data) and y are combined together.
    data<-rbind(x$data,y$data)

    # The number of imputations in the new midsobject is equal to that in x.
    m<-x$m

    # count the number of missing data in y and add them to x$nmis.
    nmis<-x$nmis+y$nmis

    # The listelements method, post, predictorMatrix, visitSequence will be copied from x.
    method<-x$method
    post<-x$post
    predictorMatrix<-x$predictorMatrix
    visitSequence<-x$visitSequence

    # The original data of y will be binded into the multiple
    # imputed dataset, including the imputed values of y.
    imp <- vector("list", ncol(x$data))
    for (j in 1:ncol(x$data)){
      imp[[j]] <- rbind(x$imp[[j]],y$imp[[j]])
    }
    names(imp)<-varnames

    # seed, lastSeedvalue, number of iterations, chainMean and chainVar is taken as in mids object x.
    seed<-x$seed
    lastSeedvalue<-x$lastSeedvalue
    iteration<-x$iteration
    chainMean=NA
    chainVar=NA

    # padModel for the data rbind(x$data,y).

    pad<-padModel(data,  method, predictorMatrix, visitSequence, post, nmis, nvar=ncol(data))

    x<-list(call=call,data=data,m=m,nmis=nmis,imp=imp,method=method,predictorMatrix=predictorMatrix,
        visitSequence=visitSequence,post=post,seed=seed,iteration=iteration,lastSeedvalue=lastSeedvalue,
        chainMean=chainMean,chainVar=chainVar,pad=pad)
  }
  
  
  oldClass(x) <- "mids"
  return(x)
}


ibind<-function(x,y){
  ## This function combines two midsobjects x and y into a
  ## single midsobject. The two midsobjects should have the same underlying multiple imputation model and should
  ## have exactly the same dataset. If the number of imputations in x is m(x) and y is m(y) then the combination of both
  ## objects contains m(x)+m(y) imputations.
  ## KO 08/09.
 
  call<-match.call()
  call<-c(x$call,call)
  
  if (!is.mids(y)&!is.mids(x)) stop("Both x and y should be a midsobject\n")
  if ((!all(is.na(x$data)==is.na(y$data))) | (!all(x$data[!is.na(x$data)]==y$data[!is.na(y$data)])))
    stop("The data in x and y and/or their reponsepattern do not completely agree")
  if (!all(x$nmis==y$nmis)) stop("Number of missings does is not equal in x and y \n")
  if (!all(x$method==y$method))   stop("Methods vector is not equal in x and y \n")
  if (!all(x$predictorMatrix==y$predictorMatrix))     stop("Predictormatrix is not equal in x and y\n")
  if (!all(x$visitSequence==y$visitSequence))     stop("Visitsequence is not equal in x and y \n")
  if (!all(x$post==y$post))      stop("The post vector is not equal in x and y \n")
  if (!all(x$pad$categories==y$pad$categories))   stop("The categories in the padmodels are not equal in x and y \n")

  varnames<-c(dimnames(x$data)[[2]])
  visitSequence<-x$visitSequence
  imp <- vector("list", ncol(x$data))
  for (j in visitSequence){
      imp[[j]] <- cbind(x$imp[[j]],y$imp[[j]])
    }

  m<-(x$m+y$m)
  iteration<-max(x$iteration,y$iteration)

  chainMean <- chainVar <- array(NA, dim=c(length(visitSequence),iteration,m),dimnames=list(names(visitSequence),1:iteration,paste("Chain",1:m)))
    for(j in 1:x$m) {
        chainMean[,1:x$iteration,j] <- x$chainMean[,,j]
        chainVar[,1:x$iteration,j] <- x$chainVar[,,j]
    }
    for(j in 1:y$m) {
        chainMean[,1:y$iteration,j+x$m] <- y$chainMean[,,j]
        chainVar[,1:y$iteration,j+x$m] <- y$chainVar[,,j]
    }
    
  z<-list(call=call,data=x$data,m=m,nmis=x$nmis,imp=imp,method=x$method,predictorMatrix=x$predictorMatrix,
        visitSequence=visitSequence,post=x$post,seed=x$seed,iteration=iteration,lastSeedvalue=x$lastSeedvalue,
        chainMean=chainMean,chainVar=chainVar,pad=x$pad)
   
  oldClass(z) <- "mids"
  return(z)     
}



#------------------------------COMPLETE------------------------------------

complete <- function(x, action = 1, include = FALSE)
{
# complete
# Takes an object of class "mids", fills in the missing data, and
# return the completed data in the format specified by action.
# If action is a scalar between 1 and x$m, the function 
# returns the data with the action's imputation filled in.
# action can also be one of the following strings:
# "long"    produces a long matrix with nrow(x)*x$m
#       rows, containing all imputed data plus 
#       two additional variables ".id" (containing the 
#       row.names and ".imp" (containing the imputation 
#       number).
# "broad"   produces a broad matrix with ncol(x)*x$m
#       columns. The first ncol(x) columns contain the first
#       imputed data matrix. Column names are changed to reflect the
#       imputation number.
# "repeated"    produces a broad matrix with ncol(x)*x$m
#       columns. The first x$m columns give the filled-in
#       first variable. Column names are changed to reflect the 
#       imputation number.
#
#   Authors: S van Buuren
#   Copyright (c) 2010 TNO Quality of Life
#   Last change: 18/11/2010 SvB
#
    if(!is.mids(x)) stop("Input data must have class 'mids'.")
    if(!is.logical(include)) stop("Argument 'include' should be either TRUE or FALSE.")
    if(is.numeric(action) && action == 0) return(x$data)
    if(is.numeric(action) && action >= 1 && action <= x$m) {
        data <- x$data
        mis <- is.na(data)
        ind <- (1:ncol(data))[colSums(mis) > 0]
        for(j in ind) {         
            if (is.null(x$imp[[j]])) data[mis[,j],j] <- NA   # SvB 10/2/2010
            else data[mis[, j], j] <- x$imp[[j]][, action]   # SvB 10/2/2010
        }
        return(data)
    }
    code <- pmatch(action, c("long", "broad", "repeated"))
    if(!is.na(code) && code >= 1 && code <= 3) {
        m <- x$m
        nr <- nrow(x$data)
        nc <- ncol(x$data)
        add <- ifelse(include, 1, 0)
        mylist <- vector("list", length = m + add)     # SvB 18/11/2010
        for(j in 1:length(mylist)) mylist[[j]] <- Recall(x, j-add)   # recursive
        if (code == 1) {  # long
          data <- do.call(rbind, mylist)
          data <- data.frame(.imp = as.factor(rep((1-add):m, each=nr)),
                             .id  = rep(row.names(x$data), m + add),
                             data)
          row.names(data) <- 1:nrow(data)
        }
        if (code >= 2 && code <= 3) {  # broad or repeated
          data <- do.call(cbind, mylist)
          names(data) <- paste(rep(names(x$data), m), rep((1-add):m, rep(nc, m+add)), sep = ".")
        }
        if (code == 3) {  # repeated
          data <- data[, order(rep(1:nc, m+add))]
        }        
        return(data)
    }
    stop("Argument action not recognized. \n")
}



#------------------------------with.mids----------------------------

with.mids <- function(data, expr, ...)
{
# General function to do repeated analyses.
# Generalisation of lm.mids and glm.mids.
# KO, 2009.
#
# repeated complete data regression on a mids data set.
# Depending on 'expr' different types of regressions are preformed.
# for 'expr' can be used: lm, lme, glm, etc.
# SvB formula deleted, 13Aug09: expr can contain any executable expression
# SvB: now works for both calls and expressions
  
  call <- match.call()
  if (!is.mids(data)) stop("The data must have class mids")
  analyses <- as.list(1:data$m)
  
#  do the repeated analysis, store the result.
#
  for (i in 1:data$m){
    data.i <- complete(data,i)
    analyses[[i]] <- eval(expr = substitute(expr),
                          envir = data.i,
                          enclos = parent.frame())
    if (is.expression(analyses[[i]]))
      analyses[[i]] <- eval(expr = analyses[[i]],
                            envir = data.i,
                            enclos = parent.frame())      
    }
#
# return the complete data analyses as a list of length nimp
#
  object <- list(call=call,call1=data$call,nmis=data$nmis,analyses = analyses)
  # formula=formula(analyses[[1]]$terms))
  oldClass(object)<-c("mira","matrix")
  return(object)
}



#-------------------------------LM.MIDS---------------------------------

#setMethod("lm",signature(data = "mids"),
#   function( formula, data, ...) {
#     lm.mids( formula, data, ...)
#   }
#)


lm.mids <- function(formula, data, ...)
{
#  adapted 28/1/00
#  repeated complete data regression (lm) on a mids data set
#
    call <- match.call()
    if(!is.mids(data))
        stop("The data must have class mids")
    analyses <- as.list(1:data$m)   #
#  do the repated analysis, store the result
#
    for(i in 1:data$m) {
        data.i <- complete(data, i)
        analyses[[i]] <- lm(formula, data = data.i, ...)
    }
#
# return the complete data analyses as a list of length nimp
#
    object <- list(call = call, call1 = data$call, nmis = data$nmis, analyses = analyses)
    oldClass(object) <- c("mira", "lm")  ## FEH
    return(object)
}

#-------------------------------GLM.MIDS---------------------------------
#setMethod("glm",signature(data = "mids"),
#   function( formula, family = gaussian, data, ...) {
#     glm.mids( formula, family = gaussian, data, ...)
#   }
#)

glm.mids <- function(formula, family = gaussian, data, ...)
{
#  adapted 04/02/00
#  repeated complete data regression (glm) on a mids data set
#
    call <- match.call()
    if(!is.mids(data))
        stop("The data must have class mids")
    analyses <- as.list(1:data$m)   #
#  do the repated analysis, store the result
#
    for(i in 1:data$m) {
        data.i <- complete(data, i)
        analyses[[i]] <- glm(formula, family, data = data.i, ...)  ## SvB 22jan13
    }
#
# return the complete data analyses as a list of length nimp
#
    object <- list(call = call, call1 = data$call, nmis = data$nmis, analyses = analyses)
    oldClass(object) <- c("mira", "glm","lm") ## FEH
    return(object)
}

#----------------------------getfit-------------------------------

getfit <- function(x, i= -1, simplify=FALSE) {
  if (!is.mira(x)) return(NULL)
  ra <- x$analyses
  if (i != -1) return(ra[[i]])
  if (simplify) ra <- unlist(ra)
  return(ra)
}


#------------------------------pool-------------------------------

pool <- function (object, method = "smallsample")
{
### General pooling function for multiple imputation parameters
### object: an object of class mira (Multiple Imputed Repeated Analysis)
### Based on Rubin's rules (Rubin, 1987);
#
### Stef van Buuren, Karin Groothuis-Oudshoorn, July 1999.
### Extended for mle (S3) and mer (S4) objects, KO 2009.
### Updated V2.1 - Aug 31, 2009
### Updated V2.2 - Jan 13, 2010
### Updated V2.4 - Oct 12, 2010
### Updated V2.6 - Jan 14, 2011
### Updated V2.12 - Mar 19, 2012
  
### Check the arguments

  call <- match.call()
  if (!is.mira(object))
    stop("The object must have class 'mira'")
  m <- length(object$analyses)
  fa <- getfit(object, 1)
  if (m == 1) {
    warning("Number of multiple imputations m=1. No pooling done.")
    return(fa)
  }
  analyses <- getfit(object)

  if (class(fa)[1]=="lme") require(nlme)  # fixed 13/1/2010
  if (class(fa)[1]=="mer") require(lme4)  # fixed 13/1/2010
  if (class(fa)[1]=="survreg") require(survival)  # added 18/5/2012

###   Set up arrays for object.
  
  mess <- try(coef(fa), silent=TRUE)
  if (inherits(mess,"try-error")) stop("Object has no coef() method.")
  mess <- try(vcov(fa), silent=TRUE)
  if (inherits(mess,"try-error")) stop("Object has no vcov() method.")
  
  if (class(fa)[1]=="mer")               # fixed 13/1/2010
    { 
      k <- length(fixef(fa))
      names <- names(fixef(fa))
    }
  else if (class(fa)[1]=="polr")          # fixed 17/10/2010
    {
      k <- length(coef(fa))+length(fa$zeta)
      names <- c(names(coef(fa)),names(fa$zeta))
    }
  else
    {
      k <- length(coef(fa))
      names <- names(coef(fa))
    }
  
  qhat <- matrix(NA, nrow = m, ncol = k, dimnames = list(1:m, names))
  u <- array(NA, dim = c(m, k, k), dimnames = list(1:m, names,
                                     names))
  
###   Fill arrays
  
  for (i in 1:m) {
    fit <- analyses[[i]]
    if (class(fit)[1]=="mer")
      {
        qhat[i,] <- fixef(fit)
        ui <- as.matrix(vcov(fit))
        if (ncol(ui)!=ncol(qhat)) stop("Different number of parameters: class mer, fixef(fit): ",ncol(qhat),", as.matrix(vcov(fit)): ", ncol(ui))
        u[i , ,] <- ui
      }
    else if (class(fit)[1]=="lme")
      {
        qhat[i,] <- fit$coefficients$fixed
        ui <- vcov(fit)
        if (ncol(ui)!=ncol(qhat)) stop("Different number of parameters: class lme, fit$coefficients$fixef: ",ncol(qhat),", vcov(fit): ", ncol(ui))
        u[i, , ] <- ui
      }
    else if (class(fit)[1]=="polr")
      {
        qhat[i,] <- c(coef(fit),fit$zeta)
        ui <- vcov(fit)
        if (ncol(ui)!=ncol(qhat)) stop("Different number of parameters: class polr, c(coef(fit, fit$zeta): ",ncol(qhat),", vcov(fit): ", ncol(ui))
        u[i, , ] <- ui
      }
    else if (class(fit)[1]=="survreg")
      {
        qhat[i,] <- coef(fit)
        ui <- vcov(fit)
        parnames <- dimnames(ui)[[1]]
        select <- !(parnames %in% "Log(scale)")  ## do not pool Log(scale) columns SvB 18/3/12
        ui <- ui[select, select]
        if (ncol(ui)!=ncol(qhat)) stop("Different number of parameters: class survreg, coef(fit): ",ncol(qhat),", vcov(fit): ", ncol(ui))
        u[i, , ] <- ui
      }
    else
      {
        qhat[i,] <- coef(fit)
        ui <- vcov(fit)
        ### add rows and columns to ui if qhat is missing
        ui <- expandvcov(qhat[i,], ui)
        if (ncol(ui)!=ncol(qhat)) stop("Different number of parameters: coef(fit): ",ncol(qhat),", vcov(fit): ", ncol(ui))
        u[i, , ] <- ui
      }
  }
  
###   Within, between and total variances

  qbar <- apply(qhat, 2, mean)                              # (3.1.2)
  ubar <- apply(u, c(2, 3), mean)                           # (3.1.3)
  e <- qhat - matrix(qbar, nrow = m, ncol = k, byrow = TRUE)
  b <- (t(e) %*% e)/(m - 1)                                 # (3.1.4)
  t <- ubar + (1 + 1/m) * b                                 # (3.1.5)
  
###   Scalar inference quantities
  
  r <- (1 + 1/m) * diag(b/ubar)                             # (3.1.7)
  lambda <- (1 + 1/m) * diag(b/t)
  dfcom <- df.residual(object)
  df <- mice.df(m, lambda, dfcom, method)
  fmi <- (r + 2/(df+3))/(r + 1)                             # fraction of missing information

###
  names(r) <- names(df) <- names(fmi) <- names(lambda) <- names
  fit <- list(call = call, call1 = object$call, call2 = object$call1,
              nmis = object$nmis, m = m, qhat = qhat, u = u, qbar = qbar,
              ubar = ubar, b = b, t = t, r = r, dfcom = dfcom, df = df,
              fmi = fmi, lambda = lambda)
  oldClass(fit) <- c("mipo", oldClass(object))              ## FEH
  return(fit)
}

#---------------------------expandvcov--------------------------------

expandvcov <- function(q, u) {
  err <- is.na(q)
  return(u)
  ## if (all(!err)) return(u)
  ## k <- length(q)
  ## v <- names(q)
  ## z <- u
  ## for (i in 1:ncol(z)){
  ##   if (err[i]) {
  ##     rbind(z[,],NA,z[,])
  ##     j <- j + 1
  ##     up <- 
  ##   }
  ##   j <- j + 1
  ##   z[i,] <- u[j,]
  ##   z[,i] <- u[,j]
  ## }

  ## z <- matrix(NA, ncol=k, nrow=k, dimnames = list(v,v))
  ## idx <- (is.na())
  ## j <- 0
  ## for (i in 1:k){
  ##   if (err[i]) next
  ##   j <- j + 1
  ##   z[i,] <- u[j,]
  ##   z[,i] <- u[,j]
  ## }
  ## return(z)
}


#------------------------------mice.df--------------------------------

mice.df <- function(m, lambda, dfcom, method)
{
  if (is.null(dfcom)) {
    dfcom <- 999999
    warning("Large sample assumed.")
  }
  lambda[lambda<0.0001] <- 0.0001
  dfold <- (m - 1) / lambda^2
  dfobs <- (dfcom+1) / (dfcom+3) * dfcom * (1-lambda)
  df <- dfold * dfobs / (dfold + dfobs)
  if (method != "smallsample") df <- dfold  ## Rubin 1987, 3.1.6, Van Buuren 2012, 2.30, added 31/10/2012
  return(df)
}


#------------------------------pool.scalar----------------------------

pool.scalar <- function(Q,U){
  # Simple pooling function for univariate parameter
  #
  # Based on Rubin's rules (Rubin, 1987);
  #

	m<-length(Q)
	qbar <- mean(Q)                        # (3.1.2)
	ubar <- mean(U)                        # (3.1.3)
	b<-var(Q)                              # (3.1.4)
	t<-ubar+(m+1)*b/m                      # (3.1.5)
	r <- (1+1/m)*b/ubar                    # (3.1.7)
	df <- (m-1)*(1 + 1/r)^2                # (3.1.6)
	f <- (r+2/(df + 3))/(r+1)              # (3.1.10)
  fit <- list(m = m, qhat = Q, u = U, qbar = qbar,
         ubar = ubar, b = b, t = t, r = r, df = df, f = f)
  return(fit)
}

#--------------------------pool.r.squared--------------------------

pool.r.squared <- function(object, adjusted=FALSE){
#
# pooled rsquared for multiple imputed datasets.
#
# object: object of class mira
# based on article of O. Harel (Journal of Applied Statistics, 2009).

    call <- match.call()
    if (!is.mira(object))
        stop("The object must have class 'mira'")
    if ((m <- length(object$analyses)) < 2)
        stop("At least two imputations are needed for pooling.\n" )
    if (class((object$analyses[[1]]))[1]!="lm")
        stop("r^2 can only be calculated for results of the 'lm' modelling function")
#
#   Set up array r2 to store R2 values, Fisher z-transformations of R2 values
#   and its variance.
#
    analyses <- object$analyses
    m <- length(analyses)
    r2 <- matrix(NA, nrow = m, ncol = 3, dimnames=list(1:m,c("R^2","Fisher trans F^2","se()")))
#
#   Fill arrays
#
    for (i in 1:m) {
        fit <- analyses[[i]]
        if (adjusted==FALSE) r2[i,1] <- sqrt(summary(fit)$r.squared)
        else r2[i,1]<- sqrt(summary(fit)$adj.r.squared)
        r2[i,2] <- 0.5*log((r2[i,1]+1)/(1 - r2[i,1]))
        r2[i,3] <- 1/(length(summary(fit)$residuals)-3)
    }
#
#   Compute within, between and total variances following Rubin's rules.
#   with function pool.scalar().
#
    fit<-pool.scalar(r2[,2],r2[,3])

#   Make table with results.
    table <- array(((exp(2*fit$qbar)-1)/(1+exp(2*fit$qbar)))^2, dim = c(1,4))

    if (adjusted==FALSE) dimnames(table) <- list("R^2", c("est", "lo 95", "hi 95", "fmi"))
    else dimnames(table) <- list("adj R^2", c("est", "lo 95", "hi 95", "fmi"))

    table[, 2] <- ((exp(2*(fit$qbar-1.96*sqrt(fit$t)))-1)/(1+exp(2*(fit$qbar-1.96*sqrt(fit$t)))))^2
    table[, 3] <- ((exp(2*(fit$qbar+1.96*sqrt(fit$t)))-1)/(1+exp(2*(fit$qbar+1.96*sqrt(fit$t)))))^2
    table[, 4] <- fit$f
    return(table)
}

#--------------------------pool.compare----------------------------

pool.compare <- function (fit1, fit0, data=NULL, method = "Wald")
{
# It is assumed that fit1 contains the larger model
# and the model in fit0 is nested within fit1.
# In case of method=Wald the null hypothesis is tested that the extra parameters are all zero.                                                                       
# Usage of this function: pool.compare(fit1, fit0).
# The case of method=likelihood refers to the method of Meng & Rubin (1992), based 
# on complete data log likelihood ratios to combine likelihood ratio tests. So far only the 
# likelihood function of logistic regression is implemented.
# For the likelihood method, data should refer to the original mids-object.

LLlogistic<-function(formula, data, coefs){
    ### Calculates -2 loglikelihood of a model.
    ### 
    logistic<-function(mu) exp(mu)/(1+exp(mu))
    Xb<-model.matrix(formula,data)%*%coefs
    y<-model.frame(formula,data)[1][,1]
    p<-logistic(Xb)
    y<-(y-min(y))/(max(y)-min(y)) ## in case values of categorical var are other than 0 and 1.
    term1<-term2<-rep(0,length(y))
    term1[y!=0]<-y[y!=0]*log(y[y!=0]/p[y!=0])
    term2[y==0]<-(1-y[y==0])*log((1-y[y==0])/(1-p[y==0]))
    return(-(2*sum(term1+term2)))
}

# Check the arguments
#
    call <- match.call()
    meth <- match.arg(tolower(method),c("wald","likelihood"))

    if (!is.mira(fit1)|!is.mira(fit0))
        stop("fit1 and fit0 should both have class 'mira'.\n")
    m1 <- length(fit1$analyses)
    m0 <- length(fit0$analyses)
    if (m1 != m0)
        stop("Number of imputations differs between fit1 and fit0.\n" )
     if (m1 < 2)
        stop("At least two imputations are needed for pooling.\n" )
   
    m <- m1
    est1 <- pool(fit1)
    est0 <- pool(fit0)
    dimQ1 <- length(est1$qbar)
    dimQ2 <- dimQ1-length(est0$qbar)
    formula1 <- formula(fit1$analyses[[1]]$terms)
    formula0 <- formula(fit0$analyses[[1]]$terms)

    if (dimQ2<1) stop("The larger model should be specified first and must be strictly larger than the smaller model.\n")
    if (!setequal(all.vars(formula0),intersect(all.vars(formula0),all.vars(formula1))))
        stop("The smaller model should be fully contained in the larger model. \n")
    if (!all(charmatch(all.vars(formula0),all.vars(formula1))==(1:length(all.vars(formula0)))))
         stop("The first variables of the larger model should be the variables of the smaller model in the same order.\n")           
    
    if (meth=="wald")   { 
    # Reference: paragraph 2.2, Article Meng & Rubin, Biometrika, 1992.
    # When two objects are to be compared we need to calculate the matrix Q.    
      Q <- diag(rep(1,dimQ1),ncol=dimQ1)
      Q <- matrix(Q[((dimQ1-dimQ2+1):dimQ1),],nrow=dimQ2,ncol=dimQ1)
      qbar <- Q %*% est1$qbar
      Ubar <- Q %*% est1$ubar %*% (t(Q))
      Bm <- Q %*% est1$b %*% (t(Q))
      rm <-(1 + 1/m) * sum(diag(Bm%*%(solve(Ubar))))/dimQ2
      Dm <- (t(qbar)) %*% (solve(Ubar)) %*% qbar / (dimQ2*(1+rm))
    }   
    
    if (meth=="likelihood") {
      if (is.null(data)) stop ("For method=likelihood the imputed data set (a mids object) should be included.\n") 
 
      devM<-devL<-0
      for (i in (1: m)){ 
        # Calculate for each imputed dataset the deviance between the two models
        # with the pooled coefficients.                    
        devL<-devL + LLlogistic(formula1,complete(data,i),est1$qbar)-
                          LLlogistic(formula0,complete(data,i),est0$qbar)
        # Calculate for each imputed dataset the deviance between the two models
        # with its estimated  coefficients. 
        devM<-devM + LLlogistic(formula1,complete(data,i),est1$qhat[i,])-
                          LLlogistic(formula0,complete(data,i),est0$qhat[i,])
      }  
      devL<-devL/m 
      devM<-devM/m  
      rm<-((m+1)/(dimQ2 * (m-1) ) )*(devM-devL)   # SvB 17jan2011
      Dm<-devL/(dimQ2 * (1+rm))
    }   

    # Calculation of degrees of freedom for F distribution; the same for both methods.  
    v<-dimQ2 * (m-1)
    if (v>4) w <- 4 + (v-4)*((1+(1-v/2)*(1/rm))^2)
    else w <- v*(1+1/dimQ2)*((1+1/rm)^2)/2

    statistic <- list(call = call, call11 = fit1$call, call12 = fit1$call1,
        call01 = fit0$call, call02 = fit0$call1, method = method,
        nmis = fit1$nmis, m = m, qhat1 = est1$qhat, qhat0 = est0$qhat,
        qbar1 = est1$qbar, qbar0 = est0$qbar, ubar1 = est1$ubar, 
        ubar0 = est0$ubar,  Dm = Dm, rm = rm, df1 = dimQ2, df2 = w, 
        pvalue = 1 - pf(Dm,dimQ2,w))
    return(statistic)
   }     


df.residual.mira <- function(object, ...){
  fit <- object$analyses[[1]]
  return(df.residual(fit))
}

df.residual.lme <- function(object, ...){
  return(object$fixDF[["X"]][1])
}

df.residual.mer <- function(object, ...){
  return(sum(object@dims[2:4]*c(1,-1,-1))+1)
}

df.residual.default <- function(object, q=1.3, ...)
{
  df <- object$df.residual
  if (!is.null(df)) return(df)
  
  mk <- try(c <- coef(object), silent=TRUE)
  mn <- try(f <- fitted(object), silent=TRUE)
  if (inherits(mk,"try-error") | inherits(mn,"try-error")) return(NULL)
  n <- ifelse(is.data.frame(f) | is.matrix(f), nrow(f), length(f))
  k <- length(c)
  if (k==0 | n==0) return(NULL)
  return(max(1,n-q*k))
}


# miceNews <- function() {
#     file.show(system.file("doc", "NEWS.txt", package = "mice"))
# }


mids2spss <- function(imp, filedat="midsdata.txt", filesps="readmids.sps",
                      path=getwd(), sep="\t", dec=".", silent=FALSE) {

  miceWriteForeignSPSS <- function (df, datafile, codefile, varnames = NULL, dec=".", sep="\t") 
    {
      ##adapted version of writeForeignSPSS from foreign package to write mids-objects
      adQuote <- function (x) paste("\"", x, "\"", sep = "")
      dfn <- lapply(df, function(x) if (is.factor(x)) as.numeric(x) else x)
      eol <- paste(sep,"\n",sep="")
      write.table(dfn, file = datafile, row.names = FALSE, col.names = FALSE, 
                  sep = sep, dec = dec, quote = FALSE, na = "", eol=eol)
      varlabels <- names(df)
      if (is.null(varnames)) {
        varnames <- abbreviate(names(df), 8L)
        if (any(sapply(varnames, nchar) > 8L)) 
          stop("I cannot abbreviate the variable names to eight or fewer letters")
        if (any(varnames != varlabels)) 
          warning("some variable names were abbreviated")
      }
      varnames <- gsub("[^[:alnum:]_\\$@#]", "\\.", varnames)
      dl.varnames <- varnames
      if (any(chv <- sapply(df, is.character))) {
        lengths <- sapply(df[chv], function(v) max(nchar(v)))
        if (any(lengths > 255L)) 
          stop("Cannot handle character variables longer than 255")
        lengths <- paste("(A", lengths, ")", sep = "")
        star <- ifelse(c(FALSE, diff(which(chv) > 1L)), " *", 
                       " ")
        dl.varnames[chv] <- paste(star, dl.varnames[chv], lengths)
      }
      if (sep=="\t") freefield <- " free (TAB)\n"
      if (sep!="\t") freefield <- cat(' free (\"',sep,'\")\n',sep="")
      cat("DATA LIST FILE=", adQuote(datafile), freefield, 
          file = codefile)
      cat(" /", dl.varnames, ".\n\n", file = codefile, append = TRUE, fill=60, labels=" ")
      cat("VARIABLE LABELS\n", file = codefile, append = TRUE)
      cat(" ",paste(varnames, adQuote(varlabels), "\n"), ".\n", file = codefile, 
          append = TRUE)
      factors <- sapply(df, is.factor)
      if (any(factors)) {
        cat("\nVALUE LABELS\n", file = codefile, append = TRUE)
        for (v in which(factors)) {
          cat(" /", varnames[v], "\n", file = codefile, append = TRUE)
          levs <- levels(df[[v]])
          for (u in 1:length(levs)) 
            cat(paste("  ",seq_along(levs)[u], adQuote(levs)[u], sep = " "), 
                file = codefile, append = TRUE, fill=60)
        }
        cat(" .\n", file = codefile, append = TRUE)
      }
      cat("\nEXECUTE.\n", file = codefile, append = TRUE)
      cat("SORT CASES by Imputation_.\n",  file = codefile, append = TRUE)
      cat("SPLIT FILE layered by Imputation_.\n",  file = codefile, append=TRUE)
    }
  
  if(!is.mids(imp)) stop("Exports only objects of class 'mids'.")
  imputed <- complete(imp, "long", include=TRUE)[,-2]
  names(imputed)[1] <- "Imputation_"
  f <- imputed[,"Imputation_"]
  imputed[,"Imputation_"] <- as.numeric(c(levels(f),NA))[f]
  if (!is.null(path)) {
    filedat <- file.path(path,filedat)
    filesps <- file.path(path,filesps)
  }
  miceWriteForeignSPSS(imputed, filedat, filesps, varnames=names(imputed),sep=sep,dec=dec)
  if (!silent) {
    cat("Data values written to",filedat,"\n")
    cat("Syntax file written to",filesps,"\n")
  }
}

supports.transparent <- function(){
  query <- dev.capabilities("semiTransparency")$semiTransparency
  if (is.na(query)) query <- FALSE
  return(query)
}


mdc <- function(r="observed", s="symbol", transparent=TRUE,
               cso = hcl(240,100,40,0.7),
               csi = hcl(0,100,40,0.7),
               csc = "gray50",
               clo = hcl(240,100,40,0.8),
               cli = hcl(0,100,40,0.8), 
               clc = "gray50")
{
  ## cso: blue symbol color for observed data
  ## csi: red symbol color for imputations
  ## csc: symbol color for combined data
  ## clo: blue line color for observed data
  ## cli: red line color for observed data
  ## clc: line color for combined data

  if (missing(transparent)) {
    if (!supports.transparent()) {
      cso <- hcl(240,100,40)
      csi <- hcl(0,100,40)
      csc <- "black"
      clo <- hcl(240,100,40)
      cli <- hcl(0,100,40)
      clc <- "black"
    }
  } else if (transparent == FALSE) {
      cso <- hcl(240,100,40)
      csi <- hcl(0,100,40)
      csc <- "black"
      clo <- hcl(240,100,40)
      cli <- hcl(0,100,40)
      clc <- "black"
  }
    
  fallback <- palette()[1]
  if(is.numeric(r)){
    idx <- floor(r)
    idx[r<1 | r>6] <- 7
    myc <- c(cso, csi, csc, clo, cli, clc, fallback)[idx]
    return(myc)
  }
  rc <- pmatch(r, c("observed", "missing", "both"))
  sc <- pmatch(s, c("symbol", "line"))
  idx <- rc + (sc-1)*3
  idx[is.na(idx)] <- 7
  myc <- c(cso, csi, csc, clo, cli, clc, fallback)[idx]
  return(myc)      
}

