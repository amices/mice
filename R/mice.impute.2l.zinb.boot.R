mice.impute.2l.zinb.boot <-
function(y, ry, x, type){
  require(pscl) 
  if (!require(pscl)){stop("pscl package must be 
  installed!")}
  Y <- y[ry]
  X <- x[ry,]
  X <- data.frame(X)
  nam <- colnames(X)
  b <- which(type==1) # variables used in zero AND count model
  c <- which(type==2) # count model ONLY variables
  z <- which(type==3) # zero model ONLY variables
  zero <- c(b,z); zero <- unique(zero); zero <- sort(zero)
  count <- c(b,c); count <- unique(count); count <- sort(count)
  form <-
  as.formula(paste("Y","~",paste(nam[count],collapse="+"),
  "|",paste(nam[zero],collapse="+")))
  dat <- data.frame(Y,X)
  datBS <- dat[sample(1:length(Y),length(Y),replace=TRUE),]
  fit <- zeroinfl(form,
  data=datBS,dist="negbin",link="logit")
  newdata <- data.frame(X=x[!ry,])
  colnames(newdata) <- nam
  pc <- predict(fit,
  newdata=newdata,type="prob",na.action=na.pass)
  pcvec <- 1:nrow(pc)
  for (i in 1:nrow(pc))
  {
    pcvec[i] <-
    sample(as.numeric(names(pc[i,])),1, pc[i,],
    replace=TRUE)
  }
  return(pcvec)
}
