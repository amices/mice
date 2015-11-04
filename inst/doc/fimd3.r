### fimd3.R
### R code from Van Buuren, S. (2012). 
###		Flexible Imputation of Missing Data. 
###		CRC/Chapman & Hall, Boca Raton, FL.
### (c) 2012 Stef van Buuren, www.multiple-imputation.com
### Version 1, 7mar2012
### Version 2, 4nov2015 tested with mice 2.23
### Tested with Mac OS X 10.7.3, R2.14-2, mice 2.11

library("mice")
library("lattice")
library("gamlss")
library("rpart")

### Section 3.1 How to generate multiple imputations

### In order to draw figure 3.1, we define two tweaked
### mice.impute.xxx functions that will store the coefficients
### of the imputation models in the global environment.
### Normally, we would not be so interested in the
### parameter estimates of the imputation model, but here
### we need them for Figure 3.1.
mice.impute.normdump <- function (y, ry, x, ...) 
{
    x <- cbind(1, as.matrix(x))
    parm <- .norm.draw(y, ry, x, ...)
    betadump <<- c(betadump,parm$beta) 
    return(x[!ry, ] %*% parm$beta + rnorm(sum(!ry)) * parm$sigma)
}
mice.impute.pmmdump <- function (y, ry, x, ...) 
{
    x <- cbind(1, as.matrix(x))
    parm <- .norm.draw(y, ry, x, ...)
    yhatobs <- x[ry, ] %*% parm$coef
    yhatmis <- x[!ry, ] %*% parm$beta
    betadump <<- c(betadump,parm$beta)
    return(apply(as.array(yhatmis), 1, .pmm.match, yhat = yhatobs, 
        y = y[ry], ...))
}


### Figure 3.1
par(mfrow=c(3,2))
data <- whiteside
lwd <- 1.5
plot(x=data$Temp, y=data$Gas, col=mdc(1), lwd=lwd, 
     xlab=expression(paste("Temperature (", degree, "C)")), 
     ylab="Gas consumption (cubic feet)")
points(x=5, y=3.6, pch=4, cex=2, lwd=lwd, col=mdc(2))
legend(x="bottomleft", legend="deleted observation", pch=4, col=mdc(2), 
       pt.lwd=lwd, bty="n", pt.cex=2)
text(x=9, y=6.5, label="a",cex=2)

data[47,"Gas"] <- NA
plot(x=data$Temp, y=data$Gas, col=mdc(1), lwd=lwd, 
     xlab=expression(paste("Temperature (", degree, "C)")), 
     ylab="Gas consumption (cubic feet)")
abline(m1<-lm(Gas~Temp, data=data, na.action=na.omit), col=mdc(4))
points(5,4.04, lwd=lwd, col=mdc(2),pch=19)
text(x=9, y=6.5, label="b",cex=2)

plot(x=data$Temp, y=data$Gas, col=mdc(1), lwd=lwd, 
     xlab=expression(paste("Temperature (", degree, "C)")), 
     ylab="Gas consumption (cubic feet)")
imp <- mice(data, m=1, maxit=0)
pred <- imp$pred
pred["Gas","Insul"] <- 0
imp <- mice(data, m=5, pred=pred, meth="norm.nob", maxit=1, print=FALSE, seed=45433)
abline(m1<-lm(Gas~Temp, data=data, na.action=na.omit), col=mdc(4))
points(rep(5,5),imp$imp$Gas, lwd=lwd, col=mdc(2),pch=19)
text(x=9, y=6.5, label="c",cex=2)

plot(x=data$Temp, y=data$Gas, col=mdc(1), lwd=lwd, 
     xlab=expression(paste("Temperature (", degree, "C)")), 
     ylab="Gas consumption (cubic feet)")
imp <- mice(data, m=1, maxit=0)
pred <- imp$pred
pred["Gas","Insul"] <- 0
betadump <- vector("list", 0) 
imp <- mice(data, m=5, pred=pred, meth="normdump", maxit=1, print=FALSE, seed=83126)
abline(m1<-lm(Gas~Temp, data=data, na.action=na.omit), col=mdc(4))
betadump <- matrix(betadump, nc=2, byrow=TRUE)
for (i in 1:5) abline(coef=unlist(betadump[i,]), col=mdc(5))
points(rep(5,5),imp$imp$Gas, lwd=lwd, col=mdc(2),pch=19)
text(x=9, y=6.5, label="d",cex=2)

pch <- c(rep(3,26),rep(1,30))
plot(x=data$Temp, y=data$Gas, col=mdc(1), lwd=lwd, pch=pch, 
     xlab=expression(paste("Temperature (", degree, "C)")), 
     ylab="Gas consumption (cubic feet)")
imp <- mice(data, m=5, meth="norm", maxit=1, print=FALSE, seed=11727)
abline(m1<-lm(Gas~Temp, data=data, na.action=na.omit, subset=Insul=="Before"), col=mdc(4))
abline(m2<-lm(Gas~Temp, data=data, na.action=na.omit, subset=Insul=="After"), col=mdc(4))
points(rep(5,5),imp$imp$Gas, lwd=lwd, col=mdc(2),pch=19)
legend(x="bottomleft", legend=c("before insulation","after insulation"), pch=c(3,1),bty="n", pt.lwd=lwd)
text(x=9, y=6.5, label="e",cex=2)

pch <- c(rep(3,26),rep(1,30))
plot(x=data$Temp, y=data$Gas, col=mdc(1), lwd=lwd, pch=pch, 
     xlab=expression(paste("Temperature (", degree, "C)")), 
     ylab="Gas consumption (cubic feet)")
betadump <- vector("list", 0) 
imp <- mice(data, m=5, meth="pmmdump", maxit=1, print=FALSE, seed=68006)
betadump <- matrix(betadump, nc=3, byrow=TRUE)
m1<-lm(Gas~Temp+Insul, data=data, na.action=na.omit)
an <- coef(m1)[1]
ai <- an + coef(m1)[3]
b <- coef(m1)[2]
abline(a=ai, b=b, col=mdc(4))
abline(a=an, b=b, col=mdc(4))
eta <- 0.6
ylo <- ai+b*(5-eta)
yhi <- ai+b*(5+eta)
lines(x=c(5-eta,5+eta),y=c(ylo,yhi),lwd=3,col=mdc(5))
xlo <- (ylo-an)/b
xhi <- (yhi-an)/b
lines(x=c(xlo,xhi),y=c(ylo,yhi),lwd=3,col=mdc(5))

donors <- subset(data, (Insul=="After"&Temp>5-eta&Temp<5+eta) 
                 |    (Insul=="Before"&Temp>xlo&Temp<xhi))
points(x=donors$Temp, y=donors$Gas, cex=1.8, col=mdc(5), lwd=lwd)
legend(x="bottomleft", legend=c("before insulation","after insulation"), pch=c(3,1),bty="n", pt.lwd=lwd)
text(x=9, y=6.5, label="f",cex=2)


### Section 3.2.3 Performance

### number of simulations
### use nsim <- 10 for testing
###     nsim <- 10000 for the real stuff (TIME CONSUMING)
nsim <- 10

### create data
createdata <- function(beta0=5.49, beta1=-0.29, sigma=0.86, n=50, mx=5, sdx=3) { 
  x <- round(rnorm(n,mean=mx,sd=sdx),1)
  eps <- rnorm(n,mean=0,sd=sigma)
  y <- round(beta0 + x * beta1 + eps, 1)
  return(data.frame(x=x, y=y))
}

### make 50% random missing data
makemissing <- function(data, p=0.5){
  rx <- rbinom(nrow(data), 1, p)
  data[rx==0,"y"] <- NA
  return(data)
}

### test function for three imputation functions
test.impute <- function(data, m=5, method="norm", ...){
  imp <- mice(data, method=method, m=m, print=FALSE, ridge=0, ...)
  fit <- with(imp, lm(y~x))
  est <- pool(fit)
  tab <- summary(est)
  return(tab["x",c("est","se","lo 95","hi 95","fmi","lambda")])
}

### put everything together
simulate <- function(nsim=10, seed=41872){
  set.seed(seed)
  res <- array(NA,dim=c(4, nsim, 6))
  dimnames(res) <- list(c("predict","pred + noise","Bayes MI","boot MI"),
                      as.character(1:nsim),
                      c("est","se","lo 95","hi 95","fmi","lambda"))
  im <- c("norm.predict","norm.nob","norm","norm.boot")
  for(i in 1:nsim){
    data <- createdata()
    data <- makemissing(data)
    res[1,i,] <- test.impute(data, method=im[1], m=2)
    res[2,i,] <- test.impute(data, method=im[2])
    res[3,i,] <- test.impute(data, method=im[3])
    res[4,i,] <- test.impute(data, method=im[4])
  }
  return(res)
}

summarize.results <- function(res) {
  apply(res,c(1,3),mean,na.rm=TRUE)

  ## bias of beta1
  true <- -0.29
  bias <- rowMeans(res[,,1] - true)

  ## coverage
  isin <- res[,,3] < true & true < res[,,4]
  cov <- rowMeans(isin)

  ## average width of 95 c.i.
  intwidth <- res[,,4] - res[,,3]
  aiw <- rowMeans(intwidth)

  return(list(bias=bias, cov=cov, aiw=aiw))
}

### do the simulations - MAY BE TIME CONSUMING
res <- simulate(nsim)
summarize.results(res)

### function to simulate performance of CCA
simulate.cca <- function(nsim=10, seed=41872){

  set.seed(seed)
  res <- array(NA,dim=c(1, nsim, 6))
  for(i in 1:nsim){
    data <- createdata()
    data <- makemissing(data)
    fit <- lm(y~x, data=data, na.action=na.omit)
    est <- coef(summary(fit))
    lo95 <- est[,1] - qt(0.975,fit$df) * est[,2]
    hi95 <- est[,1] + qt(0.975,fit$df) * est[,2]
    est <- cbind(est, lo95, hi95)
    res[1,i,] <- c(est["x",c(1:2,5:6)],NA,NA)
  }
  return(res)
}

### simulate - MAY BE TIME CONSUMING
res.cca <- simulate.cca(nsim)
### cannot be analysed by summarize.results() but it should
### be obvious how to calculate bias, coverage and aiw

### Section 3.2.4 Generating MAR missing data

### script to create missing data according to MARRIGHT
logistic <- function(x) exp(x)/(1+exp(x))
set.seed(32881)
n <- 10000
y <- mvrnorm(n=n,mu=c(5,5),Sigma=matrix(c(1,0.6,0.6,1),nrow=2))
p2.marright <- 1 - logistic(-5 + y[,1])
r2.marright <- rbinom(n, 1, p2.marright)
yobs <- y
yobs[r2.marright==0, 2] <- NA

### Figure 3.2

grid <- seq(0,10,0.1)
mr <- logistic(-5 + grid)
mm <- logistic( 0.75-abs(grid-5))
mt <- logistic(-0.75+abs(grid-5))

par(mfrow=c(1,1))
z <- data.frame(grid, mr, mm, mt) 
matplot(x=z[,1],y=z[,2:4], type="l", lty=1:3, col=mdc(5) , lwd=2,
        xlab="Y1", ylab="Missingness in Y2", las=1)
legend(x="top", bty="n",
       legend=c("MARRIGHT","MARMID","MARTAIL"), lty=1:3, lwd=2, col=mdc(5), cex=0.8)

### Figure 3.3

y3 <- rbind(y,y,y)
p2.marmid <- 1 - logistic(0.75-abs(y[,1]-5))
p2.martail <- 1 - logistic(0.75+abs(y[,1]-5))
r2.marmid <- rbinom(n, 1, p2.marright)
r2.martail <- rbinom(n, 1, p2.martail)
r2 <- c(r2.marright,r2.marmid,r2.martail)
r2 <- factor(r2, labels=c("Ymis","Yobs"))
typ <- factor(rep(3:1,each=n),labels=c("MARTAIL","MARMID","MARRIGHT"))
d <- data.frame(y1=y3[,1],y2=y3[,2],r2=r2,typ=typ)
trellis.par.set(box.rectangle=list(col=c(mdc(2),mdc(1)),lwd=1.2))
trellis.par.set(box.umbrella=list(col=c(mdc(2),mdc(1)),lwd=1.2))
trellis.par.set(plot.symbol=list(col="transparent",lwd=1))
tp <- bwplot(r2~y2|typ, data=d,
             horizontal=TRUE, layout=c(1,3),
             xlab=expression(Y2),
             col=c(mdc(2),mdc(1)),strip=FALSE, xlim=c(2,8),
             strip.left = strip.custom(bg="grey95"))
print(tp)


### Section 3.2.2 Imputation from the t-distribution

mice.impute.TF <- function(y, ry, x, 
                  gamlss.trace = FALSE, ...)
{
  require(gamlss)

  # prepare data
  xobs <- x[ry, , drop = FALSE]
  xmis <- x[!ry, , drop = FALSE]
  yobs <- y[ry]
  n1 <- sum(ry)
  n0 <- sum(!ry)

  # draw bootstrap sample
  s <- sample(n1, n1, replace = TRUE)
  dotxobs <- xobs[s, , drop = FALSE]
  dotyobs <- yobs[s]
  dotxy <- data.frame(dotxobs, y = dotyobs)
  
  # fit the gamlss model
  fit <- gamlss(y ~ ., data = dotxy, family = TF, 
                trace = gamlss.trace, ...)
  yhat <- predict(fit, data=dotxy, newdata = xmis)
  sigma <- exp(coef(fit, "sigma"))
  nu <- exp(coef(fit, "nu"))
  
  # draw the imputations
  return(rTF(n0, yhat, sigma, nu))
}


### Section 3.3.3 Example
data(db)

### Figure 3.4

par(mfrow=c(1,2))
data <- subset(db, age>1&age<2, c("age","head"))
names(data) <- c("age","hc")
truehist(data$hc, col=mdc(1), border="white", 
         xlab="Head circumference (cm)",
         ylab="Density",
         ylim=c(0,0.3), xlim=c(38,60),
         nbins=44)

mn <- gamlss(hc~1, data=na.omit(data), family=NO, trace=FALSE)
mu <- coef(mn)
sigma <- exp(coef(mn, "sigma"))
cmfine <- seq(38,60,0.1)
lines(cmfine, dNO(cmfine, mu, sigma), lwd=0.8, lty=2) 
mt <- gamlss(hc~1, data=data, family=TF, trace=FALSE)
mu <- coef(mt)
sigma <- exp(coef(mt, "sigma"))
nu <- exp(coef(mt, "nu"))
lines(cmfine, dTF(cmfine, mu, sigma, nu), lwd=0.8, lty=1) 
legend(x="right",legend=c("normal","t, df=6.7"), 
       lwd=0.8, lty=c(2,1), bty="n", cex=0.7)

plot(x=data$age, y=data$hc, col=mdc(1), cex=0.3,
     xlab="Age (in years)",
     ylab="Head circumference (cm)",
     ylim=c(39,60))


### create a synthetic data set
data(db)
data <- subset(db, age>1&age<2, c("age","head"))
names(data) <- c("age","hc")
synthetic <- rep(c(FALSE,TRUE), each=nrow(data))
data2 <- rbind(data, data)
data2[synthetic,"hc"] <- NA
imp <- mice(data2, m=1, meth="TF", seed=36650, print=FALSE)
syn <- subset(complete(imp), synthetic)

### Figure 3.5

data <- syn
truehist(data$hc, col=mdc(2), border="white", 
         xlab="Head circumference (cm)",
         ylab="Density",
         ylim=c(0,0.3), xlim=c(38,60),
         nbins=44)

mn <- gamlss(hc~1, data=na.omit(data), family=NO, trace=FALSE)
mu <- coef(mn)
sigma <- exp(coef(mn, "sigma"))
cmfine <- seq(38,60,0.1)
lines(cmfine, dNO(cmfine, mu, sigma), lwd=0.8, lty=2) 
mt <- gamlss(hc~1, data=data, family=TF, trace=FALSE)
mu <- coef(mt)
sigma <- exp(coef(mt, "sigma"))
nu <- exp(coef(mt, "nu"))
lines(cmfine, dTF(cmfine, mu, sigma, nu), lwd=0.8, lty=1) 
legend(x="right",legend=c("normal",paste("t, df=",round(nu,1),sep="")), 
       lwd=0.8, lty=c(2,1), bty="n", cex=0.7)
plot(x=data$age, y=data$hc, col=mdc(2), cex=0.3,
     xlab="Age (in years)",
     ylab="Head circumference (cm)",
     ylim=c(39,60))

### Section 3.4.1 PMM, Overview
### Figure 3.6

data <- boys[boys$age<=2,c("age","bmi")]
set.seed(87120)
data[sample(92:136,10),"bmi"] <- NA
imp <- mice(data, meth="norm", m=1, seed=32212, print=FALSE)
cd1 <- complete(imp)
imp <- mice(data, m=1, seed=32212, print=FALSE)
cd2 <- complete(imp)
r <- !is.na(data$bmi)
plot(data, col=mdc(1), xlab="Age", ylab="BMI")
points(cd1[!r,], col=mdc(2), pch=19, cex=0.8)
plot(data, col=mdc(1), xlab="Age", ylab="BMI")
points(cd2[!r,], col=mdc(2), pch=19, cex=0.8)

### Section 3.4.2 PMM, Computational details
### Figure 3.7

par(mfrow=c(1,1))
data <- whiteside
lwd <- 1.5
data[47,"Gas"] <- NA

pch <- c(rep(3,26),rep(1,30))
plot(x=data$Temp, y=data$Gas, col=mdc(1), lwd=lwd, pch=pch, 
     xlab=expression(paste("Temperature (", degree, "C)")), 
     ylab="Gas consumption (cubic feet)")
betadump <- vector("list", 0) 
imp <- mice(data, m=5, meth="pmmdump", maxit=1, print=FALSE, seed=68006)
betadump <- matrix(unlist(betadump), nc=3, byrow=TRUE)
m1<-lm(Gas~Temp+Insul, data=data, na.action=na.omit)
an <- coef(m1)[1]
ai <- an + coef(m1)[3]
b <- coef(m1)[2]
abline(a=ai, b=b, col=mdc(4))
abline(a=an, b=b, col=mdc(4))
## for (i in 56:56) {
##    abline(a=unlist(betadump[i,1]), b=unlist(betadump[i,2]), col=mdc(5))
##    abline(a=unlist(betadump[i,1])+unlist(betadump[i,3]), b=unlist(betadump[i,2]), col=mdc(5))
## }
## points(rep(5,5),imp$imp$Gas, lwd=lwd, col=mdc(2), pch=20) 
eta <- 0.6
ylo <- ai+b*(5-eta)
yhi <- ai+b*(5+eta)
lines(x=c(5-eta,5+eta),y=c(ylo,yhi),lwd=3,col=mdc(4))
an <- 7.05; ai<-an-1.7; b <- -0.38
xlo1 <- (ylo-ai)/b
xhi1 <- (yhi-ai)/b
xlo2 <- (ylo-an)/b
xhi2 <- (yhi-an)/b
abline(a=an, b=b, col=mdc(5))
abline(a=ai, b=b, col=mdc(5))
lines(x=c(xlo1,xhi1),y=c(ylo,yhi),lwd=3,col=mdc(5))
lines(x=c(xlo2,xhi2),y=c(ylo,yhi),lwd=3,col=mdc(5))
abline(v=c(5-eta,5+eta),h=c(ylo,yhi),col=mdc(4),lty=3)
rect(xlo1,0,xhi1,8,col=hcl(0,100,40,0.05),border=NA)
rect(xlo2,0,xhi2,8,col=hcl(0,100,40,0.05),border=NA)
# abline(v=c(xlo1,xhi1,xlo2,xhi2),col=mdc(5),lty=3)

donors <- subset(data, (Insul=="After"&Temp>xlo1&Temp<xhi1) 
                 |    (Insul=="Before"&Temp>xlo2&Temp<xhi2))
points(x=donors$Temp, y=donors$Gas, cex=1.8, col=mdc(5), lwd=lwd)
legend(x="bottomleft", legend=c("before insulation","after insulation"), pch=c(3,1),bty="n", pt.lwd=lwd)


### Section 3.4.3 PMM, Algorithm 

### simulate function used to calculate Table 3.3
simulate <- function(nsim=10, seed=41872){
  set.seed(seed)
  res <- array(NA,dim=c(3, nsim, 6))
  for(i in 1:nsim){
    data <- createdata()
    data <- makemissing(data)
    res[1,i,] <- test.impute(data, m=5)
    res[2,i,] <- test.impute(data, m=5, donors=1)
    res[3,i,] <- test.impute(data, m=5, donors=10)
  }
  return(res)
}

### and perform simulations - MAY BE TIME CONSUMING
res.pmm <- simulate(nsim)
summarize.results(res.pmm)


### Section 3.7.1
### Figure 3.8

par(mfrow=c(1,2))
fit <- rpart(Gas ~ Temp + Insul, data=whiteside)
plot(fit, branch=0, margin=0.15)
text(fit, use=T,pretty=0,dig=3,cex=0.8)

leaf <- row.names(fit$frame)[fit$where]
label <- factor(leaf, labels=c(2,3,1,4,5))
plot(x=whiteside$Temp, y=whiteside$Gas, type="n",
     xlab=expression(paste("Temperature (", degree, "C)")), 
     ylab="Gas consumption (cubic feet)")
text(x=whiteside$Temp, y=whiteside$Gas, label=label, col=mdc(4), cex=0.6)



### Section 3.9.4 Converting selection and pattern-mixture models
### Figure 3.9

par(mfrow=c(1,2))
s <- c(
       100, 0.015, 0.058, 0.02, 0.35,
       110, 0.024, 0.074, 0.03, 0.30,
       120, 0.043, 0.103, 0.05, 0.25,
       130, 0.091, 0.164, 0.10, 0.20,
       140, 0.145, 0.185, 0.15, 0.15,
       150, 0.307, 0.247, 0.30, 0.10,
       160, 0.157, 0.099, 0.15, 0.08,
       170, 0.107, 0.049, 0.10, 0.06,
       180, 0.055, 0.016, 0.05, 0.04,
       190, 0.033, 0.005, 0.03, 0.02,
       200, 0.023, 0.000, 0.02, 0.00,
       210, 0.023, 0.000, 0.02, 0.00
       )
sm <- matrix(s, nr=12, nc=5, byrow=TRUE)
snug <- 1.5
xx <- cbind(sm[,1]-5+snug,
        sm[,1]-5-snug,
        sm[,1]-5)
matplot(x=xx[,3], y=cbind(1-sm[,5]),
        col=mdc(1), type="p", lwd=2, lty=1, pch=20,
        xlab="Systolic BP (mmHg)",
        ylab="Observation probability")
matplot(x=xx, y=sm[,2:4], type="s",
        col=c(mdc(4),mdc(5),mdc(6)), lwd=2, lty=1,
        xlab="Systolic BP (mmHg)",
        ylab="Density")



