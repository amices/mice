## This file contains the R code used in  

## Van Buuren, S., Groothuis-Oudshoorn, K. (2011) 
## mice: Multivariate Imputation by Chained Equations in R. 
## Journal of Statistical Software.

## Compiles with R 2.13.0
## Assumes that subdirectory "figures" is part of working directory 
## Requires R packages: mice (>2.9), VIM, mitools, Zelig (+dependencies)

## SvB 31AUG2011
### Version 2, 4nov2015 tested with mice 2.23


###################################################
### chunk number 1: init1
###################################################
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
if (as.numeric(packageDescription("mice")$Version)<2.9) warning("mice 2.9 or higher needed")

###
###   2.4 Simple example
###
 
###################################################
### chunk number 4: example1
###################################################
library("mice")
library("lattice")


###################################################
### chunk number 5: example2 eval=FALSE
###################################################
nhanes


###################################################
### chunk number 6: example3
###################################################
md.pattern(nhanes)


###################################################
### chunk number 7: example4
###################################################
p <- md.pairs(nhanes)
p


###################################################
### chunk number 9: marginplot
###################################################
library("VIM")
marginplot(nhanes[,c("chl","bmi")], col=mdc(1:2), cex=1.2, cex.lab=1.2, cex.numbers=1.3, pch=19)


###################################################
### chunk number 10: example1
###################################################
imp <- mice(nhanes, seed=23109)


###################################################
### chunk number 11: example2
###################################################
print(imp)


###################################################
### chunk number 12: example3
###################################################
imp$imp$bmi


###################################################
### chunk number 13: example4 eval=FALSE
###################################################
complete(imp)


###################################################
### chunk number 14: stripplotcmd eval=FALSE
###################################################
stripplot(imp, pch=20, cex=1.2)


###################################################
### chunk number 16: xyplotcmd eval=FALSE
###################################################
xyplot(imp, bmi~chl|.imp, pch=20, cex=1.4)


###################################################
### chunk number 18: example6
###################################################
fit <- with(imp, lm(chl~age+bmi))


###################################################
### chunk number 19: example7
###################################################
print(pool(fit))


###################################################
### chunk number 20: example8
###################################################
round(summary(pool(fit)),2)


###################################################
### chunk number 21: example9
###################################################
imp50 <- mice(nhanes, m=50, seed=23109)
fit <- with(imp50, lm(chl~age+bmi))


###################################################
### chunk number 22: example10
###################################################
round(summary(pool(fit)),2)

###
###  3.2 Univariate imputation methods
###

###################################################
### chunk number 23: models1 eval=FALSE
###################################################
imp <- mice(nhanes, method = "norm")


###################################################
### chunk number 24: models2 eval=FALSE
###################################################
imp <- mice(nhanes, meth = c("", "norm", "pmm", "mean"))


###################################################
### chunk number 25: models3
###################################################
str(nhanes2)


###################################################
### chunk number 26: models4 eval=FALSE
###################################################
imp <- mice(nhanes2, me=c("polyreg","pmm","logreg","norm"))


###################################################
### chunk number 27: models5 eval=FALSE
###################################################
imp <- mice(nhanes2, meth=c("","","logreg","norm"))


###################################################
### chunk number 28: models6 eval=FALSE
###################################################
mice(nhanes2, defaultMethod = c("norm","logreg","polyreg","polr"))

###
###   3.3 Predictor selection
###

###################################################
### chunk number 29: predictor1
###################################################
imp <- mice(nhanes, print=FALSE)
imp$predictorMatrix


### removing a predictor
###################################################
### chunk number 30: predictor2
###################################################
pred <- imp$predictorMatrix
pred[,"bmi"] <- 0
pred


###################################################
### chunk number 31: predictor3
###################################################
imp <- mice(nhanes, pred=pred, pri=FALSE)


### skipping imputation
###################################################
### chunk number 32: predictor4
###################################################
ini <- mice(nhanes2, maxit=0, pri=FALSE)
pred <- ini$pred
pred[,"bmi"] <- 0
meth <- ini$meth
meth["bmi"] <- ""
imp <- mice(nhanes2, meth=meth, pred=pred, pri=FALSE)
imp$imp$bmi


### intercept imputation
###################################################
### chunk number 33: predictor5
###################################################
pred <- ini$pred
pred["bmi",] <- 0
imp <- mice(nhanes2, pred=pred, pri=FALSE, seed=51162)
imp$imp$bmi

### multilevel imputation
###################################################
### chunk number 34: multilevel1
###################################################
popmis[1:3,]
ini <- mice(popmis, maxit=0)
pred <- ini$pred
pred["popular",] <- c(0, -2, 0, 2, 1, 2, 0)
imp <- mice(popmis, meth=c("","","2l.norm","","","",""), pred=pred, maxit=1, seed=71152)

###
###   quick predictor selection
###

###################################################
### chunk number 35: quickpred1
###################################################
round(cor(nhanes, use="pair"),3)


###################################################
### chunk number 36: quickpred2
###################################################
round(cor(y=nhanes, x=!is.na(nhanes), use="pair"),3)


###################################################
### chunk number 37: quickpred3
###################################################
p <- md.pairs(nhanes)
round(p$mr/(p$mr+p$mm), 3)


###################################################
### chunk number 38: quickpred4
###################################################
quickpred(nhanes)


###################################################
### chunk number 39: quickpred5 eval=FALSE
###################################################
imp <- mice(nhanes, pred=quickpred(nhanes, minpuc=0.25, include="age"))

###
###   3.4 Passive imputation
###

### Preserving a transformation
###################################################
### chunk number 40: transform1
###################################################
nhanes2.ext <- cbind(nhanes2, lchl=log(nhanes2$chl))
ini <- mice(nhanes2.ext, max=0, print=FALSE)
meth <- ini$meth
meth["lchl"] <- "~log(chl)"
pred <- ini$pred
pred[c("hyp","chl"),"lchl"] <- 0
pred["bmi","chl"] <- 0
pred
imp <- mice(nhanes2.ext, meth=meth, pred=pred, seed=38788, print=FALSE)
head(complete(imp))


### index of two variables
###################################################
### chunk number 41: transform2
###################################################
nhanes2.ext <- cbind(nhanes2, lchl=NA)


###################################################
### chunk number 42: index1
###################################################
md.pattern(boys[,c("hgt","wgt","bmi")])


###################################################
### chunk number 43: index2
###################################################
ini <- mice(boys,max=0,print=FALSE)
meth <- ini$meth
meth["bmi"] <- "~I(wgt/(hgt/100)^2)"
pred <- ini$pred
pred[c("wgt","hgt","hc","reg"),"bmi"] <- 0
pred[c("gen","phb","tv"),c("hgt","wgt","hc")] <- 0
pred


###################################################
### chunk number 44: index3
###################################################
imp.idx <- mice(boys, pred=pred, meth=meth, maxit=20, seed=09212, print=FALSE)
head(complete(imp.idx)[is.na(boys$bmi),],3)


###################################################
### chunk number 45: index4
###################################################
meth["bmi"] <- "~round(wgt/(hgt/100)^2,dig=2)"


### sum scores
###################################################
### chunk number 46: sumscore1
###################################################
ini <- mice(cbind(boys,mat=NA),max=0,print=FALSE)
meth <- ini$meth
meth["mat"] <- "~I(as.integer(gen) + as.integer(phb) +
 + as.integer(cut(tv,breaks=c(0,3,6,10,15,20,25))))"
meth["bmi"] <- "~I(wgt/(hgt/100)^2)"
pred <- ini$pred
pred[c("bmi","gen","phb","tv"),"mat"] <- 0
pred[c("hgt","wgt","hc","reg"),"mat"] <- 1
pred[c("hgt","wgt","hc","reg"),c("gen","phb","tv")] <- 0
pred[c("wgt","hgt","hc","reg"),"bmi"] <- 0
pred[c("gen","phb","tv"),c("hgt","wgt","hc")] <- 0
pred


###################################################
### chunk number 47: sumscore2
###################################################
#line 705 "/Users/stefvanbuuren/Documents/Sync/Impute/JSS/ArtikelV5/mice.Rnw"
imp.sum <- mice(cbind(boys,mat=NA), pred=pred, meth=meth, maxit=20, seed=10948, print=FALSE)


###################################################
### chunk number 48: sumscore3 eval=FALSE
###################################################
xyplot(imp.sum, mat~age|.imp, na=gen|phb|tv, subset=.imp==1,
        ylab="Maturation score", xlab="Age (years)")


### interactions continuous variables
###################################################
### chunk number 50: interaction1
###################################################
nhanes2.ext <- cbind(nhanes2, bmi.chl=NA)
ini <- mice(nhanes2.ext, max=0, print=FALSE)
meth <- ini$meth
meth["bmi.chl"] <- "~I((bmi-25)*(chl-200))"
pred <- ini$pred
pred[c("bmi","chl"),"bmi.chl"] <- 0
imp <- mice(nhanes2.ext, meth=meth, pred=pred, seed=51600, print=FALSE)


###################################################
### chunk number 51: interaction2
###################################################
head(ini$pad$data,3)


### interaction categorical variables
###################################################
### chunk number 52: interaction3 eval=FALSE
###################################################
nhanes2.ext <- cbind(nhanes2,age.1.bmi=NA,age.2.bmi=NA)
ini <- mice(nhanes2.ext, max=0, print=FALSE)
meth <- ini$meth
meth["age.1.bmi"] <- "~I(age.1*(bmi-25))"
meth["age.2.bmi"] <- "~I(age.2*(bmi-25))"
pred <- ini$pred
pred[c("age","bmi"),c("age.1.bmi","age.2.bmi")] <- 0
imp <- mice(nhanes2.ext, meth=meth, pred=pred, maxit=10)


### squeeze
###################################################
### chunk number 53: squeeze1 eval=FALSE
###################################################
nhanes2.ext <- cbind(nhanes2, lchl=NA)
ini <- mice(nhanes2.ext,max=0,pri=FALSE)
meth <- ini$meth
meth[c("lchl","chl")] <- c("~log(chl)","norm")
pred <- ini$pred
pred[c("hyp","chl"),"lchl"] <- 0
pred["bmi","chl"] <- 0
imp <- mice(nhanes2.ext, meth=meth, pred=pred, seed=1, maxit=100)


###################################################
### chunk number 54: squeeze2 eval=FALSE
###################################################
meth["lchl"] <- "~log(squeeze(chl, bounds=c(100,300)))"
imp <- mice(nhanes2.ext, meth=meth, pred=pred, seed=1, maxit=100)


### post-processing: prevent negative chl
###################################################
### chunk number 55: post1
###################################################
nhanes2.ext <- cbind(nhanes2, lchl=NA)
ini <- mice(nhanes2.ext,max=0,print=FALSE)
meth <- ini$meth
meth[c("lchl","chl")] <- c("~log(chl)","norm")
pred <- ini$pred
pred[c("hyp","chl"),"lchl"] <- 0
pred["bmi","chl"] <- 0
post <- ini$post
post["chl"] <- "imp[[j]][,i] <- squeeze(imp[[j]][,i],c(100,300))"
imp <- mice(nhanes2.ext, meth=meth, pred=pred, post=post, 
            seed=30031, maxit=10, print=FALSE)
imp$imp$chl

### post-processing: constrain maturation scores below age 5 yrs
###################################################
### chunk number 56: post2
###################################################
ini <- mice(cbind(boys,mat=NA),max=0,print=FALSE)
meth <- ini$meth
meth["mat"] <- "~I(as.integer(gen) + as.integer(phb) +
 + as.integer(cut(tv,breaks=c(0,3,6,10,15,20,25))))"
meth["bmi"] <- "~I(wgt/(hgt/100)^2)"
pred <- ini$pred
pred[c("bmi","gen","phb","tv"),"mat"] <- 0
pred[c("hgt","wgt","hc","reg"),"mat"] <- 1
pred[c("hgt","wgt","hc","reg"),c("gen","phb","tv")] <- 0
pred[c("wgt","hgt","hc","reg"),"bmi"] <- 0
pred[c("gen","phb","tv"),c("hgt","wgt","hc")] <- 0
pred


###################################################
### chunk number 57: post3
###################################################
post <- ini$post
post["gen"] <- "imp[[j]][p$data$age[!r[,j]]<5,i] <- levels(boys$gen)[1]"
post["phb"] <- "imp[[j]][p$data$age[!r[,j]]<5,i] <- levels(boys$phb)[1]"
post["tv"]  <- "imp[[j]][p$data$age[!r[,j]]<5,i] <- 1"
imp <- mice(cbind(boys,mat=NA), pred=pred, meth=meth, post=post, maxit=10, print=FALSE)


###################################################
### chunk number 58: post4 eval=FALSE
###################################################
xyplot(imp,mat~age|.imp, na=gen|phb|tv, subset=.imp==1,ylab="Maturation score", xlab="Age (years)")


###
###   3.6 Visiting scheme
###

###################################################
### chunk number 60: visit1
###################################################
nhanes2.ext <- cbind(nhanes2, bmi.chl=NA)
ini <- mice(nhanes2.ext, max=0, print=FALSE)
meth <- ini$meth
meth["bmi.chl"] <- "~I((bmi-25)*(chl-200))"
pred <- ini$pred
pred[c("bmi","chl"),"bmi.chl"] <- 0
imp <- mice(nhanes2.ext, meth=meth, pred=pred, seed=51600, print=FALSE)


###################################################
### chunk number 61: visit2
###################################################
imp$vis


###################################################
### chunk number 62: visit3 eval=FALSE
###################################################
vis<-imp$vis
vis<-append(vis,vis[4],1)
vis
imp <- mice(nhanes2.ext, meth=meth, pred=pred, vis=vis)


###################################################
### chunk number 63: visit4 eval=FALSE
###################################################
imp <- mice(nhanes2.ext, meth=meth, pred=pred, vis=c(2,4,5,3))


###################################################
### chunk number 64: visit5 eval=FALSE
###################################################
imp <- mice(nhanes2.ext, meth=meth, pred=pred, vis="monotone")


###
###   4.1 Dry run
###

###################################################
### chunk number 65: dryrun1
###################################################
ini <- mice(nhanes2, maxit=0)


###################################################
### chunk number 66: dryrun2
###################################################
import <- matrix(c(30,30,30,29,25,21,25,25,22,33,27,22,27,35,27,20,27,30), byrow=TRUE,nr=9)
imp <- mice(nhanes, print=FALSE, seed=77172)
imp$imp$bmi[,1:2] <- import
imp$imp$bmi


###
###   4.2 Step by Step
###

###################################################
### chunk number 67: step1
###################################################
imp <- mice(nhanes, maxit=4, seed=44612, print=FALSE)
imp1 <- mice(nhanes, maxit=1, seed=44612, print=FALSE)
a <- runif(10)
imp2 <- mice.mids(imp1, maxit=3, print=FALSE)


###################################################
### chunk number 68: step2
###################################################
all(imp$imp$bmi==imp2$imp$bmi)


###
###   4.3 Assessing convergence
###

### first pathological example
###################################################
### chunk number 69: convergence1
###################################################
ini <- mice(boys,max=0,print=FALSE)
meth <- ini$meth
meth["bmi"] <- "~I(wgt/(hgt/100)^2)"
meth["wgt"] <- "~I(bmi*(hgt/100)^2)"
meth["hgt"] <- "~I(100*sqrt(wgt/bmi))"
imp1 <- mice(boys, meth=meth, maxit=20, print=FALSE, seed=09212)


###################################################
### chunk number 70: convergence1plotcmd eval=FALSE
###################################################
plot(imp1, c("hgt","wgt","bmi"))


### second pathological example
###################################################
### chunk number 72: convergence2
###################################################
ini <- mice(boys,max=0,print=FALSE)
meth <- ini$meth
meth["bmi"] <- "~I(wgt/(hgt/100)^2)"
imp2 <- mice(boys, meth=meth, maxit=20, print=FALSE, seed=09212)


###################################################
### chunk number 73: convergence2plotcmd eval=FALSE
###################################################
plot(imp2, c("hgt","wgt","bmi"))


# solution from section 3.3, sum scores - healthy convergenc
###################################################
### chunk number 75: convergence3plotcmd eval=FALSE
###################################################
plot(imp.idx, c("hgt","wgt","bmi"))



###################################################
### chunk number 77: convergence4plotcmd eval=FALSE
###################################################
plot(imp.idx, c("hc","gen","phb"), tick.number=3)


### monitor Kendall's tau between gen and phb
###################################################
### chunk number 79: convergence5
###################################################
m <- 5
T <- 20
imp.kendall <- mice(boys, m=m, meth=imp.idx$meth, pred=imp.idx$pred, maxit=0, print=FALSE)
tau <- matrix(NA,nrow=T,ncol=m)
for (i in 1:T) {
  if(i==1) set.seed(09212)
  imp.kendall <- mice.mids(imp.kendall, maxit=1, print=FALSE)
  x <- complete(imp.kendall,"repeated")[,paste("gen",1:m,sep=".")]
  y <- complete(imp.kendall,"repeated")[,paste("phb",1:m,sep=".")]
  xn <- as.data.frame(lapply(x,as.numeric))
  yn <- as.data.frame(lapply(y,as.numeric))
  tau[i,] <- diag(cor(xn,yn,method="kendall"))
}


###################################################
### chunk number 80: convergence5plotcmd
###################################################
matplot(x=1:T,y=tau,xlab="Iteration",type="l")


###################################################
### chunk number 81: convergence5plot
###################################################
matplot(x=1:T,y=tau,xlab="Iteration",type="l")

###
###   4.5 Checking your imputations
### 

### density plot
###################################################
### chunk number 82: densityplotcmd eval=FALSE
###################################################
densityplot(imp.kendall, scales=list(x=list(relation="free")), layout=c(5,1))


### conditional propensity score plot
###################################################
### chunk number 84: propensity
###################################################
hc.na <- is.na(boys$hc)
fit.hc <- with(imp.kendall, glm(hc.na~age+wgt+hgt+reg,family=binomial))
ps <- rep(rowMeans(sapply(fit.hc$analyses, fitted.values)),6)


###################################################
### chunk number 85: propensityplotcmd eval=FALSE
###################################################
xyplot(imp.kendall, hc~ps|.imp, pch=c(1,20), cex=c(0.8,1.2), 
         xlab="Probability that head circumference is missing", 
         ylab="Head circumference (cm)",scales=list(tick.number=3))


### calculate model and residuals
###################################################
### chunk number 87: residuals1
###################################################
hc <- complete(imp.kendall,"long",TRUE)$hc
fit <- lm(hc ~ poly(ps,4))


###################################################
### chunk number 88: residuals2 eval=FALSE
###################################################
densityplot(~ residuals(fit), group = hc.na, plot.points = FALSE, ref = TRUE, scales = list(y=list(draw=FALSE)), par.settings = simpleTheme(col.line = rep(mdc(1:2))), xlab = "Residuals of regression of hc on propensity score", lwd=2)


###
###   5.1 Repeated data analysis
###

###################################################
### chunk number 90: repeated1
###################################################
imp <- mice(nhanes2, seed=99210, print=FALSE)
fit <- with(imp, lm(chl~age+bmi))
summary(pool(fit))


###################################################
### chunk number 91: repeated2
###################################################
expr <- expression(ov<-cut(bmi,c(10,25,50)),table(age,ov))
fit <- with(imp, eval(expr))


###################################################
### chunk number 92: repeated3
###################################################
fit$an[c(2,5)]


###################################################
### chunk number 93: extract1
###################################################
com <- complete(imp, 3)


###################################################
### chunk number 94: extract2
###################################################
com <- complete(imp, "long")


###################################################
### chunk number 95: extract3
###################################################
com <- complete(imp, "long", include=TRUE)
by(cbind(age=com$age,ov=cut(com$bmi,c(10,25,50))), com$.imp, table)


###
###   6.1 Adding your own imputation functions
###

###################################################
### chunk number 96: pool1
###################################################
fit <- with(imp, lm(chl~age+bmi))
est <- pool(fit)


###################################################
### chunk number 97: pool2
###################################################
methods(coef)
methods(vcov)

### Model testing - Wald method
###################################################
### chunk number 98: pool3
###################################################
imp <- mice(nhanes2, print=FALSE, m=50, seed=00219)
fit0 <- with(data=imp,expr=lm(bmi~age+hyp))
fit1 <- with(data=imp,expr=lm(bmi~age+hyp+chl))
stat <- pool.compare(fit1, fit0, method="Wald")
stat$p


### Model testing - LLR method
###################################################
### chunk number 99: pool4
###################################################
imp <- mice(boys, print=FALSE, seed=60019)
fit0 <- with(data=imp, expr=glm(I(gen>levels(gen)[1])~hgt+hc, family=binomial))
fit1 <- with(data=imp, expr=glm(I(gen>levels(gen)[1])~hgt+hc+reg, family=binomial))
stat <- pool.compare(fit1, fit0, method="likelihood", data=imp)
stat$p



###################################################
### chunk number 100: ownfunction eval=FALSE
###################################################
mice(nhanes, method="myfunc")

###
###   6.2 Sensitivity analysis under MNAR
###

###################################################
### chunk number 101: sensitivity
###################################################
ini <- mice(nhanes2,maxit=0,print=FALSE)
post <- ini$post
k <- seq(1,1.5,0.1)
est <- vector("list",length(k))
for (i in 1:length(k)) {
   post["chl"] <- paste("imp[[j]][,i] <-",k[i],"* imp[[j]][,i]")
   imp <- mice(nhanes2, post=post, seed=00010, print=FALSE, maxit=20)
   fit <- with(imp, lm(bmi~age+chl))
   est[[i]] <- summary(pool(fit))
 }


###
###   7.2 SPSS
###

# The code below should not be run from R, but from SPSS.
# Remove one (1) # sign per line, and run.
# 
# BEGIN PROGRAM R.
# Instruct R to import the data and the dictionary from SPSS.
#dict <- spssdictionary.GetDictionaryFromSPSS()
#data <- spssdata.GetDataFromSPSS()
## Load mice, impute and get the original + imputed data.
#library(mice)
#imp <- mice(data,maxit=10)
#com <- complete(imp,"long",inc=TRUE)
#com <- cbind(com, Imputation_ = as.integer(com$.imp)-1)
## Export imputed data and dictionary from R to SPSS.
#spssdictionary.SetDictionaryToSPSS("com", dict)
#spssdata.SetDataToSPSS("com", com)
#spssdictionary.EndDataStep()
#END PROGRAM.

###
###   7.3 MItools
###

###################################################
### chunk number 102: mitools1
###################################################
library("mitools")
mydata <- imputationList(lapply(1:5, complete, x=imp))


###################################################
### chunk number 103: mitools2 eval=FALSE
###################################################
fit <- with(mydata, expr=lm(chl~age + bmi))

###
###   7.3 Zelig
###

###################################################
### chunk number 104: zelig1
###################################################
library("Zelig")
imp <- cbind.mids(imp.idx,data.frame(r.hc=is.na(boys$hc)))
mydata2 <- mi(complete(imp,1), complete(imp,2), complete(imp,3), complete(imp,4),complete(imp,5))
fit <- zelig(r.hc~age+wgt+hgt+bmi+gen+phb+tv+reg, model="logit", data=mydata2)
summary(fit)


