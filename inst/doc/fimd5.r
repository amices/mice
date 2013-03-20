### fimd5.R
### R code from Van Buuren, S. (2012). 
###		Flexible Imputation of Missing Data. 
###		CRC/Chapman & Hall, Boca Raton, FL.
### (c) 2012 Stef van Buuren, www.multiple-imputation.com
### Version 1, 21mar2012
### Tested with Mac OS X 10.7.3, R2.14-2, mice 2.12

if (packageVersion("mice")<'2.12') stop("This code requires mice 2.12.")

library("mice")
library("gamlss")

### set Trellis layout parameters
lhset <- trellis.par.get("layout.heights")
lhset$top.padding <- 0
lhset$bottom.padding <- 0
trellis.par.set("layout.heights", lhset)
lwset <- trellis.par.get("layout.widths")
lwset$left.padding <- 0
lwset$right.padding <- 0
trellis.par.set("layout.widths", lwset)


### Section 5.3.2 Predictors

imp <- mice(nhanes, print=FALSE)
imp$predictorMatrix
imp <- mice(cbind(nhanes, chl2=2*nhanes$chl), print=FALSE)
imp$loggedEvents


### Section 5.4.1 Ratio of two variables

imp1 <- mice(boys)
long <- complete(imp1, "long", inc=TRUE)
long$whr <- with(long, wgt/(hgt/100))
# imp2 <- long2mids(long)

### Just anothor varianble (JAV)
boys$whr <- boys$wgt/(boys$hgt/100)
imp.jav <- mice(boys, m=1, seed=32093, maxit=10)
imp.jav$loggedEvents

### Passive imputation
ini <- mice(boys, m=1, maxit=0)
meth <- ini$meth
meth["whr"] <- "~I(wgt/(hgt/100))"
meth["bmi"] <- "~I(wgt/(hgt/100)^2)"
pred <- ini$pred
pred[c("wgt","hgt","bmi"),"whr"] <- 0
pred[c("wgt","hgt","whr"),"bmi"] <- 0
pred
imp.pas <- mice(boys, m=1, meth=meth, pred=pred, seed=32093, maxit=10)

### passive imputation 2 
pred[c("wgt","hgt","hc","reg"),"bmi"] <- 0
pred[c("gen","phb","tv"),c("hgt","wgt","hc")] <- 0
pred[,"whr"] <- 0
imp.pas2 <- mice(boys, m=1, meth=meth, pred=pred, seed=32093, maxit=10)

### Figure 5.1
c1 <- cbind(model="JAV",complete(imp.jav))
c2 <- cbind(model="passive",complete(imp.pas))
c3 <- cbind(model="passive 2",complete(imp.pas2))
cd <- rbind(c1,c2, c3)
trellis.par.set(mice.theme())
tp51 <- xyplot(whr~hgt|model,data=cd,layout=c(3,1),
       groups=rep(is.na(imp.jav$data$whr),3), pch=c(1,20), cex=c(0.4,1),
       xlab="Height (cm)", ylab="Weight/Height (kg/m)")
print(tp51)

### Section 5.4.3 Interaction terms
rm(boys)
expr <- expression((wgt-40)*(hc-50))
boys$wgt.hc <- with(boys, eval(expr))
ini <- mice(boys, max=0)
meth <- ini$meth
meth["wgt.hc"] <- paste("~I(",expr,")",sep="")
meth["bmi"] <- ""
pred <- ini$pred
pred[c("wgt","hc"),"wgt.hc"] <- 0
imp.int <- mice(boys, m=1, maxit=10, meth=meth, pred=pred, seed=62587)

### Figure 5.2
miss <- is.na(imp.int$data$wgt.hc)
tp52a <- xyplot(imp.int, wgt~wgt.hc, na.groups = miss,
       cex=c(0.8,1.2), pch=c(1,20),
       ylab="Weight (kg)", xlab="Interaction")
tp52b <- xyplot(imp.int, hc~wgt.hc, na.groups = miss,
       cex=c(0.8,1.2), pch=c(1,20),
       ylab="Head circumference (cm)", xlab="Interaction")
print(tp52a)
print(tp52b)

### Section 5.4.4 Conditional imputation

ini <- mice(airquality[,1:2],maxit=0)
post <- ini$post
post["Ozone"] <- "imp[[j]][,i] <- squeeze(imp[[j]][,i],c(1,200))"
### Note: ifdo() not yet implemented
### so for the moment use old form
## post["Ozone"] <- "ifdo(c(Ozone<1, Ozone>200), c(1, 200))"
imp <- mice(airquality[,1:2],method="norm.nob",m=1,maxit=1,seed=1,post=post)

### Figure 5.3

lwd <- 1.5
par(mfrow=c(1,2))
breaks <- seq(-20, 200, 10)
nudge <- 1
x <- matrix(c(breaks-nudge, breaks+nudge), ncol=2)
obs <- airquality[,"Ozone"]
mis  <- imp$imp$Ozone[,1]
fobs <- c(hist(obs, breaks, plot=FALSE)$counts, 0)
fmis <- c(hist(mis, breaks, plot=FALSE)$counts, 0)
y <- matrix(c(fobs, fmis), ncol=2)
matplot(x, y, type="s",
        col=c(mdc(4),mdc(5)), lwd=2, lty=1,
        xlim = c(0, 170), ylim = c(0,40), yaxs = "i",
        xlab="Ozone (ppb)",
        ylab="Frequency")
box()

tp <- xyplot(imp, Ozone~Solar.R, na.groups=ici(imp),
       ylab="Ozone (ppb)", xlab="Solar Radiation (lang)",
       cex = 0.75, lex=lwd,
       ylim = c(-20, 180), xlim = c(0,350))
print(tp, newpage = FALSE, position = c(0.48,0.08,1,0.92))



### Post-processing on the boys data

post <- mice(boys, m=1, maxit=0)$post
post["gen"] <- "imp[[j]][p$data$age[!r[,j]]<8,i] <- levels(boys$gen)[1]"
post["phb"] <- "imp[[j]][p$data$age[!r[,j]]<8,i] <- levels(boys$phb)[1]"
post["tv"]  <- "imp[[j]][p$data$age[!r[,j]]<8,i] <- 1"
free <- mice(boys, m=1, seed=85444, print=FALSE)
restricted <- mice(boys, m=1, post=post, seed=85444, print=FALSE)

### The following code using ifdo does not yet work
### Use the form given above
## post <- mice(boys, m=1, maxit=0)$post
## post["gen"] <- "ifdo(age<8, levels(gen)[1])"
## post["phb"] <- "ifdo(age<8, levels(phb)[1])"
## post["tv"] <- "ifdo(age<8, 1)"
## free <- mice(boys, m=1, seed=85444)
## restricted <- mice(boys, m=1, post=post, seed=85444)


### Figure 5.4

imp3 <- rbind(free, restricted)  # ignore warning
model <- rep(c("Free","Restricted"),each=nrow(boys))
tp54 <- xyplot(imp3, gen~age|model, pch=c(3,1), cex=c(3,1.5),
            ylab="Genital development", xlab="Age (years)")
print(tp54)

### Section 5.4.5 Compositional data

set.seed(43112)
n <- 400
Y1 <- sample(1:10, size=n, replace=TRUE)
Y2 <- sample(1:20, size=n, replace=TRUE)
Y3 <- 10 + 2 * Y1 + 0.6 * Y2 + sample(-10:10, size=n, replace=TRUE)
Y <- data.frame(Y1, Y2, Y3)
Y[1:100, 1:2] <- NA
md.pattern(Y)

Y123 <- Y1+Y2+Y3
Y12 <- Y123-Y[,3]
P1 <- Y[,1]/Y12
data <- data.frame(Y, Y123, Y12, P1)

ini <- mice(data, maxit=0, m=10, print=FALSE, seed=21772)
meth <- ini$meth
meth["Y1"] <- "~I(P1*Y12)"
meth["Y2"] <- "~I((1-P1)*Y12)"
meth["Y12"] <- "~I(Y123-Y3)"
pred <- ini$pred
pred["P1",] <- 0
pred[c("P1"),c("Y12","Y3")] <- 1
imp1 <- mice(data, meth=meth, pred=pred, m=10, print=FALSE)

round(summary(pool(with(imp1, lm(Y3~Y1+Y2))))[,1:2],2)

### improved solution for Figure 5.5b
ini <- mice(data, maxit=0, m=10, print=FALSE, seed=21772)
meth <- ini$meth
meth["Y1"] <- "~I(P1*Y12)"
meth["Y2"] <- "~I((1-P1)*Y12)"
meth["Y12"] <- "~I(Y123-Y3)"
pred <- ini$pred
pred["P1",] <- 0
pred[c("P1"),c("Y12","Y3")] <- 1
imp1 <- mice(data, meth=meth, pred=pred, m=1, print=FALSE)
pred["P1","Y3"] <- 0
imp2 <- mice(data, meth=meth, pred=pred, m=1, print=FALSE)

### Figure 5.5
imp <- rbind(imp1, imp2)  # ignore warning
model <- rep(c("Y12 and Y3","Y12 only"),each=n)
tp55 <- xyplot(imp, P1~Y12|model, pch=c(1,19), xlab="Y1 + Y2")
print(tp55)

### Section 5.5.1 Visit sequence

### Continue from the imp.int object calculated in section 5.4.3 
imp.int$vis
vis <- c(2,3,5,10,6:9)
rm(boys)  # refresh boys data
expr <- expression((wgt-40)*(hc-50))
boys$wgt.hc <- with(boys, eval(expr))
imp.int2 <- mice(boys, m=1, max=1, vis=vis, meth=imp.int$meth, pred=imp.int$pred, seed=23390)

### monotone sequence
imp.int2 <- mice(boys, m=1, max=1, vis="monotone", meth=imp.int$meth, pred=imp.int$pred, seed=23390)

### Section 5.5.2 Convergence

### Figure 5.6 Default color version

imp <- mice(nhanes, seed=62006, maxit=20, print=FALSE)
tp56 <- plot(imp)
print(tp56)

### Figure 5.6 Book version

tp56b <- plot(imp, col=mdc(5), lty=1:5)
print(tp56b)

### Example of pathological convergence
rm(boys)
ini <- mice(boys,max=0,print=FALSE)
meth <- ini$meth
meth["bmi"] <- "~I(wgt/(hgt/100)^2)"
imp.bmi1 <- mice(boys, meth=meth, maxit=20, seed=60109)

### Figure 5.7
### Does not reproduce exactly, but the message is the same
tp57 <- plot(imp.bmi1,c("hgt","wgt","bmi"), col=mdc(5), lty=1:5)
print(tp57)

### Breaking cyclic imputations for hgt and wgt
pred <- ini$pred
pred[c("hgt","wgt"),"bmi"] <- 0
imp.bmi2 <- mice(boys, meth=meth, pred=pred, maxit=20, seed=60109)

### Figure 5.8
### Does not reproduce exactly, but the message is the same
tp58 <- plot(imp.bmi2,c("hgt","wgt","bmi"), col=mdc(5), lty=1:5)
print(tp58)

### Section 5.6.2 Diagnostic graphs

set.seed(24417)
### create 50% missing data in wgt
boys$wgt[sample(1:nrow(boys),nrow(boys)/2)] <- NA
meth <- c("","pmm","pmm","","pmm","polr","polr","pmm","polyreg")
imp <- mice(boys, m=1, meth=meth, seed=53882, print=FALSE)

### Simple scatterplot wgt-age (not given in book)
tp58b <- xyplot(imp, wgt~age|.imp)
print(tp58b)

### Fit separate regression models for wgt
### for observed (n=372) and imputed (n=376) data
cd <- complete(imp)[,-4]
isobs <- !is.na(boys$wgt)
cdobs <- cd[isobs,]
cdmis <- cd[!isobs,]
obs <- gamlss(wgt~age+hgt+hc+gen+phb+tv+reg, data=cdobs)
mis <- gamlss(wgt~age+hgt+hc+gen+phb+tv+reg, data=cdmis)

### Figure 5.9 worm plot
wp.twin(obs, mis, xvar=NULL, xvar.column=2, n.inter=9, col1=mdc(4), col2=mdc(5), ylim=0.9, cex=1, pch=1)

### Figure 5.10
imp <- mice(nhanes, seed=29981)
tp510 <- stripplot(imp, pch=c(1,20))
print(tp510)

### Figure 5.11
tp511 <- densityplot(imp)
print(tp511)

### Calculate propensity scores
fit <- with(imp, glm(ici(imp)~age+bmi+hyp+chl,family=binomial))
ps <- rep(rowMeans(sapply(fit$analyses, fitted.values)),imp$m)

### Figure 5.12
tp512 <- xyplot(imp, bmi~ps|.imp, pch=c(1,19), 
       xlab="Probability that record is incomplete", 
       ylab="BMI")
print(tp512)


