### fimd1.R
### R code from Van Buuren, S. (2012). 
###		Flexible Imputation of Missing Data. 
###		CRC/Chapman & Hall, Boca Raton, FL.
### (c) 2012 Stef van Buuren, www.multiple-imputation.com
### Version 1, 21mar2012
### Tested with Mac OS X 10.7.3, R2.14-2, mice 2.12

### Chapter 1 	Introduction
### Section 1.1 The problem of missing data

### calculate the mean of three numbers
y <- c(1, 2, 4) 
mean(y)

### calculate the mean of three numbers, where one is missing
y <- c(1, 2, NA)
mean(y)

### repeat, but with any missing data removed
mean(y, na.rm=TRUE)

### store your current options (safety measure)
youroptions <- options()

### demo regression that generates an error message using default na.fail
options(na.action = na.fail)

### uncomment to see the error
### lm(Ozone ~ Wind, data=airquality)

### remove incomplete observations by 'na.action = na.omit'
fit <- lm(Ozone ~ Wind, data = airquality, na.action = na.omit)
coef(fit)

### set automatic na.action = na.omit
options(na.action = na.omit)

### find out how many rows were deleted
deleted <- na.action(fit)
naprint(deleted)

### more incomplete rows if we add Solar.R as predictor
fit2 <- lm(Ozone ~ Wind+Solar.R, data=airquality)
naprint(na.action(fit2))

### restore your original options, but set na.omit
options(youroptions)
options(na.action=na.omit)

### Section 1.3.2 Pairwise deletion

### note: mean(<data.frame>) is deprecated
colMeans(airquality, na.rm=TRUE)
cor(airquality, use="pair")
cov(airquality, use="pair")


### Section 1.3.3 Mean imputation

### install the mice package from CRAN if you have not yet done so
### by uncommenting and running the next line
# install.packages("mice")

library("mice")

### impute the mean
imp <- mice(airquality, method="mean", m=1, maxit=1)


### Figure 1.1
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

### Section 1.3.4 Regression imputation

fit <- lm(Ozone ~ Solar.R, data=airquality)
pred <- predict(fit, newdata=ic(airquality))

### alternative using mice
imp <- mice(airquality[,1:2], method="norm.predict", m=1, maxit=3,seed=1)

### Figure 1.2
par(mfrow = c(1,2))
fmis <- c(hist(pred, breaks, plot=FALSE)$counts, 0)
y <- matrix(c(fobs, fmis), ncol=2)
matplot(x, y, type="s",
        col=c(mdc(4),mdc(5)), lwd=2, lty=1,
        xlim = c(0, 170), ylim = c(0,40), yaxs = "i",
        xlab="Ozone (ppb)",
        ylab="Frequency")
box()

tp <- xyplot(imp, Ozone~Solar.R,
       ylab="Ozone (ppb)", xlab="Solar Radiation (lang)",
       cex = 0.75, lex=lwd,
       ylim = c(-20, 180), xlim = c(0,350))
print(tp, newpage = FALSE, position = c(0.48,0.08,1,0.92))


### Section 1.3.5 Stochastic regression imputation

imp <- mice(airquality[,1:2],method="norm.nob",m=1,maxit=1,seed=1)

### Figure 1.3
par(mfrow = c(1,2))
mis  <- imp$imp$Ozone[,1]
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


### Section 1.3.6 LOCF and BOCF

par(mfrow=c(1,1))
Oz <- airquality$Ozone
locf <- function(x) {
  a <- x[1]
  for (i in 2:length(x)) {
    if (is.na(x[i])) x[i] <- a
    else a <- x[i]
  }
  return(x)
}
Ozi <- locf(Oz)
colvec <- ifelse(is.na(Oz),mdc(2),mdc(1))

### Figure 1.4

plot(Ozi[1:80],col=colvec,type="l",xlab="Day number",ylab="Ozone (ppb)")
points(Ozi[1:80],col=colvec,pch=20,cex=1)


### Section 1.4.3 Example of multiple imputation

imp <- mice(airquality, seed=1, print=FALSE)
fit <- with(imp, lm(Ozone ~ Wind + Temp + Solar.R))
tab <- round(summary(pool(fit)),3)
tab[,c(1:3,5)]

fit <- lm(Ozone~Wind+Temp+Solar.R,data=airquality,na.action=na.omit)
round(coef(summary(fit)),3)

### Figure 1.6
par(mfrow = c(1,2))
mis  <- imp$imp$Ozone[,1]
fmis <- c(hist(mis, breaks, plot=FALSE)$counts, 0)
y <- matrix(c(fobs, fmis), ncol=2)
matplot(x, y, type="s",
        col=c(mdc(4),mdc(5)), lwd=2, lty=1,
        xlim = c(0, 170), ylim = c(0,40), yaxs = "i",
        xlab="Ozone (ppb)",
        ylab="Frequency")
box()

tp <- xyplot(imp, Ozone~Solar.R, subset = .imp==1,
       ylab="Ozone (ppb)", xlab="Solar Radiation (lang)",
       cex = 0.75, lex=lwd,
       ylim = c(-20, 180), xlim = c(0,350))
print(tp, newpage = FALSE, position = c(0.48,0.08,1,0.92))

### scatterplot of all imputed data sets (not in the book)
xyplot(imp, Ozone~Solar.R | .imp,
       ylab="Ozone (ppb)", xlab="Solar Radiation (lang)",
       cex = 0.75, lex=lwd,
       ylim = c(-20, 180), xlim = c(0,350))

### Figure 1.7

Oz <- airquality$Ozone
colvec <- ifelse(is.na(Oz),mdc(2),mdc(1))
par(mfrow=c(1,1))
plot(Oz[1:80],col=mdc(1),type="l",xlab="Day number",ylab="Ozone (ppb)")
points(Oz[1:80],col=mdc(1),pch=20,cex=1)

idx <- ici(airquality$Ozone) & (1:153)<81 
x   <- (1:153)[idx]
points(x=x,y=complete(imp,1)$Ozone[idx],col=mdc(2),pch=20,cex=1)
points(x=x,y=complete(imp,2)$Ozone[idx],col=mdc(2),pch=20,cex=1)
points(x=x,y=complete(imp,3)$Ozone[idx],col=mdc(2),pch=20,cex=1)
points(x=x,y=complete(imp,4)$Ozone[idx],col=mdc(2),pch=20,cex=1)
points(x=x,y=complete(imp,5)$Ozone[idx],col=mdc(2),pch=20,cex=1)

### figure of the autocorrelation function
### (not in the book)

par(mfrow=c(2,5))
acf.ozone <- with(imp,acf(Ozone))
model <- expression(acf(resid(lm(Ozone ~ Wind + Temp + Solar.R))))
acf.resid <- with(imp,model)
calcacf <- function(acf.list) {
  k <- length(acf.list)
  acc <- acf.list[[1]]$acf
  for (i in 2:k) acc <- acc + acf.list[[i]]$acf
  return(acc/k)
 }
oz <- round(calcacf(acf.ozone$analyses),2)
re <- round(calcacf(acf.resid$analyses),2)


