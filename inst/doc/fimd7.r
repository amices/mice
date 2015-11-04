### fimd7.R
### R code from Van Buuren, S. (2012). 
###		Flexible Imputation of Missing Data. 
###		CRC/Chapman & Hall, Boca Raton, FL.
### (c) 2012 Stef van Buuren, www.multiple-imputation.com
### Version 1, 22mar2012
### Version 2, 4nov2015 tested with mice 2.23
### Tested with Mac OS X 10.7.3, R2.14-2, mice 2.12

if (packageVersion("mice")<'2.12') stop("This code requires mice 2.12.")

### Section 7.1 Too many columns

library("mice")
library("lattice")
library("foreign")
library("survival")

### Section 7.1.3 Data exploration

data <- leiden85   ## Note: the leiden85 data is not yet avialable in V2.12
if (!is.data.frame(data)) warning("The code for section 7.1/7.2 requires access to the LEIDEN85 data.")

ini <- mice(data, maxit=0)   # recommended 
table(ini$nmis)

table(data$beroep1, useNA="always")

v1 <- names(ini$nmis[ini$nmis==0])
outlist1 <- v1[c(1,3:5,7:10,16:47,51:60,62,64:65,69:72)]
length(outlist1)

### Section 7.1.4 Outflux

fx <- fluxplot(data, main=NULL, cex=0.9)

outlist2 <- row.names(fx)[fx$outflux<0.5]
length(outlist2)

data2 <- data[,!names(data) %in% outlist2]
fx2 <- flux(data2)
outlist3 <- row.names(fx2)[fx2$outflux<0.5]


### Section 7.1.5 Logged events

ini$log[1:3,]

outlist4 <- as.character(ini$log[,"out"])


### Section 7.1.6 Quick predictor selection for wide data

outlist <- unique(c(outlist1, outlist2, outlist4))
length(outlist)

data2 <- data[,!names(data) %in% outlist]

inlist <- c("sex","lftanam","rrsyst","rrdiast")
pred <- quickpred(data2, minpuc=0.5, inc=inlist)

table(rowSums(pred))

rowSums(pred[c("rrsyst","rrdiast"),])

names(data2)[pred["rrsyst",]==1]

vname <- "rrsyst"
y <- cbind(data2[vname], r=!is.na(data2[,vname]))
vdata <- data2[,pred[vname,]==1]
round(cor(y=y,x=vdata,use="pair"),2)


### Section 7.1.7 Generating the imputations 

### Smart imputation using quickpred - - TIME COMSUMING (30 MINUTES)
imp.qp <- mice(data2, pred=pred, ridge = 0.0001, seed=29725)

### Blind imputation - MUCH MORE TIME CONSUMING (10 HOURS)
### Not recommended
imp <- mice(data, ridge=0.01, seed=32417, maxit=2)

vnames <- c("rrsyst","rrdiast")
cd1 <- complete(imp)[,vnames]
cd2 <- complete(imp.qp)[,vnames]
typ <- factor(rep(c("blind imputation","quickpred"),each=nrow(cd1)))
mis <- ici(data2[,vnames])
mis <- is.na(imp$data$rrsyst)|is.na(imp$data$rrdiast)
cd <- data.frame(typ=typ,mis=mis,rbind(cd1, cd2))
tp72 <- xyplot(jitter(rrdiast,10) ~ jitter(rrsyst,10) | typ, data = cd, groups = mis, 
             xlab = "Systolic BP (mmHg)", ylab = "Diastolic BP (mmHg)", 
             col=c(mdc(1),mdc(2)), pch=c(1,19),type=c("g","p"), 
             strip = strip.custom(bg="grey95"), 
             scales = list(alternating=1, tck=c(1,0)))
print(tp72)


### Section 7.1.8 A further improvements: Survival as predictor variable

dat <- cbind(data2, dead = 1 - data2$dwa)
hazard <- nelsonaalen(dat, survda, dead)

### calculate correlations between hazard, t and logt (not in  
tmp <- data.frame(hazard, t=data2$survda, logt=log(data2$survda),
                  SBP=data2$rrsyst, DBP=data2$rrdiast)
round(cor(tmp, use="pair"),3)


### Section 7.2 Sensitivity analysis

### Figure 7.3

fit <- survfit(Surv(survda/365, 1-dwa) ~ is.na(rrsyst), data = data2) 
plot(fit, lty = 1, lwd=1.5, xlab="Years since intake",
     ylab="K-M Survival probability", las=1, 
     col=c(mdc(4),mdc(5)), mark.time=FALSE)
text(4,0.7,"BP measured")
text(2,0.3,"BP missing")


### Section 7.2.3 Generating imputations under the delta-adjustment
delta <- c(0,-5,-10,-15,-20)
post <- imp.qp$post

### undamped sensitivity analysis
### TIME CONSUMING (SEVERAL HOURS)
imp.all.undamped <- vector("list", length(delta))
for (i in 1:length(delta)) {
  d <- delta[i]
  cmd <- paste("imp[[j]][,i] <- imp[[j]][,i] +",d)
  post["rrsyst"] <- cmd
  imp <- mice(data2, pred=pred, post=post, maxit=1, seed=i*22, ridge=0.0001)
  imp.all.undamped[[i]] <- imp
}


### damped sensitivity analysis
### TIME CONSUMING (SEVERAL HOURS)
imp.all.damped <- vector("list", length(delta))
for (i in 1:length(delta)) {
  d <- delta[i]
  cmd <- paste("fit <- lm(y ~ as.matrix(x)); 
              damp <- sqrt(1 - summary(fit)$r.squared);
              imp[[j]][, i] <- imp[[j]][, i] + damp * ", d)
  post["rrsyst"] <- cmd
  imp <- mice(data2, pred=pred, post=post, maxit=1, seed=i*22, ridge=0.0001)
  imp.all.damped[[i]] <- imp
}


### Section 7.2.4 Complete data analysis

cda <- expression(
    sbpgp <- cut(rrsyst, breaks = c(50, 124, 
          144, 164, 184, 200, 500)),
    agegp <- cut(lftanam, breaks = c(85, 90, 
          95, 110)),
    dead  <- 1 - dwa,
    coxph(Surv(survda, dead) 
          ~ C(sbpgp, contr.treatment(6, base = 3)) 
          + strata(sexe, agegp)))
imp <- imp.all.damped[[1]]
fit <- with(imp, cda)
as.vector(exp(summary(pool(fit))[,1]))

### Table 7.5

fit1 <- with(imp.all.damped[[1]], cda)
fit2 <- with(imp.all.damped[[2]], cda)
fit3 <- with(imp.all.damped[[3]], cda)
fit4 <- with(imp.all.damped[[4]], cda)
fit5 <- with(imp.all.damped[[5]], cda)
r1<-as.vector(t(exp(summary(pool(fit1))[,c(1,6:7)])))
r2<-as.vector(t(exp(summary(pool(fit2))[,c(1,6:7)])))
r3<-as.vector(t(exp(summary(pool(fit3))[,c(1,6:7)])))
r4<-as.vector(t(exp(summary(pool(fit4))[,c(1,6:7)])))
r5<-as.vector(t(exp(summary(pool(fit5))[,c(1,6:7)])))
round(t(matrix(c(r1,r2,r3,r4,r5),nrow=15)),2)



### Section 7.3 Correct prevalence estimates from self-reported data

library("mice")
krul <- selfreport[selfreport$src=="krul",]
mgg <- selfreport[selfreport$src=="mgg",]

### Figure 7.4

xy <- xy.coords(krul$bm, krul$br-krul$bm)
plot(xy,col=mdc(1),xlab="Measured BMI",ylab="Reported - Measured BMI",
     xlim=c(17,45),ylim=c(-5,5), type="n",lwd=0.7)
polygon(x=c(30,20,30),y=c(0,10,10),col="grey95",border=NA) 
polygon(x=c(30,40,30),y=c(0,-10,-10),col="grey95",border=NA)
abline(0,0,lty=2,lwd=0.7)
points(xy, col=mdc(1),cex=0.7)
lines(lowess(xy),lwd=2,col=mdc(4))
text(1:4,x=c(40,28,20,32),y=c(4,4,-4,-4),cex=3)
box(lwd=1)

### Figure 7.5 Krul data - males
males <- krul[krul$sex=="Male",]
fit <- lm(bm~br,data=males)
plot(x=males$br,y=males$bm,
     xlim=c(26,34),ylim=c(26,34),
     xlab="Self-reported BMI",ylab="Measured BMI",col=mdc(1),
     cex.lab=1.5, cex.axis=1.3)
abline(h=30,v=30,lty=2)
abline(coef(fit),col=mdc(4))
abline(v=(30-coef(fit)[1])/coef(fit)[2],col=mdc(4))
text(1:4,x=c(33.8,33.8,26.2,26.2),y=c(33.8,26.2,26.2,33.8),cex=4)
text(c("a","b"),x=c(29.1,29.7,29.1,29.7),y=c(34.1,34.1,26.4,26.4),cex=3,adj=c(0.5,1))


### Section 7.3.4 Data

md.pattern(selfreport[,c("age","sex","hm","hr","wm","wr")])


### Section 7.3.5 Application

bmi <- function(h,w){return(w/(h/100)^2)}
init <- mice(selfreport,maxit=0)
meth <- init$meth
meth["bm"] <- "~bmi(hm,wm)"
meth[c("prg", "edu", "etn")] <- ""
pred <- init$pred
pred[,c("src","id","pop","prg","edu","etn","web","bm","br")] <- 0
imp <- mice(selfreport, pred=pred, meth=meth, seed=66573, maxit=20, m=10)


### Figure 7.6 

cd <- complete(imp, 1)
xy <- xy.coords(cd$bm, cd$br-cd$bm)
plot(xy,col=mdc(2),xlab="Measured BMI",ylab="Reported - Measured BMI",
     xlim=c(17,45),ylim=c(-5,5), type="n",lwd=0.7)
polygon(x=c(30,20,30),y=c(0,10,10),col="grey95",border=NA) 
polygon(x=c(30,40,30),y=c(0,-10,-10),col="grey95",border=NA)
abline(0,0,lty=2,lwd=0.7)

idx <- cd$src=="krul"
xyc <- xy; xyc$x <- xy$x[idx]; xyc$y <- xy$y[idx]
xys <- xy; xys$x <- xy$x[!idx]; xys$y <- xy$y[!idx]
points(xyc,col=mdc(1), cex=0.7)
points(xys,col=mdc(2), cex=0.7)
lines(lowess(xyc),col=mdc(4),lwd=2)
lines(lowess(xys),col=mdc(5),lwd=2)
text(1:4,x=c(40,28,20,32),y=c(4,4,-4,-4),cex=3)
box(lwd=1)


### Table 7.7
### Not particularly elegant code, at the moment
  prev <- matrix(NA,nr=15,nc=4)
  prev[1,1:2] <- summary(pool(with(imp, lm(br>=30~1,subset=(src=="mgg")))))[1,1:2]
  prev[1,3:4] <- summary(pool(with(imp, lm(bm>=30~1,subset=(src=="mgg")))))[1,1:2]
  prev[2,1:2] <- summary(pool(with(imp, lm(br>=30~1,subset=(src=="mgg"&sex=="Male")))))[1,1:2]
  prev[2,3:4] <- summary(pool(with(imp, lm(bm>=30~1,subset=(src=="mgg"&sex=="Male")))))[1,1:2]
  prev[3,1:2] <- summary(pool(with(imp, lm(br>=30~1,subset=(src=="mgg"&sex=="Female")))))[1,1:2]
  prev[3,3:4] <- summary(pool(with(imp, lm(bm>=30~1,subset=(src=="mgg"&sex=="Female")))))[1,1:2]
  prev[4,1:2] <- summary(pool(with(imp, lm(br>=30~1,subset=(src=="mgg"&sex=="Male"&age>=18&age<30)))))[1,1:2]
  prev[4,3:4] <- summary(pool(with(imp, lm(bm>=30~1,subset=(src=="mgg"&sex=="Male"&age>18&age<30)))))[1,1:2]
  prev[5,1:2] <- summary(pool(with(imp, lm(br>=30~1,subset=(src=="mgg"&sex=="Male"&age>=30&age<40)))))[1,1:2]
  prev[5,3:4] <- summary(pool(with(imp, lm(bm>=30~1,subset=(src=="mgg"&sex=="Male"&age>30&age<40)))))[1,1:2]
  prev[6,1:2] <- summary(pool(with(imp, lm(br>=30~1,subset=(src=="mgg"&sex=="Male"&age>=40&age<50)))))[1,1:2]
  prev[6,3:4] <- summary(pool(with(imp, lm(bm>=30~1,subset=(src=="mgg"&sex=="Male"&age>40&age<50)))))[1,1:2]
  prev[7,1:2] <- summary(pool(with(imp, lm(br>=30~1,subset=(src=="mgg"&sex=="Male"&age>=50&age<60)))))[1,1:2]
  prev[7,3:4] <- summary(pool(with(imp, lm(bm>=30~1,subset=(src=="mgg"&sex=="Male"&age>50&age<60)))))[1,1:2]
  prev[8,1:2] <- summary(pool(with(imp, lm(br>=30~1,subset=(src=="mgg"&sex=="Male"&age>=60&age<80)))))[1,1:2]
  prev[8,3:4] <- summary(pool(with(imp, lm(bm>=30~1,subset=(src=="mgg"&sex=="Male"&age>60&age<80)))))[1,1:2]
  prev[10,1:2] <- summary(pool(with(imp, lm(br>=30~1,subset=(src=="mgg"&sex=="Female"&age>=18&age<30)))))[1,1:2]
  prev[10,3:4] <- summary(pool(with(imp, lm(bm>=30~1,subset=(src=="mgg"&sex=="Female"&age>18&age<30)))))[1,1:2]
  prev[11,1:2] <- summary(pool(with(imp, lm(br>=30~1,subset=(src=="mgg"&sex=="Female"&age>=30&age<40)))))[1,1:2]
  prev[11,3:4] <- summary(pool(with(imp, lm(bm>=30~1,subset=(src=="mgg"&sex=="Female"&age>30&age<40)))))[1,1:2]
  prev[12,1:2] <- summary(pool(with(imp, lm(br>=30~1,subset=(src=="mgg"&sex=="Female"&age>=40&age<50)))))[1,1:2]
  prev[12,3:4] <- summary(pool(with(imp, lm(bm>=30~1,subset=(src=="mgg"&sex=="Female"&age>40&age<50)))))[1,1:2]
  prev[13,1:2] <- summary(pool(with(imp, lm(br>=30~1,subset=(src=="mgg"&sex=="Female"&age>=50&age<60)))))[1,1:2]
  prev[13,3:4] <- summary(pool(with(imp, lm(bm>=30~1,subset=(src=="mgg"&sex=="Female"&age>50&age<60)))))[1,1:2]
  prev[14,1:2] <- summary(pool(with(imp, lm(br>=30~1,subset=(src=="mgg"&sex=="Female"&age>=60&age<80)))))[1,1:2]
  prev[14,3:4] <- summary(pool(with(imp, lm(bm>=30~1,subset=(src=="mgg"&sex=="Female"&age>60&age<80)))))[1,1:2]
 
### find group sizes
table(mgg$sex,mgg$web)
table(mgg$sex,mgg$web,mgg$age>55)


### Section 7.4 Enhancing comparability

library("mice")

### Section 7.4.3 Independence: Imputation without a bridge study

fA <- c(242, 43, 15, 0, 6)
fB <- c(145, 110, 29, 8)
YA <- rep(ordered(c(0:3,NA)), fA)
YB <- rep(ordered(c(0:3)), fB)
Y <- rbind(data.frame(YA,YB=ordered(NA)),
           data.frame(YB,YA=ordered(NA)))

md.pattern(Y)

### simulation function
### Warning: micemill() overwrites objects 'imp' and 'tau'
###          in the global enviroment

micemill <- function(n) {
  for (i in 1:n) {
    imp <<- mice.mids(imp) # global assignment 
    cors <- with(imp, cor(as.numeric(YA),
                         as.numeric(YB),
                         method="kendall"))
    tau <<- rbind(tau, getfit(cors, s=TRUE))  # global assignment
  }
}

tau <- NULL
imp <- mice(Y, max=0, m=10, seed=32662)
micemill(50)

### Figure 7.7
plotit <- function()
  matplot(x=1:nrow(tau),y=tau,
        ylab=expression(paste("Kendall's ",tau)), 
          xlab="Iteration", type="l", lwd=1,
        lty=1:10,col="black")
plotit()

### Section 7.4.4 Fully dependent or independent?
### Use the 'walking' dataset of the mice package

## split walking: ab is sources A and B (598 records)
ab <- walking[walking$src=="A"|walking$src=="B",]

## split walking: euridiss is external source (292 records)
external <- walking[walking$src=="E",]

### create contingency tables: YA by YB (in ab)
ftable(addmargins(table(ab[,c("YA","YB")],useNA="ifany")))

### Table 7.8 contingency table YA by YB (in euridiss)
ftable(addmargins(table(external[,c("YA","YB")],useNA="ifany")))

### Section 7.4.5 Imputation using a bridge study
md.pattern(walking)

### Figure 7.8
tau <- NULL
imp <- mice(walking, max=0, m=10, seed=92786)
pred <- imp$pred
pred[,c("src","age","sex")] <- 0
imp <- mice(walking, max=0, m=10, seed=92786, pred=pred)
micemill(20)
plotit()


### impute without and with covariates

thetaAB <- NULL
props <- with(imp, mean(YB[src=="A"]=='0'))
thetaAB <<- rbind(thetaAB, getfit(props, s=TRUE)) 

micemill <- function(n) {
  for (i in 1:n) {
    imp <<- mice.mids(imp) # global assignment 
    cors <- with(imp, cor(as.numeric(YA[src=="A"]),
                         as.numeric(YB[src=="A"]),
                         method="kendall"))
    tau <<- rbind(tau, getfit(cors, s=TRUE))  # global assignment
    means <- with(imp, mean(as.numeric(YA[src=="A"]), na.rm=TRUE))
    thetaBA <<- rbind(thetaBA, getfit(means, s=TRUE)-1)
    props <- with(imp, mean(YB[src=="A"]=='0'))
    thetaAB <<- rbind(thetaAB, getfit(props, s=TRUE))    
    tabs <- with(imp, ftable(addmargins(
                      table(YA[src=="A"],YB[src=="A"],
                            useNA="ifany", dnn=c("YA","YB")))))
    print(getfit(tabs)[[2]])
  }
}


tau <- NULL
thetaBA <- NULL
thetaAB <- NULL
imp <- mice(walking, max=0, m=10, seed=99786)
oldpred <- pred <- imp$pred
pred[,c("src","age","sex")] <- 0
imp <- mice(walking, max=0, m=10, seed=99786, pred=pred)
micemill(20)
pred <- oldpred
pred[,c("src")] <- 0
imp <- mice(walking, max=0, m=10, pred=pred)
micemill(20)

### Figure 7.9

matplot(x=1:nrow(thetaAB),y=thetaAB,xlab="Iteration",
        ylab = expression(hat(theta)[AB]),
        type="l", lwd=1, lty=1:10,col="black",
        ylim=c(0.4,0.7))
abline(h=0.497, v=20, lty=2)
text(x=5,y=0.488,expression(hat(theta)[BB]),adj=0)
text(x=5,y=0.430,"Without covariates", adj=0)
text(x=25,y=0.430,"With covariates", adj=0)
arrows(x0=4.5,y0=0.488,x1=0,y1=0.495,length=0.1, angle=20)
points(x=-0.4,y=0.497,pch=20)

## file <- file.path("~/Documents/Sync/Impute/mice/V2.12/mice/data/walking.rda")
## save(walking, file=file)

## selfreport <- nl
## file <- file.path("~/Documents/Sync/Impute/mice/V2.12/mice/data/selfreport.rda")
## save(selfreport, file=file)

