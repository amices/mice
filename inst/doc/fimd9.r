### fimd9.R
### R code from Van Buuren, S. (2012). 
###		Flexible Imputation of Missing Data. 
###		CRC/Chapman & Hall, Boca Raton, FL.
### (c) 2012 Stef van Buuren, www.multiple-imputation.com
### Version 1, 21mar2012
### Tested with Mac OS X 10.7.3, R2.14-2, mice 2.12

if (packageVersion("mice")<'2.12') stop("This code requires mice 2.12.")

### Section 9.2 SE Fireworks Disaster Study

### Section 9.2.1 Intention to treat

library("mice")

### Table 9.1

yvars <- c("yc1","yc2","yc3", "yp1","yp2","yp3")
x <- cbind(fdd[,c("id","trt","pp",yvars)])
x

md.pattern(fdd[fdd$pp=="Y",yvars])
md.pattern(fdd[fdd$pp=="N",yvars])


### Section 9.2.2 Imputation model

pred <- fdd.pred 
vars <-c("ypa1", "ypb1", "ypc1",
         "ypa2", "ypb2", "ypc2",
         "ypa3", "ypb3", "ypc3")
pred[vars[1:3],vars]

dry <- mice(fdd, maxit=0)
method <- dry$method
method["yc1"] <- "~I(yca1 + ycb1 + ycc1)"
method["yc2"] <- "~I(yca2 + ycb2 + ycc2)"
method["yc3"] <- "~I(yca3 + ycb3 + ycc3)"
method["yp1"] <- "~I(ypa1 + ypb1 + ypc1)"
method["yp2"] <- "~I(ypa2 + ypb2 + ypc2)"
method["yp3"] <- "~I(ypa3 + ypb3 + ypc3)"
imp <- mice(fdd,pred=pred,meth=method,maxit=20,seed=54434)


### Section 9.2.3 Inspecting imputations

lowi <- complete(imp, "long", inc=TRUE)
lowi <- data.frame(lowi,cbcl2=NA, cbin2=NA,cbex2=NA)
lolo <- reshape(lowi, idvar = 'id',
                varying = 11:ncol(lowi),
                direction = "long",
                new.row.names = 1:(nrow(lowi)*3),
                sep="")
lolo <- lolo[order(lolo$.imp, lolo$id, lolo$time),]
row.names(lolo) <- 1:nrow(lolo)


### Figure 9.1

iv <- is.na(lolo[lolo$.imp==0,]$yp)
ivn <- ifelse(iv,1,0)
col12  <- c("grey80","grey80",
            mdc(2),mdc(1),
            mdc(2),"transparent",
            mdc(2),"transparent",
            mdc(2),"transparent",
            mdc(2),"transparent")
ic <- unique(lolo$id[iv])
ss <- lolo$id %in% ic
grp <- 2*as.integer(lolo$.imp) - ivn
loloss <- data.frame(lolo, grp=grp)
trellis.par.set(strip.background=list(col="grey95"))
tp1 <- xyplot(yp~time|factor(id), data=loloss, type="l",
      groups=factor(.imp), col="grey80", subset=ss,
       ylab="UCLA-RI Parent Score", pch=19, cex=1,
       xlab="Time", xlim=c("T1","T2","T3"),
             as.table=TRUE)
tp2 <- xyplot(yp~time|factor(id), data=loloss, type="p",
       groups=grp, col=col12, subset=ss,
       ylab="UCLA-RI Parent Score",        
       pch=19,
       cex=0.8,
       xlab="Time", xlim=c("T1","T2","T3"),
       as.table=TRUE)
print(tp1)
print(tp2, newpage=FALSE)


### Figure 9.2

means <- aggregate(lolo$yp, list(lolo$.imp!=0,lolo$trt,lolo$time), mean, na.rm=TRUE)
names(means) <- c(".imp","trt","time","yp")
levels(means$trt) <- c("EMDR","CBT")
tp <- xyplot(yp~time|trt, data=means, type="o", pch=19,
      groups=factor(.imp), col=c(mdc(4), mdc(6)),
       ylab="UCLA-RI Parent Score", lwd=2,
       xlab="Time", xlim=c("T1","T2","T3"))
print(tp)


### Figure 9.3

means <- aggregate(lolo$yc, list(lolo$.imp!=0,lolo$trt,lolo$time), mean, na.rm=TRUE)
names(means) <- c(".imp","trt","time","yc")
levels(means$trt) <- c("EMDR","CBT")
tp <- xyplot(yc~time|trt, data=means, type="o", pch=19,
      groups=factor(.imp), col=c(mdc(4),mdc(6)),
       ylab="UCLA-RI Child Score", lwd=2,
       xlab="Time", xlim=c("T1","T2","T3"))
print(tp)


### Section 9.3 Time raster imputation

library("mice")
library("splines")
library("lme4")

data <- tbc
md.pattern(data)

### remove those with nocc 1 or 2 or 3
data <- data[data$nocc>=3,]

### Section 9.3.3 Broken stick model

### specify break ages
brk <- c(0, 8/365, 1/3, 1, 2, 6, 10, 18, 29)
k <- length(brk)
                  
### calculate B-spline
X <- bs(data$age,
        knots = brk,
        B = c(brk[1],brk[k]+0.0001),
        degree = 1)
X <- X[,-(k+1)]
dimnames(X)[[2]] <- paste("x",1:ncol(X),sep="")
data <- cbind(data,X)
round(head(X,3),2)

### fit broken stick model
fit <- lmer(wgt.z~0+x1+x2+x3+x4+x5+x6+x7+x8+x9+
                 (0+x1+x2+x3+x4+x5+x6+x7+x8+x9|id),
                 data=data)

### calculate size and increment per person
tsiz <- t(ranef(fit)$id) + fixef(fit)
tinc <- diff(tsiz)

### inspect size and increment estimates
round(head(t(tsiz)),2)
round(head(t(tinc)),2)

### broken stick for hgt and BMI
fit.wgt <- fit
fit.hgt <- lmer(hgt.z~0+x1+x2+x3+x4+x5+x6+x7+x8+x9+
                 (0+x1+x2+x3+x4+x5+x6+x7+x8+x9|id),
                 data=data)
fit.bmi <- lmer(bmi.z~0+x1+x2+x3+x4+x5+x6+x7+x8+x9+
                 (0+x1+x2+x3+x4+x5+x6+x7+x8+x9|id),
                 data=data)


### define time warp

warp.setup <- data.frame(age = brk, age2 = seq(0, 29, length.out=k))
warp.model <- lm(age2 ~ bs(age, knots=brk[c(-1,-k)], degree=1) - 1, 
                 data=warp.setup, x=T, y=T)
warped.knots <- warp.model$y
maxage <- max(warped.knots)
age2  <- predict(warp.model,newdata=data)
data  <- cbind(data, age2=age2)
rm(age2)

### Figure: time warping function (not in book)

par(fin=c(3,3),omi=c(0,0,0,0),mai=c(1,1,1,1),mfrow=c(1,1),lwd=0.5)
eqscplot(x=brk, y=warp.model$y, 
         ylab="Warped age",
         xlab="Age (years)",
         axes=F,type="o",lwd=1,pch=16)
axis(1, at=seq(0,32,4), cex=0.5, pos=c(-0.5,0))
axis(2, at=warp.setup$age2,
     labels=c("0y","8d","4m","1y","2y","6y","10y","18y","29y"),
     line=0.5,cex=0.5)
lines(x=c(0,29), y=c(0,29), lwd=1)


# Prepare for figure 9.5 (weight, height and BMI profiles for six cases)
 
wsiz.bs <- extractBS(fit.wgt)
hsiz.bs <- extractBS(fit.hgt)
bsiz.bs <- extractBS(fit.bmi)
id <- unique(fit.wgt@flist$id)

data2 <- appendbreak(data, brk, id=id, warp.model=warp.model, typ="pred")
data2[data2$typ=="pred",c("wgt.z","hgt.z","bmi.z")] <- round(cbind(wsiz.bs, hsiz.bs, bsiz.bs),3)

six <- c(1259,7019,2447,7460,8046,7646)
set.seed(23221)
sample <- unique(c(six,sample(unique(tbc$id),300)))

idx <- data2$id %in% six
pd  <- data2[idx,]
pd$id <- as.factor(pd$id)
pd <- cbind(pd,rowname=row.names(pd))
pdr <- reshape(pd, 
               varying = c("wgt.z","hgt.z","bmi.z"),
               idvar = "rowname",
               direction="long",
               v.names="y")
pdr$grp <- pdr$time
pdr$grp[pdr$typ=="pred"] <- pdr$grp[pdr$typ=="pred"] + 3


### Figure 9.5 Only weight

tbcstick <- xyplot(y~age2|id, data=pdr, xlim=c(-1,maxage+1), ylim=c(-4,4),
       as.table = TRUE, 
       scales = list(x=list(at=warped.knots, 
              labels=c("0","8d","4m","1y","2y","6y","10y","18y","29y"))),
                   xlab = "Age", ylab = "Weight SDS",
                   layout = c(2,3),
                   subset = (grp==1 | grp==4),
                   groups = grp,
                   pch=19,
                   type=c(rep("p",3),rep("l",3)), 
                   distribute.type = TRUE, 
                   lwd=2,
                   col=c(rep(mdc(1),3),rep(mdc(5),3)),
                   panel = function(...) {
         panel.abline(v=warped.knots, lty=2, col="grey80")
         panel.abline(h=c(-2,0,2), lty=1, col="grey80")
         panel.xyplot(...)
       },
       strip = strip.custom(bg="grey95"))
print(tbcstick)

### Same plot, but then with height and BMI added (not in book)

tbcstick <- xyplot(y~age2|id, data=pdr, xlim=c(-1,maxage+1), ylim=c(-4,4),
       as.table = TRUE, 
       scales = list(x=list(at=warped.knots, 
              labels=c("0","8d","4m","1y","2y","6y","10y","18y","29y"))),
       xlab = "Age", ylab = "Standard Deviation Score (SDS)",
       layout = c(2,3),
       groups = grp,
       pch=20,
       type=c(rep("p",3),rep("l",3)), distribute.type = TRUE, 
       lwd=1.5,
       col=c(mdc(1), hcl(80,100,40,0.7),hcl(160,100,40,0.7)),
       panel = function(...) {
         panel.abline(v=warped.knots, lty=2, col="grey80")
         panel.abline(h=c(-2,0,2), lty=1, col="grey80")
         panel.xyplot(...)
       },
       strip = strip.custom(bg="grey95"),
       key = list(
         text   = list(c("Weight","Height","BMI"), cex=0.8),
         lines  = list(lty=1, lwd=2, type="l"), 
         col  = c(mdc(1), hcl(80,100,40,0.7),hcl(160,100,40,0.7)),
         columns = 3,
         space  = "bottom",
         border = FALSE)
       )
print(tbcstick)

### Section 9.3.6 Imputation

### append break ages before imputation
id <- unique(data$id)
data2 <- appendbreak(data, brk, id=id, warp.model=warp.model, typ="sup")
table(data2$typ)

options(digits=2)
head(data2)

### specify imputation methods
Y <- c("hgt.z","wgt.z","bmi.z")
imp <- mice(data2, maxit=0)
meth <- imp$method
meth[1:length(meth)] <- ""
meth[Y] <- "2l.norm"

## Note: The following statements are outdated
##       and replaced by the intercept=FALSE option
##       in the call to mice()
## mice.impute.2l.norm.noint <- mice.impute.2l.norm
## meth[Y] <- "2l.norm.noint"

### specify predictor matrix
pred <- imp$pred
pred[1:nrow(pred),1:ncol(pred)] <- 0
pred[Y,"id"] <- (-2) # class variable
pred[Y,"sex"] <- 1 # fixed effect
pred[Y, paste("x",1:9,sep="")] <- 2
pred[Y[1],Y[2]] <- 2  # mutual random effect
pred[Y[2],Y[1]] <- 2  # mutual random effect
pred[Y[3],Y[1:2]] <- 2  # random effect


### specify visit sequence
vis <- 1:3
names(vis) <- c("hgt.z","wgt.z","bmi.z")

### ready to go - TIME CONSUMING (30 MINUTES)
imp.1 <- mice(data2, intercept = FALSE, meth=meth, pred=pred, m=5, maxit=10, seed=52711)
imp.2 <- mice(data2, intercept = FALSE, meth=meth, pred=pred, m=5, maxit=10, seed=88348)
imp.1745 <- ibind(imp.1, imp.2)  ## create m=10 imputations


### Figure 9.6 diagnostic plot
### Note: The solution is different from that in the book
###       The sample size in the book is much larger (n=2604 instead of n=306)
###       The larger sample regularizes many of the imputed sticks
###       The plot for non-typical subject 7646 reveals that
###       with n=306, fit to the data is low for this subject

cd <- complete(imp.1745,"long")
sup <- cd[cd$typ=="sup",]
data3 <- data.frame(.imp=0, data2)
data3 <- rbind(data3, sup[,-2])
idx <- data3$id %in% six
pd  <- data3[idx,]
pd$id <- as.factor(pd$id)
pd$grp <- pd$.imp
pd$grp[pd$grp==0] <- NA
pd$grp[pd$typ=="obs"] <- 11
pd$grp <- reorder(pd$grp, as.numeric(pd$grp))
tbcimp <- xyplot(wgt.z~age2|id, data=pd, xlim=c(-1,maxage+1), ylim=c(-4,4),
       as.table = TRUE, 
       scales = list(x=list(at=warped.knots, 
              labels=c("0","8d","4m","1y","2y","6y","10y","18y","29y"))),
       xlab = "Age", ylab = "Weight SDS",
       groups=grp, layout = c(2,3),
       pch=c(rep(20,10), 19),
       type=c(rep("l",10),"p"), lwd=c(rep(1,10),1),
       col=c(rep(mdc(5),10),mdc(1)), distribute.type=TRUE,
       panel = function(...) {
         panel.abline(v=warped.knots, lty=2, col="grey80")
         panel.abline(h=c(-2,0,2), lty=1, col="grey80")
         panel.xyplot(...)
       },
       strip=strip.custom(bg="grey95")
       )
print(tbcimp)


### Figure 9.7

cd <- complete(imp.1745,1)
idx <- (cd$id) %in% sample
cd <- cd[idx,]
shingle <- cut(cd$age,breaks=c(brk,29.01),right=FALSE, inc=TRUE,
               labels=c("0d-8d","8d-4m","4m-1y","1y-2y","2y-6y",
                 "6y-10y","10y-18y","18y-29y","29y"))
tbchw <- xyplot(wgt.z~hgt.z|shingle,data=cd, 
       xlim=c(-4,4), ylim=c(-4,4), type=c("p","g"),
       group=(typ=="sup"),pch=c(1,20),
       col=c(mdc(1:2)),
       xlab="Height SDS", ylab="Weight SDS",pty="s",
       strip=strip.custom(bg="grey95"))
print(tbchw)

### Section 9.3.7 Complete data analysis

### post-processing of imputed data

imp <- imp.1745
cd <- complete(imp,"long")
sup <- cd[cd$typ=="sup",]

sup$age <- round(sup$age,2)
sup$hgt.z <- round(sup$hgt.z,2)
sup$wgt.z <- round(sup$wgt.z,2)
sup$bmi.z <- round(sup$bmi.z,2)

lowi <- reshape(sup, idvar=c('id','.imp'), timevar = 'age', 
                v.names=c('hgt.z','wgt.z','bmi.z'), 
                direction="wide",
                drop=c(".id","occ","first","typ","hgt","wgt","bmi",
                  paste("x",1:9,sep=""),"age2"))
hsiz <- lowi[,c(".imp","id","nocc","sex",
                "hgt.z.0","hgt.z.0.02","hgt.z.0.33",
                "hgt.z.1","hgt.z.2","hgt.z.6","hgt.z.10","hgt.z.18",
                "hgt.z.29")]
wsiz <- lowi[,c(".imp","id","nocc","sex",
                "wgt.z.0","wgt.z.0.02","wgt.z.0.33",
                "wgt.z.1","wgt.z.2","wgt.z.6","wgt.z.10","wgt.z.18",
                "wgt.z.29")]
bsiz <- lowi[,c(".imp","id","nocc","sex",
                "bmi.z.0","bmi.z.0.02","bmi.z.0.33",
                "bmi.z.1","bmi.z.2","bmi.z.6","bmi.z.10","bmi.z.18",
                "bmi.z.29")]
# merge outcome data
bsiz <- merge(bsiz, tbc.target,all.x=TRUE)


hinc <- cbind(hsiz[,1:4],t(diff(t(hsiz[,-1:-4]))))
winc <- cbind(wsiz[,1:4],t(diff(t(wsiz[,-1:-4]))))
binc <- cbind(bsiz[,1:4],t(diff(t(bsiz[,-1:-4]))))

# merge outcome data
binc <- merge(binc, tbc.target,all.x=TRUE)

bmi.z.0.02 <- by(binc, binc$.imp, function(x) lm(bmi.z.0.02~ao, data = x, na.action='na.omit'))
bmi.z.0.33 <- by(binc, binc$.imp, function(x) lm(bmi.z.0.33~ao, data = x, na.action='na.omit'))
bmi.z.1    <- by(binc, binc$.imp, function(x) lm(bmi.z.1   ~ao, data = x, na.action='na.omit'))
bmi.z.2    <- by(binc, binc$.imp, function(x) lm(bmi.z.2   ~ao, data = x, na.action='na.omit'))
bmi.z.6    <- by(binc, binc$.imp, function(x) lm(bmi.z.6   ~ao, data = x, na.action='na.omit'))
bmi.z.10   <- by(binc, binc$.imp, function(x) lm(bmi.z.10  ~ao, data = x, na.action='na.omit'))
bmi.z.18   <- by(binc, binc$.imp, function(x) lm(bmi.z.18  ~ao, data = x, na.action='na.omit'))
bmi.z.29   <- by(binc, binc$.imp, function(x) lm(bmi.z.29  ~ao, data = x, na.action='na.omit'))

tab.bmi.z.0.02 <- summary(pool(as.mira(bmi.z.0.02)))
tab.bmi.z.0.33 <- summary(pool(as.mira(bmi.z.0.33)))
tab.bmi.z.1    <- summary(pool(as.mira(bmi.z.1)))
tab.bmi.z.2    <- summary(pool(as.mira(bmi.z.2)))
tab.bmi.z.6    <- summary(pool(as.mira(bmi.z.6)))
tab.bmi.z.10   <- summary(pool(as.mira(bmi.z.10)))
tab.bmi.z.18   <- summary(pool(as.mira(bmi.z.18)))
tab.bmi.z.29   <- summary(pool(as.mira(bmi.z.29)))

(bmi.z.0.33 <- summary(pool(as.mira(by(bsiz, bsiz$.imp, function(x) lm(bmi.z.jv~bmi.z.0.33+bmi.z.0.02, data = x, na.action='na.omit'))))))
(bmi.z.1 <- summary(pool(as.mira(by(bsiz, bsiz$.imp, function(x) lm(bmi.z.jv~bmi.z.1+bmi.z.0.33, data = x, na.action='na.omit'))))))
(bmi.z.2 <- summary(pool(as.mira(by(bsiz, bsiz$.imp, function(x) lm(bmi.z.jv~bmi.z.2+bmi.z.1, data = x, na.action='na.omit'))))))
(bmi.z.6 <- summary(pool(as.mira(by(bsiz, bsiz$.imp, function(x) lm(bmi.z.jv~bmi.z.6+bmi.z.2, data = x, na.action='na.omit'))))))
(bmi.z.10 <- summary(pool(as.mira(by(bsiz, bsiz$.imp, function(x) lm(bmi.z.jv~bmi.z.10+bmi.z.6, data = x, na.action='na.omit'))))))
(bmi.z.18 <- summary(pool(as.mira(by(bsiz, bsiz$.imp, function(x) lm(bmi.z.jv~bmi.z.18+bmi.z.10, data = x, na.action='na.omit'))))))

(bmi.z.6 <- summary(pool(as.mira(by(bsiz, bsiz$.imp, function(x) lm(bmi.z.jv~bmi.z.6+bmi.z.2, data = x, na.action='na.omit'))))))
summary(by(bsiz, bsiz$.imp, function(x) lm(bmi.z.jv~bmi.z.6+bmi.z.2, data = x, na.action='na.omit'))[[2]])


# repeat the analysis for the broken stick estimates

bsiz.bs <- as.data.frame(t(t(ranef(fit.bmi)[[1]]) + fixef(fit.bmi)))
dimnames(bsiz.bs)[[2]] <- names(bsiz)[5:13]
binc.bs <- t(diff(t(bsiz.bs)))
bsiz.bs <- data.frame(id=bsiz[bsiz$.imp==1,"id"], bsiz.bs)
binc.bs <- data.frame(id=bsiz[bsiz$.imp==1,"id"], binc.bs)
bsiz.bs <- merge(bsiz.bs, tbc.target)
binc.bs <- merge(binc.bs, tbc.target)

bmi.z.0.02.bs <- lm(bmi.z.0.02~ao, data = binc.bs, na.action='na.omit')
bmi.z.0.33.bs <- lm(bmi.z.0.33~ao, data = binc.bs, na.action='na.omit')
bmi.z.1.bs    <- lm(bmi.z.1   ~ao, data = binc.bs, na.action='na.omit')
bmi.z.2.bs    <- lm(bmi.z.2   ~ao, data = binc.bs, na.action='na.omit')
bmi.z.6.bs    <- lm(bmi.z.6   ~ao, data = binc.bs, na.action='na.omit')
bmi.z.10.bs   <- lm(bmi.z.10  ~ao, data = binc.bs, na.action='na.omit')
bmi.z.18.bs   <- lm(bmi.z.18  ~ao, data = binc.bs, na.action='na.omit')

summary(bmi.z.0.02.bs)
summary(bmi.z.0.33.bs)
summary(bmi.z.1.bs)
summary(bmi.z.2.bs)
summary(bmi.z.6.bs)
summary(bmi.z.10.bs)
summary(bmi.z.18.bs)

### Materials for Table 9.3

a<-round(cor(bsiz[,-(1:4)],use="pair"),2)
b<-round(cor(bsiz.bs,use="pair"),2)


a2<-round(cor(bsiz[!is.na(bsiz$ao),-(1:4)],use="complete.obs"),2)
b2<-round(cor(bsiz.bs[!is.na(bsiz$ao),],use="complete.obs"),2)



