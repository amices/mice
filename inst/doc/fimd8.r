### fimd8.R
### R code from Van Buuren, S. (2012). 
###		Flexible Imputation of Missing Data. 
###		CRC/Chapman & Hall, Boca Raton, FL.
### (c) 2012 Stef van Buuren, www.multiple-imputation.com
### Version 1, 22mar2012
### Tested with Mac OS X 10.7.3, R2.14-2, mice 2.12

if (packageVersion("mice")<'2.12') stop("This code requires mice 2.12.")

library("mice")


### Section 8.1 Correcting for selective drop-out

data <- pops
pred <- pops.pred
if (!is.data.frame(data)) stop("The code for section 8.1 requires access to the POPS data.")


### Section 8.1.4 A degenerate solution (TIME CONSUMING (30 MINUTES))

imp1 <- mice(data, pred=pred, maxit=20, seed=51121)

### Figure 8.2

tp82 <- plot(imp1, c("a10u","a10b","adhd"),col=mdc(5),lty=1:5)
print(tp82)


### Section 8.1.5 A better solution

##  TIME CONSUMING (30 MINUTES)
pred2 <- pred
pred2[61:86,61:86] <- 0
imp2 <- mice(data, pred=pred2, maxit=20, seed=51121)

### Figure 8.3

tp83 <- bwplot(imp2, iq+e_tot+l19_sd+b19_sd+coping+seffz~.imp,
        layout=c(2,3))
print(tp83)


### Section 8.1.6 Results

### Full responders
summary(lm(as.numeric(a10u)~1,data,na.action=na.omit))
summary(lm(as.numeric(a10b)~1,data,na.action=na.omit))
summary(lm(as.numeric(adhd)~1,data,na.action=na.omit))

### All children
summary(pool(with(imp2,lm(as.numeric(a10u)~1))))
summary(pool(with(imp2,lm(as.numeric(a10b)~1))))
summary(pool(with(imp2,lm(as.numeric(adhd)~1))))


### Section 8.2 Correcting for nonresponse

library("mice")
data <- fdgs

### Section 8.2.4 Augmenting the sample

nimp <- c(400, 600, 75, 300, 200, 400)
regcat <- c("North","City","North","East","North","City")
reg <- rep(regcat, nimp)

nimp2 <- floor(rep(nimp, each=2)/2)
nimp2[5:6] <- c(38,37)
sex <- rep(rep(c("boy","girl"),6), nimp2)

minage <- rep(c(0, 0, 10, 10, 14, 14), nimp)
maxage <- rep(c(10, 10, 14, 14, 21, 21), nimp)
set.seed(42444)
age <- runif(length(minage), minage, maxage)

id <- 600001:601975

pad <- data.frame(id, reg, age, sex, hgt=NA, wgt=NA, hgt.z=NA, wgt.z=NA)
data2 <- rbind(data, pad)


### Figure 8.4

## inspect the age by region pattern
means <- aggregate(data$hgt.z, by=list(reg=data$reg, 
             age=floor(data$age)), mean, na.rm=TRUE)
tp84 <- xyplot(x~age, means, group=reg, type=c("g","l"),
             lty=1:5,col=mdc(4), 
             xlim=c(-1,22), ylim=c(-0.6, 0.8), 
             ylab="Height (SDS)", xlab="Age (years)",
             key=list(
               text=list(levels(means$reg)), 
               lines=list(lty=1:5, col=mdc(4)), 
               x=0.1, y=0.98, background="white",
               columns=3, between.columns=0),
             scales=list(x=list(tck=c(1,0)), 
               y=list(tck=c(1,0))))
print(tp84)


### Section 8.2.5 Imputation model

## add interaction terms
na.opt <- options(na.action=na.pass)
int <- model.matrix(~I(age-10)*hgt.z+I(age-10)*wgt.z+age*reg, data=data2)[,-(1:9)]
options(na.opt)
data3 <- cbind(data2, int)

### define the imputation model

ini <- mice(data3, maxit=0)

meth <- ini$meth
meth["hgt"] <- ""
meth["wgt"] <- ""
meth["hgt.z"] <- "norm"
meth["wgt.z"] <- "norm"
meth["I(age - 10):hgt.z"] <- "~I(I(age-10)*hgt.z)"
meth["I(age - 10):wgt.z"] <- "~I(I(age-10)*wgt.z)"

pred <- ini$pred
pred[,c("hgt","wgt")] <- 0
pred["hgt.z", c("id","I(age - 10):hgt.z")] <- 0
pred["wgt.z", c("id","I(age - 10):wgt.z")] <- 0

vis <- ini$vis[c(3,5,4,6)]

### run the imputation model

imp <- mice(data3, meth=meth, pred=pred, vis=vis, m=10, maxit=20, seed=28107)

### Figure 8.5

cda <- complete(imp, "long", include=TRUE)
means2 <- aggregate(cda$hgt.z, by=list(reg=cda$reg, age=floor(cda$age), imp=cda$.imp), mean, na.rm=TRUE)
tp85 <- xyplot(x~age|reg,means2, group=imp, subset=(reg=="North"|reg=="City"),
              type=c("g","l"),lwd=c(4,rep(1,imp$m)),
              lty=1:5,col=c(mdc(4),rep(mdc(6),imp$m)), 
              ylab="Height (SDS)", xlab="Age (years)",
              ylim=c(-0.5,0.8), xlim=c(-2,23),
              scales=list(x=list(alternating=FALSE, tck=c(1,0)),
                y=list(tck=c(1,0))),
                strip = strip.custom(bg="grey95"))
print(tp85)

