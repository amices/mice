### fimd4.R
### R code from Van Buuren, S. (2012). 
###		Flexible Imputation of Missing Data. 
###		CRC/Chapman & Hall, Boca Raton, FL.
### (c) 2012 Stef van Buuren, www.multiple-imputation.com
### Version 1, 7mar2012
### Version 2, 4nov2015 tested with mice 2.23
### Tested with Mac OS X 10.7.3, R2.14-2, mice 2.12

library("mice")
library("lattice")


### Chapter 4 Multivariate missing data

### Section 4.1 Missing data patterns

### define data sets pattern1-pattern4 with four patterns
data <- matrix(sample(1:100,4*8*3,replace=TRUE),nrow=8*4,
                dimnames=list(NULL,c("A","B","C")))
data <- as.data.frame(data)
data[c(31:32),"A"] <- NA
data[c(15:16,22:24,30:32),"B"] <- NA
data[c(6:8,12:16,17:21,27:29),"C"] <- NA
mdpat <- cbind(expand.grid(rec = 8:1, pat = 1:4, var = 1:3), r=as.numeric(as.vector(is.na(data))))
pattern1 <- data[1:8,]
pattern2 <- data[9:16,]
pattern3 <- data[17:24,]
pattern4 <- data[25:32,]

### Figure 4.1
types <-  c("Univariate","Monotone","File matching","General")
tp41 <- levelplot(r~var+rec|as.factor(pat), data=mdpat,
          as.table=TRUE, aspect="iso",
          shrink=c(0.9), 
          col.regions = mdc(1:2),
          colorkey=FALSE,
          scales=list(draw=FALSE),
          xlab="", ylab="",
          between = list(x=1,y=0),
          strip = strip.custom(bg = "grey95", style = 1,
            factor.levels = types))
print(tp41)

md.pattern(pattern4)
p <- md.pairs(pattern4)
p


### proportion of usable cases
p$mr/(p$mr+p$mm)


### outbound statistics
p$rm/(p$rm+p$rr)

### Figure 4.2
par(mfrow=c(2,2))
fluxplot(pattern1, main="", xlim=c(-0.1,1.1), ylim=c(-0.1,1.1))
text(x=0.5,y=1,label="Univariate")
fluxplot(pattern2, main="", xlim=c(-0.1,1.1), ylim=c(-0.1,1.1))
text(x=0.5,y=1,label="Monotone")
fluxplot(pattern3, main="", xlim=c(-0.1,1.1), ylim=c(-0.1,1.1))
text(x=0.5,y=1,label="File matching")
fluxplot(pattern4, main="", xlim=c(-0.1,1.1), ylim=c(-0.1,1.1))
text(x=0.5,y=1,label="General")

### calculate pobs, influx, outflux of general pattern
flux(pattern4)[,1:3]

### section 4.3 Monotone data imputation
### monotone data imputation on three columns
data <- nhanes2[,1:3]
md.pattern(data)
imp <- mice(data, visit="monotone", maxit=1, m=2)

### Numerical example, approximate two-step
ini <- mice(nhanes2, maxit=0)
pred <- ini$pred
pred["bmi","chl"] <- 0
pred["hyp",c("chl","bmi")] <- 0
imp <- mice(nhanes2, vis="monotone", pred=pred, maxit=1, m=2)

### Section 4.5 Fully Conditional Specification

### Example of slow convergence
generate <- function(n = c(1000, 4500, 4500, 0),
                     cor = matrix(c(1, 0.9, 0.9, 0.9, 1, 0.7, 0.9, 0.7, 1), nrow = 3)) {
  require(MASS)
  nt <- sum(n)
  cs <- cumsum(n)
  data <- mvrnorm(nt, mu = rep(0,3), Sigma = cor)
  dimnames(data) <- list(1:nt, c("X", "Y1", "Y2"))
  if (n[2] > 0) data[(cs[1]+1):cs[2],"Y1"] <- NA 
  if (n[3] > 0) data[(cs[2]+1):cs[3],"Y2"] <- NA
  if (n[4] > 0) data[(cs[3]+1):cs[4],c("Y1","Y2")] <- NA
  return(data)
}

impute <- function(data, m = 5, method="norm", print=FALSE, maxit = 10, ...) {
  statistic <- matrix(NA, nrow=maxit, ncol=m)
  for (iter in 1:maxit) {
    if (iter==1) imp <- mice(data, m = m, method = method, print=print, maxit = 1, ...)
    else imp <- mice.mids(imp, maxit=1, print=print, ...)
    statistic[iter,] <- unlist(with(imp, cor(Y1,Y2))$analyses)
  }
  return(list(imp=imp, statistic=statistic))
}

simulate <- function(ns=matrix(c(1000,500,250,100,50,0,
                               rep(c(4500,4750,4875,4950,4975,5000),2),
                       rep(0,6)), nrow=6), m = 5, maxit=10,
                       seed=1, ...) {
  if (!missing(seed)) set.seed(seed)
  s <- cbind(rep(1:nrow(ns), each=maxit*m),
             apply(ns, 2, rep, each=maxit*m),
             rep(1:maxit,each=m), 1:m, NA)
  colnames(s) <- c("k","n111","n101","n110","n100","iteration","m","rY1Y2")
  for (k in 1:nrow(ns)){
    data <- generate(ns[k,], ...)
    r <- impute(data, m=m, maxit=maxit, ...)
    s[s[,"k"]==k,"rY1Y2"] <- t(r$statistic) 
  }
  return(data.frame(s))
}

### perform simulation - TIME CONSUMING (10 minutes)
slow.demo <- simulate(maxit=150, seed=62771)

### Figure 4.3
labels <- c("90% missing", "95% missing", "97.5% missing", 
            "99% missing", "99.5% missing", "100% missing")
tp43 <- xyplot(rY1Y2~iteration|as.factor(k), group=m,
       data = slow.demo, layout=c(3,2),
       type="l", as.table = TRUE,
       ylab = "Correlation between Y1 and Y2",
       xlab = "Iteration", col=mdc(3),
       scales=list(y=list(alternating=1, tck=c(1,0))),
       strip = strip.custom(bg="grey95", style=1,
         factor.levels=labels))
print(tp43)

### Section 4.6.3 Illustration

## boys data: select subset
select <- with(boys, age>=8 & age<=21.0)

### version with all numeric data for jm and pmm
djm <- boys[select,-4]
djm$gen <- as.integer(djm$gen)
djm$phb <- as.integer(djm$phb)
djm$reg <- as.integer(djm$reg)

### version with categorical variables for fcs
dfcs <- boys[select,-4]

### impute according to joint multivariate normal
jm.10 <- mice(djm, method="norm", seed=93005, m=10)

### impute according to predictive mean matching
pmm.10 <- mice(djm, seed=71332,m=10)

### impute according to proportional odds model
fcs.10 <- mice(dfcs, seed=81420, m=10)

### Figure 4.4
tp44 <- xyplot(jm.10, gen~age|.imp, subset=as.integer(.imp)<7, 
       xlab="Age", ylab="Genital stage")
print(tp44)

### Figure 4.5
tp45 <- xyplot(fcs.10, gen~age|.imp, subset=as.integer(.imp)<7,
       xlab="Age", ylab="Genital stage")
print(tp45)

### Figure 4.6 is too complex, and will not be given here


