### fimd2.R
### R code from Van Buuren, S. (2012). 
###		Flexible Imputation of Missing Data. 
###		CRC/Chapman & Hall, Boca Raton, FL.
### (c) 2012 Stef van Buuren, www.multiple-imputation.com
### Version 1, 7mar2012
### Version 2, 4nov2015
### Tested with Mac OS X 10.7.3, R2.14-2, mice 2.11

### Chapter 2 Multiple imputation

library("mice")
library("lattice")
library("MASS")

### Section 2.1.3 The expanding literature on multiple imputation
### Figure 2.1

cit  <- c(     2010, 37, 182, NA,
               2009, 36, 129, NA,
               2008, 27, 111, NA,
               2007, 35, 136, NA,
               2006, 18, 80, NA,
               2005, 20, 76, NA,
               2004,  6, 56, NA,
               2003, 17, 42, NA,
               2002, 14, 40, NA,
               2001, 13, 36, 57,
               2000,  8, 21, 33,
               1999,  6, 25, 47,
               1998, 6, 13, 22,
               1997, 6, 18, 29,
               1996, 4, 12, 28,
               1995, 3, 5, 20,
               1994, 2, 5, 34,
               1993, 2, 6, 15,
               1991, 2, 4, 19,
               1990, 1, 2, 15,
               1989, NA, NA, 11,
               1988, 1, NA, 13,
               1987, NA, NA, 10,
               1986, NA, NA, 5,
               1985, NA, NA, 1,
               1984, NA, NA, 2,
               1983, NA, NA, 5,
               1982, NA, NA, 2,
               1981, NA, NA, 1,
               1980, NA, NA, 5,
               1979, NA, NA, 2,
               1978, NA, NA, 1,
               1977, NA, NA, 2)
cit <- matrix(cit, nr=2010-1977, nc=4, byrow=TRUE)
cit <- as.data.frame(cit)
names(cit) <- c("Year","Title","Abstract","All")
par(mfrow=c(1,1))
par(cex=0.7, lwd=0.5)
plot(x=cit$Year, y=cit$Abstract, type="o",log="y",
     xlim=c(1975,2010),ylim=c(1,200),
     ylab="Number of publications (log)", xlab="Year", pch=2, 
     axes=FALSE)
axis(1, lwd=par("lwd"))
axis(2, lwd=par("lwd"), las=1)
# box(lwd=0.5)
lines(x=cit$Year, y=cit$Title, pch=15, type="o")
lines(x=cit$Year, y=cit$All, pch=16, type="o")
legend(x=1975,y=200,legend=c("early publications",
                            "'multiple imputation' in abstract",
                            "'multiple imputation' in title"),
       pch=c(16,2,15), bty="n")

### Section 2.2.4 MCAR, MAR and MNAR again

logistic <- function(x) exp(x)/(1+exp(x))
set.seed(80122)
n <- 300
y <- mvrnorm(n=n,mu=c(0,0),Sigma=matrix(c(1,0.5,0.5,1),nrow=2))
y1 <- y[,1]
y2 <- y[,2]
r2.mcar <- 1-rbinom(n, 1, 0.5)
r2.mar  <- 1-rbinom(n, 1, logistic(y1))
r2.mnar <- 1-rbinom(n, 1, logistic(y2))

### Figure 2.2

y3 <- rbind(y,y,y)
r2 <- c(r2.mcar,r2.mar,r2.mnar)
r2 <- factor(r2, labels=c("Ymis","Yobs"))
typ <- factor(rep(3:1,each=n),labels=c("MNAR","MAR","MCAR"))
d <- data.frame(y1=y3[,1],y2=y3[,2],r2=r2,typ=typ)
trellis.par.set(box.rectangle=list(col=c(mdc(2),mdc(1)),lwd=1.2))
trellis.par.set(box.umbrella=list(col=c(mdc(2),mdc(1)),lwd=1.2))
trellis.par.set(plot.symbol=list(col=mdc(3),lwd=1))
tp <- bwplot(r2~y2|typ, data=d,
             horizontal=TRUE, layout=c(1,3),
             xlab=expression(Y^2),
             col=c(mdc(2),mdc(1)),strip=FALSE, xlim=c(-3,3),
             strip.left = strip.custom(bg="grey95"))
print(tp)

### Section 2.3.7 Numerical example
options(digits=3)
imp <- mice(nhanes, print=FALSE, m=10, seed=24415)
fit <- with(imp, lm(bmi~age))
est <- pool(fit)
names(est)
attach(est)
(r+2/(df+3))/(r+1)
(df+1)/(df+3)*lambda+2/(df+3)
detach(est)
