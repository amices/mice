### fimd6.R
### R code from Van Buuren, S. (2012). 
###		Flexible Imputation of Missing Data. 
###		CRC/Chapman & Hall, Boca Raton, FL.
### (c) 2012 Stef van Buuren, www.multiple-imputation.com
### Version 1, 14mar2012
### Tested with Mac OS X 10.7.3, R2.14-2, mice 2.11

library("mice")

### Section 6.1.1 Averaging and stacking the data

imp <- mice(nhanes, print=FALSE, seed=55152)
stacked <- complete(imp, "long")
fit <- lm(chl~bmi+age,data=stacked)
coef(fit)

### Section 6.1.2 Repeated analyses

fit <- with(imp, lm(chl~bmi+age))
coef(fit$analyses[[1]])
coef(fit$analyses[[2]])

est <- pool(fit)
summary(est)

expr <- expression(freq <- table(hyp), freq[1]-freq[2])
fit <- with(imp, eval(expr))
unlist(fit$an)


### Section 6.3.5 Computation

imp <- mice(nhanes2, seed=23210, print=FALSE)
fit <- with(imp, lm(bmi~age+chl))
fit.restrict <- with(imp, lm(bmi~1))
res <- pool.compare(fit, fit.restrict)
res$pvalue

est <- pool(fit)
summary(est)

C <- matrix(c(c(0,1,-1,0),c(0,2,-1,0),c(0,0,0,1)),nrow=3,ncol=4,byrow=TRUE)
c <- c(0,0,0.1)
q <- C %*% est$qbar
u <- diag(C %*% est$ubar %*% t(C))
t <- diag(C %*% est$t %*% t(C))
C1 <- C
C1[C1!=0] <- 1
df <- C1 %*% est$df    # this needs to change to compromise df
d <- (q-c)^2/t
pvalue <- 1-pf(d, 1, df)
pvalue


### Section 6.4.2 Computation

data <- boys[boys$age>=8,-4]
imp <- mice(data, seed=28382, m=10, print=FALSE)
expr <- expression(
    f1 <- lm(tv~1),
    f2 <- step(f1, scope=list(upper=~age+hgt+wgt+hc+gen+phb+reg), lower=~1)) 
fit <- with(imp, expr)
formulas <- lapply(fit$an, formula)
terms <- lapply(formulas, terms)
vars <- unlist(lapply(terms, labels))
table(vars)

fit.without <- with(imp, lm(tv~age+gen+reg+phb))
fit.with <- with(imp, lm(tv~age+gen+reg+phb+hgt))
pool.compare(fit.with, fit.without)$pvalue

fit.without <- with(imp, lm(tv~age+gen+reg))
fit.with <- with(imp, lm(tv~age+gen+reg+phb))
pool.compare(fit.with, fit.without)$pvalue


