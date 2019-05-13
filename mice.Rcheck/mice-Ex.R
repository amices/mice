pkgname <- "mice"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('mice')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("ampute")
### * ampute

flush(stderr()); flush(stdout())

### Name: ampute
### Title: Generate Missing Data for Simulation Purposes
### Aliases: ampute

### ** Examples

# Simulate data set with \code{mvrnorm} from package \code{\pkg{MASS}}.
require(MASS)
sigma <- matrix(data = c(1, 0.2, 0.2, 0.2, 1, 0.2, 0.2, 0.2, 1), nrow = 3)
complete.data <- mvrnorm(n = 100, mu = c(5, 5, 5), Sigma = sigma)
# Perform quick amputation
result1 <- ampute(data = complete.data)
# Change default matrices as desired
patterns <- result1$patterns
patterns[1:3, 2] <- 0
odds <- result1$odds
odds[2,3:4] <- c(2, 4)
odds[3,] <- c(3, 1, NA, NA)
# Rerun amputation
result2 <- ampute(data = complete.data, patterns = patterns, freq = 
c(0.3, 0.3, 0.4), cont = FALSE, odds = odds)
# Run an amputation procedure with continuous probabilities
result3 <- ampute(data = complete.data, type = c("RIGHT", "TAIL", "LEFT"))




cleanEx()
nameEx("as.mids")
### * as.mids

flush(stderr()); flush(stdout())

### Name: as.mids
### Title: Converts an multiply imputed dataset (long format) into a 'mids'
###   object
### Aliases: as.mids
### Keywords: mids

### ** Examples

# impute the nhanes dataset
imp <- mice(nhanes, print = FALSE)
# extract the data in long format
X <- complete(imp, action = "long", include = TRUE)
# create dataset with .imp variable as numeric
X2 <- X

# nhanes example without .id
test1 <- as.mids(X)
is.mids(test1)
identical(complete(test1, action = "long", include = TRUE), X)

# nhanes example without .id where .imp is numeric
test2 <- as.mids(X2)
is.mids(test2)
identical(complete(test2, action = "long", include = TRUE), X)

# nhanes example, where we explicitly specify .id as column 2
test3 <- as.mids(X, .id = ".id")
is.mids(test3)
identical(complete(test3, action = "long", include = TRUE), X)

# nhanes example with .id where .imp is numeric
test4 <- as.mids(X2, .id = 2)
is.mids(test4)
identical(complete(test4, action = "long", include = TRUE), X)

# example without an .id variable
# variable .id not preserved
X3 <- X[, -2]
test5 <- as.mids(X3)
is.mids(test5)
identical(complete(test5, action = "long", include = TRUE)[, -2], X[, -2])

# as() syntax has fewer options
test7 <- as(X, "mids")
test8 <- as(X2, "mids")
test9 <- as(X2[, -2], "mids")
rev <- ncol(X):1
test10 <- as(X[, rev], "mids")

# where argument copies also observed data into $imp element
where <- matrix(TRUE, nrow = nrow(nhanes), ncol = ncol(nhanes))
colnames(where) <- colnames(nhanes)
test11 <- as.mids(X, where = where)
identical(complete(test11, action = "long", include = TRUE), X)



cleanEx()
nameEx("boys")
### * boys

flush(stderr()); flush(stdout())

### Name: boys
### Title: Growth of Dutch boys
### Aliases: boys
### Keywords: datasets

### ** Examples



# create two imputed data sets
imp <- mice(boys, m=1, maxit=2)
z <- complete(imp, 1)

# create imputations for age <8yrs
plot(z$age, z$gen, col=mdc(1:2)[1+is.na(boys$gen)],
 xlab = "Age (years)", ylab = "Tanner Stage Genital")

# figure to show that the default imputation method does not impute BMI 
# consistently
plot(z$bmi,z$wgt/(z$hgt/100)^2, col=mdc(1:2)[1+is.na(boys$bmi)],
xlab = "Imputed BMI", ylab="Calculated BMI")   

# also, BMI distributions are somewhat different
require(MASS)
oldpar <- par(mfrow=c(1,2))
truehist(z$bmi[!is.na(boys$bmi)],h=1,xlim=c(10,30),ymax=0.25,
col=mdc(1),xlab="BMI observed")
truehist(z$bmi[is.na(boys$bmi)],h=1,xlim=c(10,30),ymax=0.25,
col=mdc(2),xlab="BMI imputed")
par(oldpar)

# repair the inconsistency problem by passive imputation
meth <- imp$meth
meth["bmi"] <- "~I(wgt/(hgt/100)^2)"
pred <- imp$predictorMatrix
pred["hgt","bmi"] <- 0
pred["wgt","bmi"] <- 0
imp2 <- mice(boys, m=1, maxit=2, meth=meth, pred=pred)
z2 <- complete(imp2, 1)

# show that new imputations are consistent
plot(z2$bmi,z2$wgt/(z2$hgt/100)^2, col=mdc(1:2)[1+is.na(boys$bmi)],
ylab="Calculated BMI")   

# and compare distributions
oldpar <- par(mfrow=c(1,2))
truehist(z2$bmi[!is.na(boys$bmi)],h=1,xlim=c(10,30),ymax=0.25,col=mdc(1),
xlab="BMI observed")
truehist(z2$bmi[is.na(boys$bmi)],h=1,xlim=c(10,30),ymax=0.25,col=mdc(2),
xlab="BMI imputed")
par(oldpar)





graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("bwplot.mids")
### * bwplot.mids

flush(stderr()); flush(stdout())

### Name: bwplot.mids
### Title: Box-and-whisker plot of observed and imputed data
### Aliases: bwplot.mids bwplot
### Keywords: hplot

### ** Examples


imp <- mice(boys, maxit=1)

### box-and-whisker plot per imputation of all numerical variables
bwplot(imp)

### tv (testicular volume), conditional on region
bwplot(imp, tv~.imp|reg)

### same data, organized in a different way 
bwplot(imp, tv~reg|.imp, theme=list())






cleanEx()
nameEx("cbind.mids")
### * cbind.mids

flush(stderr()); flush(stdout())

### Name: cbind.mids
### Title: Combine 'mids' objects by columns
### Aliases: cbind.mids
### Keywords: manip

### ** Examples


# impute four variables at once (default)
imp <- mice(nhanes, m = 1, maxit = 1, print = FALSE)
imp$predictorMatrix

# impute two by two
data1 <- nhanes[, c("age", "bmi")]
data2 <- nhanes[, c("hyp", "chl")]
imp1 <- mice(data1, m = 2, maxit = 1, print = FALSE)
imp2 <- mice(data2, m = 2, maxit = 1, print = FALSE)

# Append two solutions
imp12 <- cbind(imp1, imp2)

# This is a different imputation model
imp12$predictorMatrix

# Append the other way around
imp21 <- cbind(imp2, imp1)
imp21$predictorMatrix

# Append 'forgotten' variable chl
data3 <- nhanes[, 1:3]
imp3  <- mice(data3, maxit = 1,m = 2, print = FALSE)
imp4 <- cbind(imp3, chl = nhanes$chl)

# Of course, chl was not imputed
head(complete(imp4))

# Combine mids object with data frame
imp5 <- cbind(imp3, nhanes2)
head(complete(imp5))



cleanEx()
nameEx("cc")
### * cc

flush(stderr()); flush(stdout())

### Name: cc
### Title: Select complete cases
### Aliases: cc
### Keywords: univar

### ** Examples


# cc(nhanes)   # get the 13 complete cases
# cc(nhanes$bmi) # extract complete bmi



cleanEx()
nameEx("cci")
### * cci

flush(stderr()); flush(stdout())

### Name: cci
### Title: Complete case indicator
### Aliases: cci
### Keywords: univar

### ** Examples

cci(nhanes) # indicator for 13 complete cases 
cci(mice(nhanes, maxit = 0))
f <- cci(nhanes[,c("bmi","hyp")]) # complete data for bmi and hyp
nhanes[f,] # obtain all data from those with complete bmi and hyp



cleanEx()
nameEx("complete")
### * complete

flush(stderr()); flush(stdout())

### Name: complete
### Title: Extracts the completed data from a 'mids' object
### Aliases: complete
### Keywords: manip

### ** Examples


# obtain first imputed data set
sum(is.na(nhanes2))
imp <- mice(nhanes2, print = FALSE, maxit = 1)
dat <- complete(imp)
sum(is.na(dat))

# obtain stacked third and fifth imputation
dat <- complete(imp, c(3, 5))

# obtain all datasets, with additional identifiers
head(complete(imp, "long"))

# same, but now as list, mild object
dslist <- complete(imp, "all")
length(dslist)

# same, but also include the original data
dslist <- complete(imp, "all", include = TRUE)
length(dslist)

# select original + 3 + 5, store as mild
dslist <- complete(imp, c(0, 3, 5), mild = TRUE)
names(dslist)




cleanEx()
nameEx("construct.blocks")
### * construct.blocks

flush(stderr()); flush(stdout())

### Name: construct.blocks
### Title: Construct blocks from 'formulas' and 'predictorMatrix'
### Aliases: construct.blocks

### ** Examples

form <- name.formulas(list(bmi + hyp ~ chl + age, chl ~ bmi))
pred <- make.predictorMatrix(nhanes[, c("age", "chl")])
construct.blocks(formulas = form, pred = pred)



cleanEx()
nameEx("densityplot.mids")
### * densityplot.mids

flush(stderr()); flush(stdout())

### Name: densityplot.mids
### Title: Density plot of observed and imputed data
### Aliases: densityplot.mids densityplot
### Keywords: hplot

### ** Examples

imp <- mice(boys, maxit=1)

### density plot of head circumference per imputation
### blue is observed, red is imputed
densityplot(imp, ~hc|.imp)

### All combined in one panel.
densityplot(imp, ~hc)




cleanEx()
nameEx("fdd")
### * fdd

flush(stderr()); flush(stdout())

### Name: fdd
### Title: SE Fireworks disaster data
### Aliases: fdd fdd.pred
### Keywords: datasets

### ** Examples



data <- fdd
md.pattern(fdd)





cleanEx()
nameEx("fdgs")
### * fdgs

flush(stderr()); flush(stdout())

### Name: fdgs
### Title: Fifth Dutch growth study 2009
### Aliases: fdgs
### Keywords: datasets

### ** Examples



data <- data(fdgs)
summary(data)




cleanEx()
nameEx("fix.coef")
### * fix.coef

flush(stderr()); flush(stdout())

### Name: fix.coef
### Title: Fix coefficients and update model
### Aliases: fix.coef

### ** Examples

model0 <- lm(Volume ~ Girth + Height, data = trees)
formula(model0)
coef(model0)
deviance(model0)

# refit same model
model1 <- fix.coef(model0)
formula(model1)
coef(model1)
deviance(model1)

# change the beta's
model2 <- fix.coef(model0, beta = c(-50, 5, 1))
coef(model2)
deviance(model2)

# compare predictions
plot(predict(model0), predict(model1)); abline(0,1)
plot(predict(model0), predict(model2)); abline(0,1)

# compare proportion explained variance
cor(predict(model0), predict(model0) + residuals(model0))^2
cor(predict(model1), predict(model1) + residuals(model1))^2
cor(predict(model2), predict(model2) + residuals(model2))^2

# extract offset from constrained model
summary(model2$model$offset)

# it also works with factors and missing data
model0 <- lm(bmi ~ age + hyp + chl, data = nhanes2)
model1 <- fix.coef(model0)
model2 <- fix.coef(model0, beta = c(15, -8, -8, 2, 0.2))



cleanEx()
nameEx("getfit")
### * getfit

flush(stderr()); flush(stdout())

### Name: getfit
### Title: Extract list of fitted model
### Aliases: getfit
### Keywords: manip

### ** Examples


imp <- mice(nhanes)
fit <- with(imp, lm(bmi~chl+hyp))
getfit(fit)
getfit(fit, 2)




cleanEx()
nameEx("glm.mids")
### * glm.mids

flush(stderr()); flush(stdout())

### Name: glm.mids
### Title: Generalized linear model for 'mids' object
### Aliases: glm.mids
### Keywords: multivariate

### ** Examples


imp <- mice(nhanes)

# logistic regression on the imputed data
fit <- glm.mids((hyp==2)~bmi+chl, data=imp, family = binomial)
fit




cleanEx()
nameEx("ibind")
### * ibind

flush(stderr()); flush(stdout())

### Name: ibind
### Title: Enlarge number of imputations by combining 'mids' objects
### Aliases: ibind
### Keywords: manip

### ** Examples

data(nhanes)
imp1 <- mice(nhanes, m = 1, maxit = 2, print = FALSE)
imp1$m

imp2 <- mice(nhanes, m = 3, maxit = 3, print = FALSE)
imp2$m

imp12 <- ibind(imp1, imp2)
imp12$m
plot(imp12)



cleanEx()
nameEx("ic")
### * ic

flush(stderr()); flush(stdout())

### Name: ic
### Title: Select incomplete cases
### Aliases: ic
### Keywords: univar

### ** Examples


ic(nhanes)   # get the 12 rows with incomplete cases 
ic(nhanes[1:10,])  # incomplete cases within the first ten rows
ic(nhanes[, c("bmi", "hyp")])  # restrict extraction to variables bmi and hyp




cleanEx()
nameEx("ici")
### * ici

flush(stderr()); flush(stdout())

### Name: ici
### Title: Incomplete case indicator
### Aliases: ici ici,data.frame-method ici,matrix-method ici,mids-method
### Keywords: univar

### ** Examples


 ici(nhanes) # indicator for 12 rows with incomplete cases 




cleanEx()
nameEx("lm.mids")
### * lm.mids

flush(stderr()); flush(stdout())

### Name: lm.mids
### Title: Linear regression for 'mids' object
### Aliases: lm.mids
### Keywords: multivariate

### ** Examples



imp <- mice(nhanes)
fit <- lm.mids(bmi~hyp+chl, data = imp)
fit




cleanEx()
nameEx("make.blocks")
### * make.blocks

flush(stderr()); flush(stdout())

### Name: make.blocks
### Title: Creates a 'blocks' argument
### Aliases: make.blocks

### ** Examples

make.blocks(nhanes)
make.blocks(c("age", "sex", "edu"))



cleanEx()
nameEx("make.blots")
### * make.blots

flush(stderr()); flush(stdout())

### Name: make.blots
### Title: Creates a 'blots' argument
### Aliases: make.blots

### ** Examples

make.predictorMatrix(nhanes)
make.blots(nhanes, blocks = name.blocks(c("age", "hyp"), "xxx"))



cleanEx()
nameEx("make.formulas")
### * make.formulas

flush(stderr()); flush(stdout())

### Name: make.formulas
### Title: Creates a 'formulas' argument
### Aliases: make.formulas

### ** Examples

f1 <- make.formulas(nhanes)
f1
f2 <- make.formulas(nhanes, blocks = make.blocks(nhanes, "collect"))
f2

# for editing, it may be easier to work with the character vector
c1 <- as.character(f1)
c1

# fold it back into a formula list
f3 <- name.formulas(lapply(c1, as.formula))
f3




cleanEx()
nameEx("make.method")
### * make.method

flush(stderr()); flush(stdout())

### Name: make.method
### Title: Creates a 'method' argument
### Aliases: make.method

### ** Examples

make.method(nhanes2)



cleanEx()
nameEx("make.post")
### * make.post

flush(stderr()); flush(stdout())

### Name: make.post
### Title: Creates a 'post' argument
### Aliases: make.post

### ** Examples

make.post(nhanes2)



cleanEx()
nameEx("make.predictorMatrix")
### * make.predictorMatrix

flush(stderr()); flush(stdout())

### Name: make.predictorMatrix
### Title: Creates a 'predictorMatrix' argument
### Aliases: make.predictorMatrix

### ** Examples

make.predictorMatrix(nhanes)
make.predictorMatrix(nhanes, blocks = make.blocks(nhanes, "collect"))



cleanEx()
nameEx("make.visitSequence")
### * make.visitSequence

flush(stderr()); flush(stdout())

### Name: make.visitSequence
### Title: Creates a 'visitSequence' argument
### Aliases: make.visitSequence

### ** Examples

make.visitSequence(nhanes)



cleanEx()
nameEx("make.where")
### * make.where

flush(stderr()); flush(stdout())

### Name: make.where
### Title: Creates a 'where' argument
### Aliases: make.where

### ** Examples

head(make.where(nhanes), 3)



cleanEx()
nameEx("mammalsleep")
### * mammalsleep

flush(stderr()); flush(stdout())

### Name: mammalsleep
### Title: Mammal sleep data
### Aliases: mammalsleep sleep
### Keywords: datasets

### ** Examples



sleep <- data(mammalsleep)





cleanEx()
nameEx("md.pairs")
### * md.pairs

flush(stderr()); flush(stdout())

### Name: md.pairs
### Title: Missing data pattern by variable pairs
### Aliases: md.pairs
### Keywords: univar

### ** Examples



pat <- md.pairs(nhanes)
pat

# show that these four matrices decompose the total sample size
# for each pair
pat$rr + pat$rm + pat$mr + pat$mm

# percentage of usable cases to impute row variable from column variable
round(100*pat$mr/(pat$mr+pat$mm))




cleanEx()
nameEx("md.pattern")
### * md.pattern

flush(stderr()); flush(stdout())

### Name: md.pattern
### Title: Missing data pattern
### Aliases: md.pattern
### Keywords: univar

### ** Examples



md.pattern(nhanes)
#     age hyp bmi chl
#  13   1   1   1   1  0
#   1   1   1   0   1  1
#   3   1   1   1   0  1
#   1   1   0   0   1  2
#   7   1   0   0   0  3
#   0   8   9  10 27





cleanEx()
nameEx("mdc")
### * mdc

flush(stderr()); flush(stdout())

### Name: mdc
### Title: Graphical parameter for missing data plots.
### Aliases: mdc
### Keywords: hplot

### ** Examples


# all six colors
mdc(1:6)

# lines color for observed and missing data
mdc(c('obs','mis'), 'lin')




cleanEx()
nameEx("mice")
### * mice

flush(stderr()); flush(stdout())

### Name: mice
### Title: 'mice': Multivariate Imputation by Chained Equations
### Aliases: mice mice-package
### Keywords: iteration

### ** Examples



# do default multiple imputation on a numeric matrix
imp <- mice(nhanes)
imp

# list the actual imputations for BMI
imp$imp$bmi

# first completed data matrix
complete(imp)


# imputation on mixed data with a different method per column

mice(nhanes2, meth=c('sample','pmm','logreg','norm'))




cleanEx()
nameEx("mice.impute.2l.bin")
### * mice.impute.2l.bin

flush(stderr()); flush(stdout())

### Name: mice.impute.2l.bin
### Title: Imputation by a two-level logistic model using 'glmer'
### Aliases: mice.impute.2l.bin
### Keywords: datagen

### ** Examples

library(tidyr)
library(dplyr)
data("toenail", package = "HSAUR3")
data <- tidyr::complete(toenail, patientID, visit) %>% 
 tidyr::fill(treatment) %>% 
 dplyr::select(-time) %>%
 dplyr::mutate(patientID = as.integer(patientID))

## Not run: 
##D pred <- mice(data, print = FALSE, maxit = 0, seed = 1)$pred
##D pred["outcome", "patientID"] <- -2
##D imp <- mice(data, method = "2l.bin", pred = pred, maxit = 1, m = 1, seed = 1)
## End(Not run)



cleanEx()
nameEx("mice.impute.2l.pan")
### * mice.impute.2l.pan

flush(stderr()); flush(stdout())

### Name: mice.impute.2l.pan
### Title: Imputation by a two-level normal model using 'pan'
### Aliases: mice.impute.2l.pan 2l.pan

### ** Examples


###################################
# simulate some data
# two-level regression model with fixed slope

# number of groups
G <- 250
# number of persons
n <- 20
# regression parameter
beta <- .3
# intraclass correlation
rho <- .30
# correlation with missing response
rho.miss <- .10
# missing proportion
missrate <- .50
y1 <- rep( rnorm( G , sd = sqrt( rho ) ) , each=n ) + rnorm(G*n , sd = sqrt( 1 - rho )) 
x <-  rnorm( G*n )
y <- y1 + beta  * x
dfr0 <- dfr <- data.frame( "group" = rep(1:G , each=n ) , "x" = x , "y" = y )
dfr[ rho.miss * x + rnorm( G*n , sd = sqrt( 1 - rho.miss ) ) < qnorm( missrate ) , "y" ] <- NA

#.....
# empty imputation in mice
imp0 <- mice( as.matrix(dfr)  , maxit=0 )
predM <- imp0$predictorMatrix
impM <- imp0$method

#...
# specify predictor matrix and imputationMethod
predM1 <- predM
predM1["y","group"] <- -2
predM1["y","x"] <- 1        # fixed x effects imputation
impM1 <- impM
impM1["y"] <- "2l.pan"

# multilevel imputation
imp1 <- mice( as.matrix( dfr ) , m = 1 , predictorMatrix = predM1 , 
           imputationMethod = impM1 , maxit=1 )
# multilevel analysis
library(lme4)
mod <- lmer( y ~ ( 1 + x | group) + x , data = complete(imp1) )
summary(mod)

############################################
# Examples of predictorMatrix specification

# random x effects
# predM1["y","x"] <- 2

# fixed x effects and group mean of x
# predM1["y","x"] <- 3        

# random x effects and group mean of x
# predM1["y","x"] <- 4        




cleanEx()
nameEx("mice.impute.2lonly.norm")
### * mice.impute.2lonly.norm

flush(stderr()); flush(stdout())

### Name: mice.impute.2lonly.norm
### Title: Imputation at level 2 by Bayesian linear regression
### Aliases: mice.impute.2lonly.norm 2lonly.norm

### ** Examples


##################################################
# simulate some data
# x,y ... level 1 variables
# v,w ... level 2 variables

G <- 250            # number of groups
n <- 20             # number of persons
beta <- .3          # regression coefficient
rho <- .30          # residual intraclass correlation
rho.miss <- .10     # correlation with missing response
missrate <- .50     # missing proportion
y1 <- rep( rnorm( G , sd = sqrt( rho ) ) , each=n ) + rnorm(G*n , sd = sqrt( 1 - rho )) 
w <- rep( round( rnorm(G ) , 2 ) , each=n )
v <- rep( round( runif( G , 0 , 3 ) ) , each=n )
x <-  rnorm( G*n ) 
y <- y1 + beta  * x + .2 * w + .1 * v
dfr0 <- dfr <- data.frame( "group" = rep(1:G , each=n ) , "x" = x , "y" = y , "w" = w , "v" = v )
dfr[ rho.miss * x + rnorm( G*n , sd = sqrt( 1 - rho.miss ) ) < qnorm( missrate ) , "y" ] <- NA
dfr[ rep( rnorm(G) , each=n ) < qnorm( missrate ) , "w" ] <- NA
dfr[ rep( rnorm(G) , each=n ) < qnorm( missrate ) , "v" ] <- NA

#....
# empty mice imputation
imp0 <- mice( as.matrix(dfr)  , maxit=0 )
predM <- imp0$predictorMatrix
impM <- imp0$method

#...
# multilevel imputation
predM1 <- predM
predM1[c("w","y","v"),"group"] <- -2
predM1["y","x"] <- 1        # fixed x effects imputation
impM1 <- impM
impM1[c("y","w","v")] <- c("2l.pan" , "2lonly.norm" , "2lonly.pmm" )

# y ... imputation using pan
# w ... imputation at level 2 using norm
# v ... imputation at level 2 using pmm

imp1 <- mice( as.matrix( dfr ) , m = 1 , predictorMatrix = predM1 , 
           imputationMethod = impM1 , maxit=1 , paniter=500)
   



cleanEx()
nameEx("mice.impute.2lonly.pmm")
### * mice.impute.2lonly.pmm

flush(stderr()); flush(stdout())

### Name: mice.impute.2lonly.pmm
### Title: Imputation at level 2 by predictive mean matching
### Aliases: mice.impute.2lonly.pmm 2lonly.pmm

### ** Examples


##################################################
# simulate some data
# x,y ... level 1 variables
# v,w ... level 2 variables

G <- 250            # number of groups
n <- 20             # number of persons
beta <- .3          # regression coefficient
rho <- .30          # residual intraclass correlation
rho.miss <- .10     # correlation with missing response
missrate <- .50     # missing proportion
y1 <- rep( rnorm( G , sd = sqrt( rho ) ) , each=n ) + rnorm(G*n , sd = sqrt( 1 - rho )) 
w <- rep( round( rnorm(G ) , 2 ) , each=n )
v <- rep( round( runif( G , 0 , 3 ) ) , each=n )
x <-  rnorm( G*n ) 
y <- y1 + beta  * x + .2 * w + .1 * v
dfr0 <- dfr <- data.frame( "group" = rep(1:G , each=n ) , "x" = x , "y" = y , "w" = w , "v" = v )
dfr[ rho.miss * x + rnorm( G*n , sd = sqrt( 1 - rho.miss ) ) < qnorm( missrate ) , "y" ] <- NA
dfr[ rep( rnorm(G) , each=n ) < qnorm( missrate ) , "w" ] <- NA
dfr[ rep( rnorm(G) , each=n ) < qnorm( missrate ) , "v" ] <- NA

#....
# empty mice imputation
imp0 <- mice( as.matrix(dfr)  , maxit=0 )
predM <- imp0$predictorMatrix
impM <- imp0$method

#...
# multilevel imputation
predM1 <- predM
predM1[c("w","y","v"),"group"] <- -2
predM1["y","x"] <- 1        # fixed x effects imputation
impM1 <- impM
impM1[c("y","w","v")] <- c("2l.pan" , "2lonly.norm" , "2lonly.pmm" )

# turn v into a categorical variable
dfr$v <- as.factor(dfr$v)
levels(dfr$v) <- LETTERS[1:4]

# y ... imputation using pan
# w ... imputation at level 2 using norm
# v ... imputation at level 2 using pmm

imp <- mice(dfr, m = 1, predictorMatrix = predM1 , 
           imputationMethod = impM1, maxit = 1, paniter = 500)




cleanEx()
nameEx("mice.impute.cart")
### * mice.impute.cart

flush(stderr()); flush(stdout())

### Name: mice.impute.cart
### Title: Imputation by classification and regression trees
### Aliases: mice.impute.cart cart
### Keywords: datagen

### ** Examples

require(rpart)

imp <- mice(nhanes2, meth = 'cart', minbucket = 4)
plot(imp)




cleanEx()
nameEx("mice.impute.jomoImpute")
### * mice.impute.jomoImpute

flush(stderr()); flush(stdout())

### Name: mice.impute.jomoImpute
### Title: Multivariate multilevel imputation using 'jomo'
### Aliases: mice.impute.jomoImpute
### Keywords: datagen

### ** Examples




cleanEx()
nameEx("mice.impute.midastouch")
### * mice.impute.midastouch

flush(stderr()); flush(stdout())

### Name: mice.impute.midastouch
### Title: Imputation by predictive mean matching with distance aided donor
###   selection
### Aliases: mice.impute.midastouch
### Keywords: datagen

### ** Examples

# do default multiple imputation on a numeric matrix
imp <- mice(nhanes, method = 'midastouch')
imp

# list the actual imputations for BMI
imp$imp$bmi

# first completed data matrix
complete(imp)

# imputation on mixed data with a different method per column
mice(nhanes2, method = c('sample', 'midastouch', 'logreg', 'norm'))



cleanEx()
nameEx("mice.impute.panImpute")
### * mice.impute.panImpute

flush(stderr()); flush(stdout())

### Name: mice.impute.panImpute
### Title: Impute multilevel missing data using 'pan'
### Aliases: mice.impute.panImpute
### Keywords: datagen

### ** Examples

blocks <-  list(c("bmi", "chl", "hyp"), "age")
method <- c("panImpute", "pmm")
ini <- mice(nhanes, blocks = blocks, method = method, maxit = 0)
pred <- ini$pred
pred["B1", "hyp"] <- -2
imp <- mice(nhanes, blocks = blocks, method = method, pred = pred, maxit = 1)




cleanEx()
nameEx("mice.impute.pmm")
### * mice.impute.pmm

flush(stderr()); flush(stdout())

### Name: mice.impute.pmm
### Title: Imputation by predictive mean matching
### Aliases: mice.impute.pmm pmm
### Keywords: datagen

### ** Examples

# We normally call mice.impute.pmm() from within mice()
# But we may call it directly as follows (not recommended)

set.seed(53177)
xname <- c('age', 'hgt', 'wgt')
r <- stats::complete.cases(boys[, xname])
x <- boys[r, xname]
y <- boys[r, 'tv']
ry <- !is.na(y)
table(ry)

# percentage of missing data in tv
sum(!ry) / length(ry)

# Impute missing tv data
yimp <- mice.impute.pmm(y, ry, x)
length(yimp)
hist(yimp, xlab = 'Imputed missing tv')

# Impute all tv data
yimp <- mice.impute.pmm(y, ry, x, wy = rep(TRUE, length(y)))
length(yimp)
hist(yimp, xlab = 'Imputed missing and observed tv')
plot(jitter(y), jitter(yimp), 
    main = 'Predictive mean matching on age, height and weight', 
    xlab = 'Observed tv (n = 224)',
    ylab = 'Imputed tv (n = 224)')
abline(0, 1)
cor(y, yimp, use = 'pair')



cleanEx()
nameEx("mice.impute.quadratic")
### * mice.impute.quadratic

flush(stderr()); flush(stdout())

### Name: mice.impute.quadratic
### Title: Imputation of quadratic terms
### Aliases: mice.impute.quadratic quadratic
### Keywords: datagen

### ** Examples

require(lattice)

# Create Data
B1 = .5
B2 = .5
X <- rnorm(1000)
XX <- X^2
e <- rnorm(1000, 0, 1)
Y <- B1 * X + B2 * XX + e
dat <- data.frame(x = X, xx = XX, y = Y)

# Impose 25 percent MCAR Missingness
dat[0 == rbinom(1000, 1, 1 -.25), 1:2] <- NA

# Prepare data for imputation
ini <- mice(dat, maxit = 0)
meth <- c("quadratic", "~I(x^2)", "")
pred <- ini$pred
pred[, "xx"] <- 0

# Impute data
imp <- mice(dat, meth = meth, pred = pred)

# Pool results
pool(with(imp, lm(y ~ x + xx)))

# Plot results
stripplot(imp)
plot(dat$x, dat$xx, col = mdc(1), xlab = "x", ylab = "xx")
cmp <- complete(imp)
points(cmp$x[is.na(dat$x)], cmp$xx[is.na(dat$x)], col = mdc(2))



cleanEx()
nameEx("mice.impute.rf")
### * mice.impute.rf

flush(stderr()); flush(stdout())

### Name: mice.impute.rf
### Title: Imputation by random forests
### Aliases: mice.impute.rf
### Keywords: datagen

### ** Examples

library("lattice")

imp <- mice(nhanes2, meth = "rf", ntree = 3)
plot(imp)




cleanEx()
nameEx("mice.mids")
### * mice.mids

flush(stderr()); flush(stdout())

### Name: mice.mids
### Title: Multivariate Imputation by Chained Equations (Iteration Step)
### Aliases: mice.mids
### Keywords: iteration

### ** Examples


imp1 <- mice(nhanes, maxit=1, seed = 123)
imp2 <- mice.mids(imp1)

# yields the same result as
imp <- mice(nhanes, maxit=2, seed = 123)

# verification
identical(imp$imp, imp2$imp)
# 



cleanEx()
nameEx("name.blocks")
### * name.blocks

flush(stderr()); flush(stdout())

### Name: name.blocks
### Title: Name imputation blocks
### Aliases: name.blocks

### ** Examples

blocks <- list(c("hyp", "chl"), AGE = "age", c("bmi", "hyp"), "edu")
name.blocks(blocks)



cleanEx()
nameEx("name.formulas")
### * name.formulas

flush(stderr()); flush(stdout())

### Name: name.formulas
### Title: Name formula list elements
### Aliases: name.formulas

### ** Examples

# fully conditionally specified main effects model
form1 <- list(bmi ~ age + chl + hyp, 
              hyp ~ age + bmi + chl,
              chl ~ age + bmi + hyp)
form1 <- name.formulas(form1)
imp1 <- mice(nhanes, formulas = form1, print = FALSE, m = 1, seed = 12199)

# same model using dot notation
form2 <- list(bmi ~ ., hyp ~ ., chl ~ .)
form2 <- name.formulas(form2)
imp2 <- mice(nhanes, formulas = form2, print = FALSE, m = 1, seed = 12199)
identical(complete(imp1), complete(imp2))

# same model using repeated multivariate imputation
form3 <- name.blocks(list(all = bmi + hyp + chl ~ .))
imp3 <- mice(nhanes, formulas = form3, print = FALSE, m = 1, seed = 12199)
cmp3 <- complete(imp3)
identical(complete(imp1), complete(imp3))

# same model using predictorMatrix
imp4 <- mice(nhanes, print = FALSE, m = 1, seed = 12199, auxiliary = TRUE)
identical(complete(imp1), complete(imp4))

# different model: multivariate imputation for chl and bmi
form5 <- list(chl + bmi ~ ., hyp ~ bmi + age)
form5 <- name.formulas(form5)
imp5 <- mice(nhanes, formulas = form5, print = FALSE, m = 1, seed = 71712)



cleanEx()
nameEx("ncc")
### * ncc

flush(stderr()); flush(stdout())

### Name: ncc
### Title: Number of complete cases
### Aliases: ncc

### ** Examples


 ncc(nhanes) # 13 complete cases 




cleanEx()
nameEx("nelsonaalen")
### * nelsonaalen

flush(stderr()); flush(stdout())

### Name: nelsonaalen
### Title: Cumulative hazard rate or Nelson-Aalen estimator
### Aliases: nelsonaalen hazard
### Keywords: misc

### ** Examples

require(MASS)

leuk$status <- 1  ## no censoring occurs in leuk data (MASS)
ch <- nelsonaalen(leuk, time, status)
plot(x = leuk$time, y = ch, ylab='Cumulative hazard', xlab='Time')

### See example on http://www.engineeredsoftware.com/lmar/pe_cum_hazard_function.htm
time <- c(43, 67, 92, 94, 149, rep(149,7))
status <- c(rep(1,5),rep(0,7))
eng <- data.frame(time, status)
ch <- nelsonaalen(eng, time, status)
plot(x = time, y = ch, ylab='Cumulative hazard', xlab='Time')





cleanEx()
nameEx("nhanes")
### * nhanes

flush(stderr()); flush(stdout())

### Name: nhanes
### Title: NHANES example - all variables numerical
### Aliases: nhanes
### Keywords: datasets

### ** Examples

 

imp <- mice(nhanes)     # create 5 imputed data sets
complete(imp)           # print the first imputed data set




cleanEx()
nameEx("nhanes2")
### * nhanes2

flush(stderr()); flush(stdout())

### Name: nhanes2
### Title: NHANES example - mixed numerical and discrete variables
### Aliases: nhanes2
### Keywords: datasets

### ** Examples



imp <- mice(nhanes2)     # create 5 imputed data sets
complete(imp)           # print the first imputed data set




cleanEx()
nameEx("nic")
### * nic

flush(stderr()); flush(stdout())

### Name: nic
### Title: Number of incomplete cases
### Aliases: nic

### ** Examples


 nic(nhanes) # the remaining 12 rows 
 nic(nhanes[,c("bmi","hyp")]) # number of cases with incomplete bmi and hyp




cleanEx()
nameEx("nimp")
### * nimp

flush(stderr()); flush(stdout())

### Name: nimp
### Title: Number of imputations per block
### Aliases: nimp

### ** Examples

where <- is.na(nhanes)

# standard FCS
nimp(where)

# user-defined blocks
nimp(where, blocks = name.blocks(list(c("bmi", "hyp"), "age", "chl")))



cleanEx()
nameEx("parlmice")
### * parlmice

flush(stderr()); flush(stdout())

### Name: parlmice
### Title: Wrapper function that runs MICE in parallel
### Aliases: parlmice

### ** Examples

# 150 imputations in dataset nhanes, performed by 3 cores  
## Not run: 
##D imp1 <- parlmice(data = nhanes, n.core = 3, n.imp.core = 50)
##D # Making use of arguments in mice. 
##D imp2 <- parlmice(data = nhanes, method = "norm.nob", m = 100)
##D imp2$method
##D fit <- with(imp2, lm(bmi ~ hyp))
##D pool(fit)
## End(Not run)




cleanEx()
nameEx("pattern")
### * pattern

flush(stderr()); flush(stdout())

### Name: pattern
### Title: Datasets with various missing data patterns
### Aliases: pattern pattern1 pattern2 pattern3 pattern4
### Keywords: datasets

### ** Examples

require(lattice)
require(MASS)

pattern4

data <- rbind(pattern1, pattern2, pattern3, pattern4)
mdpat <- cbind(expand.grid(rec = 8:1, pat = 1:4, var = 1:3), r=as.numeric(as.vector(is.na(data))))

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


fluxplot(pattern2)





cleanEx()
nameEx("pool")
### * pool

flush(stderr()); flush(stdout())

### Name: pool
### Title: Combine estimates by Rubin's rules
### Aliases: pool
### Keywords: htest

### ** Examples

# pool using the classic MICE workflow
imp <- mice(nhanes, maxit = 2, m = 2)
fit <- with(data = imp, exp = lm(bmi ~ hyp + chl))
summary(pool(fit))



cleanEx()
nameEx("pool.compare")
### * pool.compare

flush(stderr()); flush(stdout())

### Name: pool.compare
### Title: Compare two nested models fitted to imputed data
### Aliases: pool.compare
### Keywords: htest

### ** Examples


### To compare two linear models:
imp <- mice(nhanes2, seed = 51009, print = FALSE)
mi1 <- with(data = imp, expr = lm(bmi ~ age + hyp + chl))
mi0 <- with(data = imp, expr = lm(bmi ~ age + hyp))
pc  <- pool.compare(mi1, mi0)
pc$pvalue

### Comparison of two general linear models (logistic regression).
## Not run: 
##D imp  <- mice(boys, maxit = 2, print = FALSE)
##D fit1 <- with(imp, glm(gen > levels(gen)[1] ~ hgt + hc + reg, family = binomial))
##D fit0 <- with(imp, glm(gen > levels(gen)[1] ~ hgt + hc, family = binomial))
##D pool.compare(fit1, fit0, method = 'likelihood')$pvalue
##D 
##D # using factors
##D fit1 <- with(imp, glm(as.factor(gen > levels(gen)[1]) ~ hgt + hc + reg, family = binomial))
##D fit0 <- with(imp, glm(as.factor(gen > levels(gen)[1]) ~ hgt + hc, family = binomial))
##D pool.compare(fit1, fit0, method = 'likelihood')$pvalue
## End(Not run)



cleanEx()
nameEx("pool.r.squared")
### * pool.r.squared

flush(stderr()); flush(stdout())

### Name: pool.r.squared
### Title: Pooling: R squared
### Aliases: pool.r.squared
### Keywords: htest

### ** Examples



imp<-mice(nhanes)

fit<-lm.mids(chl~age+hyp+bmi,imp)
pool.r.squared(fit)
pool.r.squared(fit,adjusted=TRUE)

#fit<-lm.mids(chl~age+hyp+bmi,imp)
#
#> pool.r.squared(fit)
#          est     lo 95     hi 95       fmi
#R^2 0.5108041 0.1479687 0.7791927 0.3024413
#
#> pool.r.squared(fit,adjusted=TRUE)
#          est      lo 95    hi 95       fmi
#adj R^2 0.4398066 0.08251427 0.743172 0.3404165
#





cleanEx()
nameEx("pool.scalar")
### * pool.scalar

flush(stderr()); flush(stdout())

### Name: pool.scalar
### Title: Multiple imputation pooling: univariate version
### Aliases: pool.scalar
### Keywords: htest

### ** Examples



imp <- mice(nhanes)
m <- imp$m
Q <- rep(NA, m)
U <- rep(NA, m)
for (i in 1:m) {
   Q[i] <- mean(complete(imp, i)$bmi)
   U[i] <- var(complete(imp, i)$bmi) / nrow(nhanes)  # (standard error of estimate)^2
}
pool.scalar(Q, U, n = nrow(nhanes), k = 1)  # Barnard-Rubin 1999




cleanEx()
nameEx("popmis")
### * popmis

flush(stderr()); flush(stdout())

### Name: popmis
### Title: Hox pupil popularity data with missing popularity scores
### Aliases: popmis
### Keywords: datasets

### ** Examples


popmis[1:3,]




cleanEx()
nameEx("pops")
### * pops

flush(stderr()); flush(stdout())

### Name: pops
### Title: Project on preterm and small for gestational age infants (POPS)
### Aliases: pops pops.pred
### Keywords: datasets

### ** Examples



pops <- data(pops)




cleanEx()
nameEx("potthoffroy")
### * potthoffroy

flush(stderr()); flush(stdout())

### Name: potthoffroy
### Title: Potthoff-Roy data
### Aliases: potthoffroy
### Keywords: datasets

### ** Examples



### create missing values at age 10 as in Little and Rubin (1987)

phr <- potthoffroy
idmis <- c(3,6,9,10,13,16,23,24,27)
phr[idmis, 4] <- NA
phr

md.pattern(phr)





cleanEx()
nameEx("quickpred")
### * quickpred

flush(stderr()); flush(stdout())

### Name: quickpred
### Title: Quick selection of predictors from the data
### Aliases: quickpred
### Keywords: misc

### ** Examples



# default: include all predictors with absolute correlation over 0.1
quickpred(nhanes)

# all predictors with absolute correlation over 0.4
quickpred(nhanes, mincor=0.4)

# include age and bmi, exclude chl
quickpred(nhanes, mincor=0.4, inc=c('age','bmi'), exc='chl')

# only include predictors with at least 30% usable cases
quickpred(nhanes, minpuc=0.3)

# use low threshold for bmi, and high thresholds for hyp and chl
pred <- quickpred(nhanes, mincor=c(0,0.1,0.5,0.5))
pred

# use it directly from mice
imp <- mice(nhanes, pred=quickpred(nhanes, minpuc=0.25, include='age'))




cleanEx()
nameEx("rbind.mids")
### * rbind.mids

flush(stderr()); flush(stdout())

### Name: rbind.mids
### Title: Combine 'mids' objects by rows
### Aliases: rbind.mids
### Keywords: manip

### ** Examples

imp1 <- mice(nhanes[1:13, ], m = 2, maxit = 1, print = FALSE)
imp5 <- mice(nhanes[1:13, ], m = 2, maxit = 2, print = FALSE)
mylist <- list(age = NA, bmi = NA, hyp = NA, chl = NA)

nrow(complete(rbind(imp1, imp5)))
nrow(complete(rbind(imp1, mylist)))

nrow(complete(rbind(imp1, data.frame(mylist))))
nrow(complete(rbind(imp1, complete(imp5))))



cleanEx()
nameEx("selfreport")
### * selfreport

flush(stderr()); flush(stdout())

### Name: selfreport
### Title: Self-reported and measured BMI
### Aliases: selfreport mgg
### Keywords: datasets

### ** Examples



md.pattern(selfreport[,c("age","sex","hm","hr","wm","wr")])

### FIMD Section 7.3.5 Application

bmi <- function(h,w){return(w/(h/100)^2)}
init <- mice(selfreport,maxit=0)
meth <- init$meth
meth["bm"] <- "~bmi(hm,wm)"
pred <- init$pred
pred[,c("src","id","web","bm","br")] <- 0
imp <- mice(selfreport, pred=pred, meth=meth, seed=66573, maxit=2, m=1)
## imp <- mice(selfreport, pred=pred, meth=meth, seed=66573, maxit=20, m=10)

### Like FIMD Figure 7.6 

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





cleanEx()
nameEx("stripplot.mids")
### * stripplot.mids

flush(stderr()); flush(stdout())

### Name: stripplot.mids
### Title: Stripplot of observed and imputed data
### Aliases: stripplot.mids stripplot
### Keywords: hplot

### ** Examples

imp <- mice(boys, maxit=1)

### stripplot, all numerical variables
## Not run: stripplot(imp)

### same, but with improved display
## Not run: stripplot(imp, col=c("grey",mdc(2)),pch=c(1,20))

### distribution per imputation of height, weight and bmi
### labeled by their own missingness
## Not run: 
##D stripplot(imp, hgt+wgt+bmi~.imp, cex=c(2,4), pch=c(1,20),jitter=FALSE,
##D layout=c(3,1))
## End(Not run)

### same, but labeled with the missingness of wgt (just four cases)
## Not run: 
##D stripplot(imp, hgt+wgt+bmi~.imp, na=wgt, cex=c(2,4), pch=c(1,20),jitter=FALSE,
##D layout=c(3,1))
## End(Not run)

### distribution of age and height, labeled by missingness in height
### most height values are missing for those around 
### the age of two years
### some additional missings occur in region WEST
## Not run: stripplot(imp, age+hgt~.imp|reg, hgt, col=c(hcl(0,0,40,0.2), mdc(2)),pch=c(1,20))

### heavily jitted relation between two categorical variables
### labeled by missingness of gen
### aggregated over all imputed data sets
## Not run: stripplot(imp, gen~phb, factor=2, cex=c(8,1), hor=TRUE)

### circle fun
stripplot(imp, gen~.imp, na = wgt, factor = 2, cex = c(8.6), 
         hor = FALSE, outer = TRUE, scales = "free", pch = c(1,19))




cleanEx()
nameEx("supports.transparent")
### * supports.transparent

flush(stderr()); flush(stdout())

### Name: supports.transparent
### Title: Supports semi-transparent foreground colors?
### Aliases: supports.transparent transparent
### Keywords: hplot

### ** Examples


supports.transparent()




cleanEx()
nameEx("tbc")
### * tbc

flush(stderr()); flush(stdout())

### Name: tbc
### Title: Terneuzen birth cohort
### Aliases: tbc tbc.target terneuzen
### Keywords: datasets

### ** Examples


data <- tbc
md.pattern(data)




cleanEx()
nameEx("version")
### * version

flush(stderr()); flush(stdout())

### Name: version
### Title: Echoes the package version number
### Aliases: version
### Keywords: misc

### ** Examples


version()
version("base")




cleanEx()
nameEx("walking")
### * walking

flush(stderr()); flush(stdout())

### Name: walking
### Title: Walking disability data
### Aliases: walking
### Keywords: datasets

### ** Examples


md.pattern(walking)

micemill <- function(n) {
 for (i in 1:n) {
   imp <<- mice.mids(imp) # global assignment 
   cors <- with(imp, cor(as.numeric(YA),
                        as.numeric(YB),
                        method="kendall"))
   tau <<- rbind(tau, getfit(cors, s=TRUE))  # global assignment
 }
}

plotit <- function()
 matplot(x=1:nrow(tau),y=tau,
       ylab=expression(paste("Kendall's ",tau)), 
         xlab="Iteration", type="l", lwd=1,
       lty=1:10,col="black")

tau <- NULL
imp <- mice(walking, max=0, m=10, seed=92786)
pred <- imp$pred
pred[,c("src","age","sex")] <- 0
imp <- mice(walking, max=0, m=3, seed=92786, pred=pred)
micemill(5)
plotit()

### to get figure 7.8 van Buuren (2012) use m=10 and micemill(20) 




cleanEx()
nameEx("windspeed")
### * windspeed

flush(stderr()); flush(stdout())

### Name: windspeed
### Title: Subset of Irish wind speed data
### Aliases: windspeed
### Keywords: datasets

### ** Examples


windspeed[1:3,]




cleanEx()
nameEx("with.mids")
### * with.mids

flush(stderr()); flush(stdout())

### Name: with.mids
### Title: Evaluate an expression in multiple imputed datasets
### Aliases: with.mids
### Keywords: multivariate

### ** Examples



imp <- mice(nhanes2)
fit1 <- with(data=imp,exp=lm(bmi~age+hyp+chl))
fit2 <- with(data=imp,exp=glm(hyp~age+bmi+chl,family=binomial))
anova.imp <- with(data=imp,exp=anova(lm(bmi~age+hyp+chl)))



cleanEx()
nameEx("xyplot.mids")
### * xyplot.mids

flush(stderr()); flush(stdout())

### Name: xyplot.mids
### Title: Scatterplot of observed and imputed data
### Aliases: xyplot.mids xyplot
### Keywords: hplot

### ** Examples

imp <- mice(boys, maxit=1)

### xyplot: scatterplot by imputation number
### observe the erroneous outlying imputed values
### (caused by imputing hgt from bmi)
xyplot(imp, hgt~age|.imp, pch=c(1,20),cex=c(1,1.5))

### same, but label with missingness of wgt (four cases)
xyplot(imp, hgt~age|.imp, na.group=wgt, pch=c(1,20),cex=c(1,1.5))




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
