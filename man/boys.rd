\name{boys}
\alias{boys}
\docType{data}
\title{Growth of Dutch boys}
\description{
Height, weight, head circumference and puberty of 748 Dutch boys.}
\usage{data(boys)}
\format{
  A data frame with 748 rows on the following 9 variables:
  \describe{
    \item{\code{age}}{Decimal age (0-21 years)}
    \item{\code{hgt}}{Height (cm)}
    \item{\code{wgt}}{Weight (kg)}
    \item{\code{bmi}}{Body mass index}
    \item{\code{hc}}{Head circumference (cm)}
    \item{\code{gen}}{Genital Tanner stage (G1-G5)}
    \item{\code{phb}}{Pubic hair (Tanner P1-P6)}
    \item{\code{tv}}{Testicular volume (ml)}
    \item{\code{reg}}{Region (north, east, west, south, city)}
  }
}
\details{
Random sample of 10\% from the cross-sectional data used to construct the Dutch growth references 1997.
Variables \code{gen} and \code{phb} are ordered factors. \code{reg} is a factor.
}


\source{
Fredriks, A.M,, van Buuren, S., Burgmeijer, R.J., Meulmeester JF, Beuker, R.J., Brugman, E., Roede, M.J., Verloove-Vanhorick, S.P., Wit, J.M. (2000) 
Continuing positive secular growth change in The Netherlands 1955-1997. 
\emph{Pediatric Research}, \bold{47}, 316-323.
\url{http://www.stefvanbuuren.nl/publications/Continuing secular - Ped Res 2000.pdf}

Fredriks, A.M., van Buuren, S., Wit, J.M., Verloove-Vanhorick, S.P. (2000). 
Body index measurements in 1996-7 compared with 1980. 
\emph{Archives of Disease in Childhood}, \bold{82}, 107-112.
\url{http://www.stefvanbuuren.nl/publications/Body index - ADC 2000.pdf}
}

\examples{

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

}
\keyword{datasets}

