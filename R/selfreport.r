#'Self-reported and measured BMI
#'
#'Dataset containing height and weight data (measured, self-reported) from two
#'studies.
#'
#'This dataset combines two datasets: \code{krul} data (Krul, 2010) (1257
#'persons) and the \code{mgg} data (Van Keulen 2011; Van der Klauw 2011) (803
#'persons). The \code{krul} dataset contains height and weight (both measures
#'and self-reported) from 1257 Dutch adults, whereas the \code{mgg} dataset
#'contains self-reported height and weight for 803 Dutch adults. Section 7.3 in
#'Van Buuren (2012) shows how the missing measured data can be imputed in the
#'\code{mgg} data, so corrected prevalence estimates can be calculated.
#'
#'@name selfreport
#'@aliases selfreport mgg
#'@docType data
#'@format A data frame with 2060 rows and 15 variables: 
#'\describe{
#'\item{src}{Study, either \code{krul} or \code{mgg} (factor)}
#'\item{id}{Person identification number}
#'\item{pop}{Population, all \code{NL} (factor)}
#'\item{age}{Age of respondent in years}
#'\item{sex}{Sex of respondent (factor)}
#'\item{hm}{Height measured (cm)}
#'\item{wm}{Weight measured (kg)}
#'\item{hr}{Height reported (cm)}
#'\item{wr}{Weight reported (kg)}
#'\item{prg}{Pregnancy (factor), all \code{Not pregnant}} 
#'\item{edu}{Educational level (factor)}
#'\item{etn}{Ethnicity (factor)}
#'\item{web}{Obtained through web survey (factor)}
#'\item{bm}{BMI measured (kg/m2)}
#'\item{br}{BMI reported (kg/m2)} 
#'}
#'@source Krul, A., Daanen, H. A. M., Choi, H. (2010). Self-reported and
#'measured weight, height and body mass index (BMI) in Italy, The Netherlands
#'and North America. \emph{European Journal of Public Health}, \emph{21}(4),
#'414-419.
#'
#'Van Keulen, H.M.,, Chorus, A.M.J., Verheijden, M.W. (2011).  \emph{Monitor
#'Convenant Gezond Gewicht Nulmeting (determinanten van) beweeg- en eetgedrag
#'van kinderen (4-11 jaar), jongeren (12-17 jaar) en volwassenen (18+ jaar)}.
#'TNO/LS 2011.016. Leiden: TNO.
#'
#'Van der Klauw, M., Van Keulen, H.M., Verheijden, M.W. (2011).  \emph{Monitor
#'Convenant Gezond Gewicht Beweeg- en eetgedrag van kinderen (4-11 jaar),
#'jongeren (12-17 jaar) en volwassenen (18+ jaar) in 2010 en 2011.} TNO/LS
#'2011.055. Leiden: TNO. (in Dutch)
#'
#'van Buuren, S. (2012). \emph{Flexible Imputation of Missing Data.} Boca
#'Raton, FL: Chapman & Hall/CRC Press.
#'@keywords datasets
#'@examples
#'
#'
#'md.pattern(selfreport[,c("age","sex","hm","hr","wm","wr")])
#'
#'### FIMD Section 7.3.5 Application
#'
#'bmi <- function(h,w){return(w/(h/100)^2)}
#'init <- mice(selfreport,maxit=0)
#'meth <- init$meth
#'meth["bm"] <- "~bmi(hm,wm)"
#'pred <- init$pred
#'pred[,c("src","id","web","bm","br")] <- 0
#'imp <- mice(selfreport, pred=pred, meth=meth, seed=66573, maxit=2, m=1)
#'## imp <- mice(selfreport, pred=pred, meth=meth, seed=66573, maxit=20, m=10)
#'
#'### Like FIMD Figure 7.6 
#'
#'cd <- complete(imp, 1)
#'xy <- xy.coords(cd$bm, cd$br-cd$bm)
#'plot(xy,col=mdc(2),xlab="Measured BMI",ylab="Reported - Measured BMI",
#'     xlim=c(17,45),ylim=c(-5,5), type="n",lwd=0.7)
#'polygon(x=c(30,20,30),y=c(0,10,10),col="grey95",border=NA) 
#'polygon(x=c(30,40,30),y=c(0,-10,-10),col="grey95",border=NA)
#'abline(0,0,lty=2,lwd=0.7)
#'
#'idx <- cd$src=="krul"
#'xyc <- xy; xyc$x <- xy$x[idx]; xyc$y <- xy$y[idx]
#'xys <- xy; xys$x <- xy$x[!idx]; xys$y <- xy$y[!idx]
#'points(xyc,col=mdc(1), cex=0.7)
#'points(xys,col=mdc(2), cex=0.7)
#'lines(lowess(xyc),col=mdc(4),lwd=2)
#'lines(lowess(xys),col=mdc(5),lwd=2)
#'text(1:4,x=c(40,28,20,32),y=c(4,4,-4,-4),cex=3)
#'box(lwd=1)
#'
#'
NULL
