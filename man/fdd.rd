\name{fdd}
\alias{fdd}
\alias{fdd.pred}
\alias{fireworks}
\docType{data}
\title{SE Fireworks Disaster Data}
\description{Multiple outcomes of a randomized study to reduce post-traumatic stress.}
\usage{
data(fdd)
data(fdd.pred)
}
\format{
  \code{fdd} is a data frame with 52 rows and 65 columns:
  \describe{
 \item{\code{id}}{Client number}    
 \item{\code{trt}}{Treatment (E=EMDR, C=CBT)}
 \item{\code{pp}}{Per protocol (Y/N)}
 \item{\code{trtp}}{Number of parental treatments}
 \item{\code{sex}}{Sex: M/F}
 \item{\code{etn}}{Ethnicity: NL/OTHER}
 \item{\code{age}}{Age (years)}
 \item{\code{trauma}}{Trauma count (1-5)}
 \item{\code{prop1}}{PROPS total score T1}
 \item{\code{prop2}}{PROPS total score T2}
 \item{\code{prop3}}{PROPS total score T3}
 \item{\code{crop1}}{CROPS total score T1}
 \item{\code{crop2}}{CROPS total score T2}
 \item{\code{crop3}}{CROPS total score T3}
 \item{\code{masc1}}{MASC score T1}
 \item{\code{masc2}}{MASC score T2}
 \item{\code{masc3}}{MASC score T3}
 \item{\code{cbcl1}}{CBCL T1}
 \item{\code{cbcl3}}{CBCL T3}
 \item{\code{prs1}}{PRS total score T1}
 \item{\code{prs2}}{PRS total score T2}
 \item{\code{prs3}}{PRS total score T3}
 \item{\code{ypa1}}{PTSD-RI B intrusive recollection parent T1}
 \item{\code{ypb1}}{PTSD-RI C avoidant/numbing parent T1}
 \item{\code{ypc1}}{PTSD-RI D hyper-arousal parent T1}
 \item{\code{yp1}}{PTSD-RI B+C+D parent T1}
 \item{\code{ypa2}}{PTSD-RI B intrusive recollection parent T2}
 \item{\code{ypb2}}{PTSD-RI C avoidant/numbing parent T2}
 \item{\code{ypc2}}{PTSD-RI D hyper-arousal parent T2}
 \item{\code{yp2}}{PTSD-RI B+C+D parent T1}
 \item{\code{ypa3}}{PTSD-RI B intrusive recollection parent T3}
 \item{\code{ypb3}}{PTSD-RI C avoidant/numbing parent T3}
 \item{\code{ypc3}}{PTSD-RI D hyper-arousal parent T3}
 \item{\code{yp3}}{PTSD-RI B+C+D parent T3}
 \item{\code{yca1}}{PTSD-RI B intrusive recollection child T1}
 \item{\code{ycb1}}{PTSD-RI C avoidant/numbing child T1}
 \item{\code{ycc1}}{PTSD-RI D hyper-arousal child T1}
 \item{\code{yc1}}{PTSD-RI B+C+D child T1}
 \item{\code{yca2}}{PTSD-RI B intrusive recollection child T2}
 \item{\code{ycb2}}{PTSD-RI C avoidant/numbing child T2}
 \item{\code{ycc2}}{PTSD-RI D hyper-arousal child T2}
 \item{\code{yc2}}{PTSD-RI B+C+D child T2}
 \item{\code{yca3}}{PTSD-RI B intrusive recollection child T3}
 \item{\code{ycb3}}{PTSD-RI C avoidant/numbing child T3}
 \item{\code{ycc3}}{PTSD-RI D hyper-arousal child T3}
 \item{\code{yc3}}{PTSD-RI B+C+D child T3}
 \item{\code{ypf1}}{PTSD-RI parent full T1}
 \item{\code{ypf2}}{PTSD-RI parent full T2}
 \item{\code{ypf3}}{PTSD-RI parent full T3}
 \item{\code{ypp1}}{PTSD parent partial T1}
 \item{\code{ypp2}}{PTSD parent partial T2}
 \item{\code{ypp3}}{PTSD parent partial T3}
 \item{\code{ycf1}}{PTSD child full T1}
 \item{\code{ycf2}}{PTSD child full T2}
 \item{\code{ycf3}}{PTSD child full T3}
 \item{\code{ycp1}}{PTSD child partial T1}
 \item{\code{ycp2}}{PTSD child partial T2}
 \item{\code{ycp3}}{PTSD child partial T3}
 \item{\code{cbin1}}{CBCL Internalizing T1}
 \item{\code{cbin3}}{CBCL Internalizing T3}
 \item{\code{cbex1}}{CBCL Externalizing T1}
 \item{\code{cbex3}}{CBCL Externalizing T3}
 \item{\code{bir1}}{Birlison T1}
 \item{\code{bir2}}{Birlison T2}
 \item{\code{bir3}}{Birlison T3}
  }
  \code{fdd.pred} is the 65 by 65 binary predictor matrix used to impute \code{fdd}.
}

\details{
Data from a randomized experiment to reduce post-traumatic stress by two treatments: 
Eye Movement Desensitization and Reprocessing (EMDR) (experimental treatment), and 
cognitive behavioral therapy (CBT) (control treatment). 52 children were randomized to 
one of these two treatments. Outcomes were measured at three time points: 
at baseline (pre-treatment, T1), post-treatment (T2, 4-8 weeks), and at follow-up
(T3, 3 months). For more details, see de Roos et al (2011). 
Some person covariates were reshuffled.
The imputation methodology is explained in Chapter 9 of van Buuren (2012).
}


\source{
de Roos, C., Greenwald, R., den Hollander-Gijsman, M., Noorthoorn, E., van Buuren, S., de Jong, A. (2011). A Randomised Comparison of Cognitive Behavioral Therapy (CBT) and Eye Movement Desensitisation and Reprocessing (EMDR) in disaster-exposed children. \emph{European Journal of Psychotraumatology}, \emph{2}, 5694. 
\url{http://www.stefvanbuuren.nl/publications/2011 EMDR and CBT  - EJP.pdf}

van Buuren, S. (2012). \emph{Flexible Imputation of Missing Data.} Boca Raton, FL: Chapman \& Hall/CRC Press. 

}

\examples{

data <- fdd
md.pattern(fdd)

}

\keyword{datasets}

