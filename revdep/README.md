# Platform

|field    |value                                      |
|:--------|:------------------------------------------|
|version  |R version 4.4.2 (2024-10-31)               |
|os       |macOS Sequoia 15.1.1                       |
|system   |aarch64, darwin20                          |
|ui       |RStudio                                    |
|language |(EN)                                       |
|collate  |en_US.UTF-8                                |
|ctype    |en_US.UTF-8                                |
|tz       |Europe/Amsterdam                           |
|date     |2024-11-22                                 |
|rstudio  |2024.09.1+394 Cranberry Hibiscus (desktop) |
|pandoc   |1.17.1 @ /usr/local/bin/pandoc             |

# Dependencies

|package     |old         |new         |Δ  |
|:-----------|:-----------|:-----------|:--|
|mice        |3.16.0      |NA          |*  |
|backports   |1.5.0       |1.5.0       |   |
|bit         |4.5.0       |4.5.0       |   |
|bit64       |4.5.2       |4.5.2       |   |
|broom       |1.0.7       |1.0.7       |   |
|cli         |3.6.3       |3.6.3       |   |
|clipr       |0.8.0       |0.8.0       |   |
|cpp11       |0.5.0       |0.5.0       |   |
|crayon      |1.5.3       |1.5.3       |   |
|dplyr       |1.1.4       |1.1.4       |   |
|fansi       |1.0.6       |1.0.6       |   |
|forcats     |1.0.0       |1.0.0       |   |
|foreach     |1.5.2       |1.5.2       |   |
|generics    |0.1.3       |0.1.3       |   |
|glmnet      |4.1-8       |4.1-8       |   |
|glue        |1.8.0       |1.8.0       |   |
|haven       |2.5.4       |2.5.4       |   |
|hms         |1.1.3       |1.1.3       |   |
|iterators   |1.0.14      |1.0.14      |   |
|jomo        |2.7-6       |2.7-6       |   |
|lifecycle   |1.0.4       |1.0.4       |   |
|lme4        |1.1-35.5    |1.1-35.5    |   |
|magrittr    |2.0.3       |2.0.3       |   |
|minqa       |1.2.8       |1.2.8       |   |
|mitml       |0.4-5       |0.4-5       |   |
|nloptr      |2.1.1       |2.1.1       |   |
|numDeriv    |2016.8-1.1  |2016.8-1.1  |   |
|ordinal     |2023.12-4.1 |2023.12-4.1 |   |
|pan         |1.9         |1.9         |   |
|pillar      |1.9.0       |1.9.0       |   |
|pkgconfig   |2.0.3       |2.0.3       |   |
|prettyunits |1.2.0       |1.2.0       |   |
|progress    |1.2.3       |1.2.3       |   |
|purrr       |1.0.2       |1.0.2       |   |
|R6          |2.5.1       |2.5.1       |   |
|Rcpp        |1.0.13-1    |1.0.13-1    |   |
|RcppEigen   |0.3.4.0.2   |0.3.4.0.2   |   |
|readr       |2.1.5       |2.1.5       |   |
|rlang       |1.1.4       |1.1.4       |   |
|shape       |1.4.6.1     |1.4.6.1     |   |
|stringi     |1.8.4       |1.8.4       |   |
|stringr     |1.5.1       |1.5.1       |   |
|tibble      |3.2.1       |3.2.1       |   |
|tidyr       |1.3.1       |1.3.1       |   |
|tidyselect  |1.2.1       |1.2.1       |   |
|tzdb        |0.4.0       |0.4.0       |   |
|ucminf      |1.2.2       |1.2.2       |   |
|utf8        |1.2.4       |1.2.4       |   |
|vctrs       |0.6.5       |0.6.5       |   |
|vroom       |1.6.5       |1.6.5       |   |
|withr       |3.0.2       |3.0.2       |   |

# Revdeps

## Failed to check (3)

|package |version |error  |warning |note   |
|:-------|:-------|:------|:-------|:------|
|brms    |2.22.0  |1      |        |2      |
|[MKinfer](failures.md#mkinfer)|1.2     |__+1__ |        |__+1__ |
|rmsb    |1.1-1   |1      |        |1      |

## New problems (124)

|package            |version    |error     |warning |note      |
|:------------------|:----------|:---------|:-------|:---------|
|[accelmissing](problems.md#accelmissing)|1.4        |__+1__    |        |-1        |
|[adjustedCurves](problems.md#adjustedcurves)|0.11.2     |__+1__    |        |__+2__    |
|[alookr](problems.md#alookr)|0.3.9      |          |        |__+1__    |
|[autoReg](problems.md#autoreg)|0.3.3      |__+1__    |        |          |
|[BaM](problems.md#bam)|1.0.3      |__+1__    |        |          |
|[basecamb](problems.md#basecamb)|1.1.5      |__+1__    |        |          |
|[betaMC](problems.md#betamc)|1.3.2      |__+2__    |        |__+2__    |
|[BGGM](problems.md#bggm)|2.1.3      |          |__+1__  |1 __+2__  |
|[biokNN](problems.md#bioknn)|0.1.0      |__+1__    |        |-1        |
|[bipd](problems.md#bipd)|0.3        |          |1       |-1 __+1__ |
|[bootImpute](problems.md#bootimpute)|1.2.1      |__+1__    |        |          |
|[brokenstick](problems.md#brokenstick)|2.5.0      |          |        |__+2__    |
|[broom.helpers](problems.md#broomhelpers)|1.17.0     |          |        |__+1__    |
|[CALIBERrfimpute](problems.md#caliberrfimpute)|1.0-7      |__+1__    |        |          |
|[cati](problems.md#cati)|0.99.4     |          |        |__+1__    |
|[censcyt](problems.md#censcyt)|1.14.0     |__+1__    |        |-3        |
|[CIMPLE](problems.md#cimple)|0.1.0      |__+1__    |        |          |
|[ClustAll](problems.md#clustall)|1.2.0      |__+1__    |        |          |
|[clusterMI](problems.md#clustermi)|1.2.2      |__+1__    |        |          |
|[cmahalanobis](problems.md#cmahalanobis)|0.4.2      |__+1__    |        |          |
|[cobalt](problems.md#cobalt)|4.5.5      |          |__+1__  |__+1__    |
|[dlookr](problems.md#dlookr)|0.6.3      |__+1__    |        |          |
|[dynamite](problems.md#dynamite)|1.5.5      |          |        |2 __+1__  |
|[dynr](problems.md#dynr)|0.1.16-105 |-1 __+1__ |        |          |
|[eatRep](problems.md#eatrep)|0.14.7     |__+1__    |        |          |
|[finalfit](problems.md#finalfit)|1.0.8      |__+1__    |        |-1        |
|[FLAME](problems.md#flame)|2.1.1      |__+2__    |        |__+1__    |
|[flevr](problems.md#flevr)|0.0.4      |__+1__    |        |__+1__    |
|[gerbil](problems.md#gerbil)|0.1.9      |          |        |__+1__    |
|[gFormulaMI](problems.md#gformulami)|1.0.0      |__+1__    |        |          |
|[ggeffects](problems.md#ggeffects)|1.7.2      |          |        |__+2__    |
|[ggmice](problems.md#ggmice)|0.1.0      |__+1__    |        |          |
|[gtsummary](problems.md#gtsummary)|2.0.3      |          |        |__+1__    |
|[HardyWeinberg](problems.md#hardyweinberg)|1.7.8      |__+1__    |        |          |
|[hhsmm](problems.md#hhsmm)|0.4.2      |__+1__    |        |          |
|[Hmisc](problems.md#hmisc)|5.2-0      |          |        |1 __+2__  |
|[holodeck](problems.md#holodeck)|0.2.2      |          |__+1__  |__+1__    |
|[hot.deck](problems.md#hotdeck)|1.2        |__+1__    |        |-1        |
|[howManyImputations](problems.md#howmanyimputations)|0.2.5      |__+1__    |        |          |
|[HSAUR3](problems.md#hsaur3)|1.0-15     |          |__+1__  |__+1__    |
|[idem](problems.md#idem)|5.2        |__+1__    |        |-2        |
|[ImputeRobust](problems.md#imputerobust)|1.3-1      |__+1__    |        |          |
|[insight](problems.md#insight)|0.20.5     |          |        |__+1__    |
|[intmed](problems.md#intmed)|0.1.2      |__+1__    |        |          |
|[IPWboxplot](problems.md#ipwboxplot)|0.1.2      |__+1__    |__+1__  |__+1__    |
|[JWileymisc](problems.md#jwileymisc)|1.4.1      |__+1__    |        |          |
|[konfound](problems.md#konfound)|1.0.2      |          |        |__+1__    |
|[lavaan.survey](problems.md#lavaansurvey)|1.1.3.1    |1         |        |__+1__    |
|[LMMstar](problems.md#lmmstar)|1.1.0      |__+1__    |        |1 __+1__  |
|[logistf](problems.md#logistf)|1.26.0     |__+1__    |        |          |
|[manymome](problems.md#manymome)|0.2.4      |          |        |__+1__    |
|[marginaleffects](problems.md#marginaleffects)|0.23.0     |          |        |__+1__    |
|[MatchThem](problems.md#matchthem)|1.2.1      |__+1__    |        |          |
|[mdapack](problems.md#mdapack)|0.0.2      |__+1__    |        |-2        |
|[medflex](problems.md#medflex)|0.6-10     |          |        |__+1__    |
|[metavcov](problems.md#metavcov)|2.1.5      |          |        |__+1__    |
|[mi4p](problems.md#mi4p)|1.2        |__+1__    |        |          |
|[micd](problems.md#micd)|1.1.1      |__+1__    |        |          |
|[miceadds](problems.md#miceadds)|3.17-44    |__+1__    |        |-2        |
|[miceafter](problems.md#miceafter)|0.5.0      |__+1__    |        |          |
|[miceFast](problems.md#micefast)|0.8.2      |          |__+1__  |2 __+1__  |
|[micemd](problems.md#micemd)|1.10.0     |__+1__    |        |          |
|[microeco](problems.md#microeco)|1.10.0     |          |        |__+1__    |
|[midastouch](problems.md#midastouch)|1.3        |__+1__    |        |1 __+1__  |
|[midoc](problems.md#midoc)|1.0.0      |__+1__    |        |          |
|[mifa](problems.md#mifa)|0.2.0      |__+1__    |        |-1        |
|[MIIPW](problems.md#miipw)|0.1.1      |__+1__    |        |          |
|[misaem](problems.md#misaem)|1.0.1      |          |__+1__  |__+1__    |
|[miselect](problems.md#miselect)|0.9.2      |          |__+1__  |__+1__    |
|[missCompare](problems.md#misscompare)|1.0.3      |__+1__    |        |          |
|[missDiag](problems.md#missdiag)|1.0.1      |__+1__    |        |__+2__    |
|[missMDA](problems.md#missmda)|1.19       |__+1__    |        |          |
|[misty](problems.md#misty)|0.6.8      |          |        |__+1__    |
|[mitml](problems.md#mitml)|0.4-5      |__+1__    |        |__+2__    |
|[miWQS](problems.md#miwqs)|0.4.4      |          |        |__+2__    |
|[mixgb](problems.md#mixgb)|1.0.2      |__+1__    |        |          |
|[MixtureMissing](problems.md#mixturemissing)|3.0.3      |__+1__    |        |          |
|[mlim](problems.md#mlim)|0.3.0      |__+1__    |        |-1        |
|[modelsummary](problems.md#modelsummary)|2.2.0      |          |        |__+1__    |
|[monoClust](problems.md#monoclust)|1.2.1      |          |        |1 __+1__  |
|[MRPC](problems.md#mrpc)|3.1.0      |__+1__    |        |          |
|[MSiP](problems.md#msip)|1.3.7      |__+1__    |        |-1        |
|[mvnimpute](problems.md#mvnimpute)|1.0.1      |          |        |__+1__    |
|[mvs](problems.md#mvs)|2.0.0      |__+1__    |        |__+1__    |
|[NADIA](problems.md#nadia)|0.4.2      |-3 __+1__ |        |-2        |
|[NIMAA](problems.md#nimaa)|0.2.1      |__+1__    |        |-2        |
|[nncc](problems.md#nncc)|2.0.0      |          |__+1__  |__+1__    |
|[ordbetareg](problems.md#ordbetareg)|0.7.2      |          |__+1__  |2 __+1__  |
|[OTrecod](problems.md#otrecod)|0.1.2      |__+1__    |        |-1        |
|[parameters](problems.md#parameters)|0.23.0     |          |        |__+1__    |
|[pema](problems.md#pema)|0.1.3      |1 __+1__  |        |2 __+2__  |
|[pguIMP](problems.md#pguimp)|0.0.0.3    |-1 __+1__ |        |          |
|[pre](problems.md#pre)|1.0.7      |          |__+1__  |__+1__    |
|[psfmi](problems.md#psfmi)|1.4.0      |__+1__    |        |          |
|[qgcomp](problems.md#qgcomp)|2.15.2     |          |        |1 __+2__  |
|[Qtools](problems.md#qtools)|1.5.9      |          |        |__+1__    |
|[RBtest](problems.md#rbtest)|1.1        |__+1__    |        |-1        |
|[realTimeloads](problems.md#realtimeloads)|1.0.0      |-1 __+1__ |        |          |
|[RefBasedMI](problems.md#refbasedmi)|0.2.0      |__+1__    |        |          |
|[regmedint](problems.md#regmedint)|1.0.1      |          |__+1__  |1 __+1__  |
|[RegularizedSCA](problems.md#regularizedsca)|0.5.4      |__+1__    |        |-1        |
|[Replication](problems.md#replication)|0.1.2      |-1 __+1__ |        |          |
|[rexposome](problems.md#rexposome)|1.28.0     |__+1__    |-5      |-4        |
|[RfEmpImp](problems.md#rfempimp)|2.1.8      |__+1__    |        |          |
|[rms](problems.md#rms)|6.8-2      |          |        |__+1__    |
|[RNAseqCovarImpute](problems.md#rnaseqcovarimpute)|1.4.0      |__+1__    |        |-2        |
|[rqlm](problems.md#rqlm)|2.1-1      |__+1__    |        |          |
|[RSquaredMI](problems.md#rsquaredmi)|0.1.1      |__+1__    |        |          |
|[semmcci](problems.md#semmcci)|1.1.4      |__+1__    |        |          |
|[semTools](problems.md#semtools)|0.5-6      |__+1__    |        |1 __+2__  |
|[seqimpute](problems.md#seqimpute)|2.1.0      |__+1__    |        |          |
|[shapeNA](problems.md#shapena)|0.0.2      |__+1__    |        |3 __+1__  |
|[sjmisc](problems.md#sjmisc)|2.8.10     |          |        |__+2__    |
|[smdi](problems.md#smdi)|0.3.1      |-1 __+1__ |        |          |
|[sociome](problems.md#sociome)|2.2.5      |__+1__    |        |          |
|[StackImpute](problems.md#stackimpute)|0.1.0      |__+1__    |        |-2        |
|[superMICE](problems.md#supermice)|1.1.1      |__+1__    |        |-1        |
|[svyweight](problems.md#svyweight)|0.1.0      |          |        |1 __+2__  |
|[SynDI](problems.md#syndi)|0.1.0      |__+1__    |        |-2        |
|[synergyfinder](problems.md#synergyfinder)|3.14.0     |__+1__    |-1      |-3        |
|[TestDataImputation](problems.md#testdataimputation)|2.3        |__+1__    |        |          |
|[tidySEM](problems.md#tidysem)|0.2.7      |__+2__    |        |__+1__    |
|[vsmi](problems.md#vsmi)|0.1.0      |__+1__    |        |          |
|[weights](problems.md#weights)|1.0.4      |__+1__    |        |          |

