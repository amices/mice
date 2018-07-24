# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.5.1 (2018-07-02) |
|system   |x86_64, darwin15.6.0         |
|ui       |RStudio (1.1.456)            |
|language |(EN)                         |
|collate  |nl_NL.UTF-8                  |
|tz       |Europe/Amsterdam             |
|date     |2018-07-24                   |

## Packages

|package         |*  |version |date       |source          |
|:---------------|:--|:-------|:----------|:---------------|
|AGD             |   |0.39    |2018-05-29 |cran (@0.39)    |
|broom           |   |0.5.0   |2018-07-17 |cran (@0.5.0)   |
|BSDA            |   |1.2.0   |2017-07-30 |cran (@1.2.0)   |
|CALIBERrfimpute |   |1.0-1   |2018-06-11 |cran (@1.0-1)   |
|dplyr           |   |0.7.6   |2018-06-29 |cran (@0.7.6)   |
|DPpackage       |   |1.1-7.4 |2018-01-06 |cran (@1.1-7.4) |
|gamlss          |   |5.1-0   |2018-06-08 |cran (@5.1-0)   |
|HSAUR3          |   |1.0-9   |2018-05-28 |cran (@1.0-9)   |
|knitr           |   |1.20    |2018-02-20 |cran (@1.20)    |
|lme4            |   |1.1-17  |2018-04-03 |cran (@1.1-17)  |
|mice            |   |3.1.0   |2018-06-20 |cran (@3.1.0)   |
|miceadds        |   |2.13-63 |2018-07-05 |cran (@2.13-63) |
|micemd          |   |1.2.0   |2018-01-07 |cran (@1.2.0)   |
|mitml           |   |0.3-6   |2018-07-10 |cran (@0.3-6)   |
|mitools         |   |2.3     |2014-09-20 |cran (@2.3)     |
|pan             |   |1.6     |2018-06-29 |cran (@1.6)     |
|randomForest    |   |4.6-14  |2018-03-25 |cran (@4.6-14)  |
|Rcpp            |   |0.12.17 |2018-05-18 |cran (@0.12.17) |
|rlang           |   |0.2.1   |2018-05-30 |cran (@0.2.1)   |
|rmarkdown       |   |1.10    |2018-06-11 |cran (@1.10)    |
|testthat        |   |2.0.0   |2017-12-13 |cran (@2.0.0)   |
|tidyr           |   |0.8.1   |2018-05-18 |cran (@0.8.1)   |
|Zelig           |   |5.1.6   |2018-02-27 |cran (@5.1.6)   |

# Check results

10 packages with problems

|package        |version  | errors| warnings| notes|
|:--------------|:--------|------:|--------:|-----:|
|cobalt         |3.3.0    |      0|        1|     1|
|dynr           |0.1.12-5 |      1|        0|     0|
|Hmisc          |4.1-1    |      1|        0|     0|
|JointAI        |0.2.0    |      1|        0|     0|
|miceFast       |0.2.3    |      1|        0|     0|
|MissingDataGUI |0.2-5    |      1|        0|     0|
|NNLM           |0.4.2    |      1|        0|     0|
|Qtools         |1.3      |      1|        0|     0|
|rattle         |5.1.0    |      1|        0|     0|
|weightTAPSPACK |0.1      |      1|        0|     0|

## cobalt (3.3.0)
Maintainer: Noah Greifer <noah.greifer@gmail.com>  
Bug reports: https://github.com/ngreifer/cobalt/issues

0 errors | 1 warning  | 1 note 

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Loading required package: nnet
Loading required package: numDeriv
Loading required package: glmnet
Loading required package: foreach
Loaded glmnet 2.0-16

CBPS: Covariate Balancing Propensity Score
... 8 lines ...

##
## ebal Package: Implements Entropy Balancing.

## See http://www.stanford.edu/~jhain/ for additional information.


Quitting from lines 225-239 (cobalt_A1_other_packages.Rmd) 
Error: processing vignette 'cobalt_A1_other_packages.Rmd' failed with diagnostics:
there is no package called 'designmatch'
Execution halted

checking package dependencies ... NOTE
Package suggested but not available for checking: ‘designmatch’
```

## dynr (0.1.12-5)
Maintainer: Michael D. Hunter <mhunter.ou@gmail.com>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘dynr’ can be installed ... ERROR
Installation failed.
See ‘/Users/buurensv/Package/mice/mice/revdep/checks/dynr.Rcheck/00install.out’ for details.
```

## Hmisc (4.1-1)
Maintainer: Frank E Harrell Jr <f.harrell@vanderbilt.edu>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘Hmisc’ can be installed ... ERROR
Installation failed.
See ‘/Users/buurensv/Package/mice/mice/revdep/checks/Hmisc.Rcheck/00install.out’ for details.
```

## JointAI (0.2.0)
Maintainer: Nicole S. Erler <n.erler@erasmusmc.nl>  
Bug reports: https://github.com/nerler/JointAI

1 error  | 0 warnings | 0 notes

```
checking whether package ‘JointAI’ can be installed ... ERROR
Installation failed.
See ‘/Users/buurensv/Package/mice/mice/revdep/checks/JointAI.Rcheck/00install.out’ for details.
```

## miceFast (0.2.3)
Maintainer: Maciej Nasinski <nasinski.maciej@gmail.com>  
Bug reports: https://github.com/Polkas/miceFast/issues

1 error  | 0 warnings | 0 notes

```
checking whether package ‘miceFast’ can be installed ... ERROR
Installation failed.
See ‘/Users/buurensv/Package/mice/mice/revdep/checks/miceFast.Rcheck/00install.out’ for details.
```

## MissingDataGUI (0.2-5)
Maintainer: Xiaoyue Cheng <xycheng@unomaha.edu>

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Packages required but not available:
  ‘gWidgetsRGtk2’ ‘GGally’ ‘cairoDevice’ ‘reshape’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## NNLM (0.4.2)
Maintainer: Xihui Lin <ericxihuilin@gmail.com>  
Bug reports: https://github.com/linxihui/NNLM/issues

1 error  | 0 warnings | 0 notes

```
checking whether package ‘NNLM’ can be installed ... ERROR
Installation failed.
See ‘/Users/buurensv/Package/mice/mice/revdep/checks/NNLM.Rcheck/00install.out’ for details.
```

## Qtools (1.3)
Maintainer: Marco Geraci <geraci@mailbox.sc.edu>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘Qtools’ can be installed ... ERROR
Installation failed.
See ‘/Users/buurensv/Package/mice/mice/revdep/checks/Qtools.Rcheck/00install.out’ for details.
```

## rattle (5.1.0)
Maintainer: Graham Williams <Graham.Williams@togaware.com>

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Packages required but not available:
  ‘RGtk2’ ‘cairoDevice’ ‘XML’ ‘rpart.plot’

Packages suggested but not available for checking:
  ‘pmml’ ‘ada’ ‘arules’ ‘arulesViz’ ‘biclust’ ‘cba’ ‘descr’ ‘doBy’
  ‘fpc’ ‘ggdendro’ ‘ggraptR’ ‘gWidgetsRGtk2’ ‘hmeasure’ ‘odfWeave’
  ‘party’ ‘playwith’ ‘rattle.data’ ‘reshape’ ‘rggobi’ ‘RODBC’
  ‘SnowballC’ ‘tm’ ‘verification’ ‘wskm’ ‘RGtk2Extras’ ‘xgboost’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## weightTAPSPACK (0.1)
Maintainer: David G. Carlson <carlson.david@wustl.edu>

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Package required but not available: ‘HotDeckImputation’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

