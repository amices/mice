# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.4.2 (2017-09-28) |
|system   |x86_64, darwin15.6.0         |
|ui       |RStudio (1.1.383)            |
|language |(EN)                         |
|collate  |nl_NL.UTF-8                  |
|tz       |NA                           |
|date     |2017-10-22                   |

## Packages

|package         |*  |version |date       |source          |
|:---------------|:--|:-------|:----------|:---------------|
|AGD             |   |0.35    |2015-05-28 |cran (@0.35)    |
|BSDA            |   |1.2.0   |2017-07-30 |cran (@1.2.0)   |
|CALIBERrfimpute |   |0.1-6   |2014-05-07 |cran (@0.1-6)   |
|gamlss          |   |5.0-2   |2017-05-25 |cran (@5.0-2)   |
|knitr           |   |1.17    |2017-08-10 |cran (@1.17)    |
|lme4            |   |1.1-14  |2017-09-27 |cran (@1.1-14)  |
|mice            |*  |2.30    |2017-02-18 |cran (@2.30)    |
|mitools         |   |2.3     |2014-09-20 |cran (@2.3)     |
|pan             |   |1.4     |2016-02-10 |cran (@1.4)     |
|randomForest    |   |4.6-12  |2015-10-07 |cran (@4.6-12)  |
|Rcpp            |   |0.12.13 |2017-09-28 |cran (@0.12.13) |
|rmarkdown       |   |1.6     |2017-06-15 |cran (@1.6)     |
|testthat        |   |1.0.2   |2016-04-23 |cran (@1.0.2)   |
|Zelig           |   |5.1-4   |2017-09-23 |cran (@5.1-4)   |

# Check results

7 packages with problems

|package        |version  | errors| warnings| notes|
|:--------------|:--------|------:|--------:|-----:|
|dynr           |0.1.11-8 |      1|        0|     0|
|Hmisc          |4.0-3    |      1|        0|     0|
|MissingDataGUI |0.2-5    |      1|        0|     0|
|NNLM           |0.4.1    |      1|        0|     0|
|Qtools         |1.2      |      1|        0|     0|
|rattle         |5.1.0    |      1|        0|     0|
|weightTAPSPACK |0.1      |      1|        0|     0|

## dynr (0.1.11-8)
Maintainer: Michael D. Hunter <mhunter.ou@gmail.com>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘dynr’ can be installed ... ERROR
Installation failed.
See ‘/Users/buurensv/Package/mice/mice/revdep/checks/dynr.Rcheck/00install.out’ for details.
```

## Hmisc (4.0-3)
Maintainer: Frank E Harrell Jr <f.harrell@vanderbilt.edu>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘Hmisc’ can be installed ... ERROR
Installation failed.
See ‘/Users/buurensv/Package/mice/mice/revdep/checks/Hmisc.Rcheck/00install.out’ for details.
```

## MissingDataGUI (0.2-5)
Maintainer: Xiaoyue Cheng <xycheng@unomaha.edu>

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Packages required but not available: ‘gWidgetsRGtk2’ ‘cairoDevice’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## NNLM (0.4.1)
Maintainer: Xihui Lin <xihuil.silence@gmail.com>  
Bug reports: https://github.com/linxihui/NNLM/issues

1 error  | 0 warnings | 0 notes

```
checking whether package ‘NNLM’ can be installed ... ERROR
Installation failed.
See ‘/Users/buurensv/Package/mice/mice/revdep/checks/NNLM.Rcheck/00install.out’ for details.
```

## Qtools (1.2)
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
Packages required but not available: ‘RGtk2’ ‘cairoDevice’

Packages suggested but not available for checking:
  ‘pmml’ ‘ada’ ‘arules’ ‘arulesViz’ ‘biclust’ ‘cba’ ‘corrplot’ ‘descr’
  ‘doBy’ ‘fpc’ ‘ggdendro’ ‘ggraptR’ ‘gWidgetsRGtk2’ ‘hmeasure’
  ‘odfWeave’ ‘playwith’ ‘rattle.data’ ‘rggobi’ ‘RODBC’ ‘SnowballC’ ‘tm’
  ‘verification’ ‘wskm’ ‘RGtk2Extras’ ‘xgboost’

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

