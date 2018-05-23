# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.5.0 (2018-04-23) |
|system   |x86_64, darwin15.6.0         |
|ui       |RStudio (1.1.453)            |
|language |(EN)                         |
|collate  |nl_NL.UTF-8                  |
|tz       |Europe/Amsterdam             |
|date     |2018-05-23                   |

## Packages

|package         |*  |version |date       |source          |
|:---------------|:--|:-------|:----------|:---------------|
|AGD             |   |0.35    |2015-05-28 |cran (@0.35)    |
|broom           |   |0.4.4   |2018-03-29 |cran (@0.4.4)   |
|BSDA            |   |1.2.0   |2017-07-30 |cran (@1.2.0)   |
|CALIBERrfimpute |   |0.1-6   |2014-05-07 |cran (@0.1-6)   |
|dplyr           |   |0.7.5   |2018-05-19 |cran (@0.7.5)   |
|gamlss          |   |5.0-8   |2018-04-30 |cran (@5.0-8)   |
|HSAUR3          |   |1.0-8   |2017-08-18 |cran (@1.0-8)   |
|knitr           |   |1.20    |2018-02-20 |cran (@1.20)    |
|lme4            |   |1.1-17  |2018-04-03 |cran (@1.1-17)  |
|mice            |   |2.46.0  |2017-10-24 |cran (@2.46.0)  |
|miceadds        |   |2.11-87 |2018-05-21 |cran (@2.11-87) |
|micemd          |   |1.2.0   |2018-01-07 |cran (@1.2.0)   |
|mitml           |   |0.3-5   |2017-03-15 |cran (@0.3-5)   |
|mitools         |   |2.3     |2014-09-20 |cran (@2.3)     |
|pan             |   |1.4     |2016-02-10 |cran (@1.4)     |
|randomForest    |   |4.6-14  |2018-03-25 |cran (@4.6-14)  |
|Rcpp            |   |0.12.17 |2018-05-18 |cran (@0.12.17) |
|rlang           |   |0.2.0   |2018-02-20 |cran (@0.2.0)   |
|rmarkdown       |   |1.9     |2018-03-01 |cran (@1.9)     |
|testthat        |   |2.0.0   |2017-12-13 |cran (@2.0.0)   |
|tidyr           |   |0.8.1   |2018-05-18 |cran (@0.8.1)   |
|Zelig           |   |5.1.6   |2018-02-27 |cran (@5.1.6)   |

# Check results

15 packages with problems

|package         |version  | errors| warnings| notes|
|:---------------|:--------|------:|--------:|-----:|
|CALIBERrfimpute |0.1-6    |      0|        1|     1|
|codebook        |0.5.9    |      1|        0|     1|
|dynr            |0.1.12-5 |      1|        0|     0|
|HardyWeinberg   |1.5.9    |      0|        1|     0|
|Hmisc           |4.1-1    |      1|        0|     0|
|HSAUR3          |1.0-8    |      0|        1|     0|
|JointAI         |0.1.0    |      1|        0|     0|
|logistf         |1.22     |      1|        0|     1|
|miceadds        |2.11-87  |      1|        0|     0|
|miceFast        |0.2.3    |      1|        0|     0|
|miceMNAR        |1.0      |      1|        0|     0|
|MissingDataGUI  |0.2-5    |      1|        0|     0|
|Qtools          |1.3      |      1|        0|     0|
|rattle          |5.1.0    |      1|        0|     0|
|weightTAPSPACK  |0.1      |      1|        0|     0|

## CALIBERrfimpute (0.1-6)
Maintainer: Anoop Shah <anoop@doctors.org.uk>

0 errors | 1 warning  | 1 note 

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Loading required package: mice
Loading required package: lattice

Attaching package: ‘mice’

The following objects are masked from ‘package:base’:

    cbind, rbind

Loading required package: randomForest
randomForest 4.6-14
Type rfNews() to see new features/changes/bug fixes.
Loading required package: foreach
Loading required package: itertools
Loading required package: iterators
Warning in parallel::mclapply(1:N, doanalysis) :
  all scheduled cores encountered errors in user code

Error: processing vignette 'simstudy_survival.Rnw' failed with diagnostics:
 chunk 9 
Error in x[[method]] : subscript out of bounds
Execution halted


checking R code for possible problems ... NOTE
makemar : predictions: no visible global function definition for
  ‘rbinom’
mice.impute.rfcat : <anonymous>: no visible global function definition
  for ‘predict’
mice.impute.rfcont: no visible global function definition for ‘predict’
mice.impute.rfcont: no visible global function definition for ‘rnorm’
simdata: no visible global function definition for ‘rbinom’
simdata: no visible global function definition for ‘rnorm’
Undefined global functions or variables:
  predict rbinom rnorm
Consider adding
  importFrom("stats", "predict", "rbinom", "rnorm")
to your NAMESPACE file.
```

## codebook (0.5.9)
Maintainer: Ruben Arslan <ruben.arslan@gmail.com>  
Bug reports: https://github.com/rubenarslan/codebook/issues

1 error  | 0 warnings | 1 note 

```
checking examples ... ERROR
Running examples in ‘codebook-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: codebook_missingness
> ### Title: Codebook missingness
> ### Aliases: codebook_missingness
> 
> ### ** Examples
> 
> data("bfi")
> codebook_missingness(bfi)
Error in `rownames<-`(`*tmp*`, value = table(pat)) : 
  attempt to set 'rownames' on an object with no dimensions
Calls: codebook_missingness -> md_pattern -> <Anonymous> -> rownames<-
Execution halted

checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘labelled’
```

## dynr (0.1.12-5)
Maintainer: Michael D. Hunter <mhunter.ou@gmail.com>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘dynr’ can be installed ... ERROR
Installation failed.
See ‘/Users/buurensv/Package/mice/mice/revdep/checks/dynr.Rcheck/00install.out’ for details.
```

## HardyWeinberg (1.5.9)
Maintainer: Jan Graffelman <jan.graffelman@upc.edu>

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Loading required package: mice
Loading required package: lattice

Attaching package: 'mice'

The following objects are masked from 'package:base':

    cbind, rbind

Loading required package: Rsolnp

Error: processing vignette 'HardyWeinberg.Rnw' failed with diagnostics:
 chunk 13 
Error in edit.setup(data, setup, ...) : nothing left to impute
Execution halted

```

## Hmisc (4.1-1)
Maintainer: Frank E Harrell Jr <f.harrell@vanderbilt.edu>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘Hmisc’ can be installed ... ERROR
Installation failed.
See ‘/Users/buurensv/Package/mice/mice/revdep/checks/Hmisc.Rcheck/00install.out’ for details.
```

## HSAUR3 (1.0-8)
Maintainer: Torsten Hothorn <Torsten.Hothorn@R-project.org>

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
    geyser

The following object is masked from 'package:HSAUR3':

    birds

Waiting for profiling to be done...
... 8 lines ...

The following objects are masked from 'package:base':

    cbind, rbind


Error: processing vignette 'Ch_missing_values.Rnw' failed with diagnostics:
 chunk 22 (label = MV-bp-lm-mice) 
Error in `[.data.frame`(summary(pool(fit)), , c("est", "se", "t", "Pr(>|t|)")) : 
  undefined columns selected
Execution halted
```

## JointAI (0.1.0)
Maintainer: Nicole S. Erler <n.erler@erasmusmc.nl>  
Bug reports: https://github.com/nerler/JointAI

1 error  | 0 warnings | 0 notes

```
checking whether package ‘JointAI’ can be installed ... ERROR
Installation failed.
See ‘/Users/buurensv/Package/mice/mice/revdep/checks/JointAI.Rcheck/00install.out’ for details.
```

## logistf (1.22)
Maintainer: Georg Heinze <georg.heinze@meduniwien.ac.at>

1 error  | 0 warnings | 1 note 

```
checking examples ... ERROR
Running examples in ‘logistf-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: pool.RR
> ### Title: Compute Pooled Normal Confidence Intervals (following Rubin's
> ###   Rules) after Multiple Imputation
> ### Aliases: pool.RR
> ### Keywords: regression models
... 21 lines ...
+   toymi[[i]]$x[y1==TRUE]<-xnew1
+   toymi[[i]]$x[y0==TRUE]<-xnew0
+ }
> 
> 
> # logistf analyses of each imputed data set
> fit.list<-lapply(1:5, function(X) logistf(data=toymi[[X]], y~x, pl=TRUE, dataout=TRUE))
> summary(pool.RR(fit.list))
Error in sqrt(x$t) : non-numeric argument to mathematical function
Calls: summary -> summary.mipo
Execution halted

checking compiled code ... NOTE
File ‘logistf/libs/logistf.so’:
  Found no calls to: ‘R_registerRoutines’, ‘R_useDynamicSymbols’

It is good practice to register native routines and to disable symbol
search.

See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
```

## miceadds (2.11-87)
Maintainer: Alexander Robitzsch <robitzsch@ipn.uni-kiel.de>

1 error  | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘miceadds-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: mi.anova
> ### Title: Analysis of Variance for Multiply Imputed Data Sets (Using the
> ###   D_2 Statistic)
> ### Aliases: mi.anova
> ### Keywords: ANOVA mids
... 8 lines ...
> library(mice)
> library(car)
Loading required package: carData
> data(nhanes2, package="mice")
> set.seed(9090)
> 
> # nhanes data in one chain and 8 imputed datasets
> mi.res <- miceadds::mice.1chain( nhanes2, burnin=4, iter=20, Nimp=8 )
************ BURNIN PHASE | Iterations 1 - 4 ******************
Error: Argument `formulas` not a list
Execution halted
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

## miceMNAR (1.0)
Maintainer: Jacques-Emmanuel Galimard <jacques-emmanuel.galimard@inserm.fr>

1 error  | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘miceMNAR-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: MNARargument
> ### Title: Function providing modified arguments for imputation of Missing
> ###   Not At Random (MNAR) outcomes using 'mice()' function of the "mice"
> ###   package
> ### Aliases: MNARargument
... 48 lines ...
+                  method = arg$method,
+                  predictorMatrix = arg$predictorMatrix,
+                  JointModelEq=arg$JointModelEq,
+                  control=arg$control,
+                  maxit=1,m=5)

 iter imp variable
  1   1  hivError in model.frame.default(formula = "ry ~ ry + age + highhiv.1 + condom.1 + smoke.1 + y",  : 
  'data' must be a data.frame, not a matrix or an array
Calls: mice ... SemiParBIV -> eval -> eval -> model.frame -> model.frame.default
Execution halted
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
  ‘playwith’ ‘rattle.data’ ‘rggobi’ ‘RODBC’ ‘SnowballC’ ‘tm’
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

