# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.4.4 (2018-03-15) |
|system   |x86_64, darwin15.6.0         |
|ui       |RStudio (1.1.447)            |
|language |(EN)                         |
|collate  |nl_NL.UTF-8                  |
|tz       |Europe/Amsterdam             |
|date     |2018-05-18                   |

## Packages

|package         |*  |version |date       |source          |
|:---------------|:--|:-------|:----------|:---------------|
|AGD             |   |0.35    |2015-05-28 |cran (@0.35)    |
|broom           |   |0.4.4   |2018-03-29 |cran (@0.4.4)   |
|BSDA            |   |1.2.0   |2017-07-30 |cran (@1.2.0)   |
|CALIBERrfimpute |   |0.1-6   |2014-05-07 |cran (@0.1-6)   |
|dplyr           |   |0.7.4   |2017-09-28 |cran (@0.7.4)   |
|gamlss          |   |5.0-6   |2017-12-11 |cran (@5.0-6)   |
|HSAUR3          |   |1.0-8   |2017-08-18 |cran (@1.0-8)   |
|knitr           |   |1.20    |2018-02-20 |cran (@1.20)    |
|lme4            |   |1.1-17  |2018-04-03 |cran (@1.1-17)  |
|mice            |   |2.46.0  |2017-10-24 |cran (@2.46.0)  |
|miceadds        |   |2.10-14 |2018-03-29 |cran (@2.10-14) |
|micemd          |   |1.2.0   |2018-01-07 |cran (@1.2.0)   |
|mitml           |   |0.3-5   |2017-03-15 |cran (@0.3-5)   |
|mitools         |   |2.3     |2014-09-20 |cran (@2.3)     |
|pan             |   |1.4     |2016-02-10 |cran (@1.4)     |
|randomForest    |   |4.6-14  |2018-03-25 |cran (@4.6-14)  |
|Rcpp            |   |0.12.16 |2018-03-13 |cran (@0.12.16) |
|rlang           |   |0.2.0   |2018-02-20 |cran (@0.2.0)   |
|rmarkdown       |   |1.9     |2018-03-01 |cran (@1.9)     |
|testthat        |   |2.0.0   |2017-12-13 |cran (@2.0.0)   |
|tidyr           |   |0.8.0   |2018-01-29 |cran (@0.8.0)   |
|Zelig           |   |5.1.6   |2018-02-27 |cran (@5.1.6)   |

# Check results

19 packages with problems

|package         |version  | errors| warnings| notes|
|:---------------|:--------|------:|--------:|-----:|
|brms            |2.3.0    |      1|        0|     2|
|CALIBERrfimpute |0.1-6    |      0|        1|     1|
|codebook        |0.5.8    |      1|        0|     1|
|dlookr          |0.3.0    |      1|        0|     0|
|dynr            |0.1.12-5 |      1|        0|     0|
|HardyWeinberg   |1.5.9    |      0|        1|     0|
|Hmisc           |4.1-1    |      1|        0|     0|
|hot.deck        |1.1      |      0|        1|     0|
|HSAUR3          |1.0-8    |      0|        1|     0|
|JointAI         |0.1.0    |      1|        0|     0|
|logistf         |1.22     |      1|        0|     1|
|miceadds        |2.10-14  |      1|        0|     0|
|miceFast        |0.2.3    |      1|        0|     0|
|miceMNAR        |1.0      |      1|        0|     0|
|MissingDataGUI  |0.2-5    |      1|        0|     0|
|Qtools          |1.3      |      1|        0|     0|
|rattle          |5.1.0    |      1|        0|     0|
|sjmisc          |2.7.2    |      1|        0|     0|
|weightTAPSPACK  |0.1      |      1|        0|     0|

## brms (2.3.0)
Maintainer: Paul-Christian Bürkner <paul.buerkner@gmail.com>  
Bug reports: https://github.com/paul-buerkner/brms/issues

1 error  | 0 warnings | 2 notes

```
checking tests ... ERROR
  Running ‘testthat.R’ [81s/82s]
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  ══ testthat results  ══════════════════════════════════════════════════════════════
  OK: 995 SKIPPED: 1 FAILED: 19
  1. Failure: brm produces expected errors (@tests.brm.R#17) 
  2. Error: all S3 methods have reasonable ouputs (@tests.brmsfit-methods.R#235) 
  3. Error: self-defined functions appear in the Stan code (@tests.make_stancode.R#334) 
  4. Failure: invalid combinations of modeling options are detected (@tests.make_stancode.R#403) 
  5. Failure: invalid combinations of modeling options are detected (@tests.make_stancode.R#411) 
  6. Error: Stan code for multivariate models is correct (@tests.make_stancode.R#454) 
  7. Error: known standard errors appear in the Stan code (@tests.make_stancode.R#701) 
  8. Error: Stan code of response times models is correct (@tests.make_stancode.R#802) 
  9. Error: weighted, censored, and truncated likelihoods are correct (@tests.make_stancode.R#896) 
  1. ...
  
  Error: testthat unit tests failed
  Execution halted

checking installed package size ... NOTE
  installed size is  8.2Mb
  sub-directories of 1Mb or more:
    R     5.1Mb
    doc   2.4Mb

checking R code for possible problems ... NOTE
combine_family_info: no visible binding for global variable ‘isFALSE’
no_sigma: no visible global function definition for ‘isFALSE’
resp_cens : prepare_cens: no visible global function definition for
  ‘isFALSE’
Undefined global functions or variables:
  isFALSE
```

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

## codebook (0.5.8)
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

## dlookr (0.3.0)
Maintainer: Choonghyun Ryu <choonghyun.ryu@gmail.com>

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Packages required but not available:
  ‘RcmdrMisc’ ‘corrplot’ ‘classInt’ ‘moments’ ‘kableExtra’ ‘prettydoc’
  ‘smbinning’ ‘DMwR’

Package suggested but not available for checking: ‘ISLR’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
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

## hot.deck (1.1)
Maintainer: Dave Armstrong <dave@quantoid.net>

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

Warning in hot.deck(isq99, sdCutoff = 3, IDvars = c("IDORIGIN", "YEAR")) :
  52 observations with no observed data.  These observations were removed

Warning in hot.deck(isq99, sdCutoff = 3, IDvars = c("IDORIGIN", "YEAR")) :
  45 of 4661 imputations with # donors < 5, consider increasing sdCutoff or using method='p.draw'

Loading required package: survival
```

## HSAUR3 (1.0-8)
Maintainer: Torsten Hothorn <Torsten.Hothorn@R-project.org>

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...

The following object is masked from 'package:MASS':

    geyser

The following object is masked from 'package:HSAUR3':

... 8 lines ...
Loading required package: lattice

Attaching package: 'mice'

The following objects are masked from 'package:base':

    cbind, rbind

Error: processing vignette 'Ch_missing_values.Rnw' failed with diagnostics:
subscript out of bounds
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

## miceadds (2.10-14)
Maintainer: Alexander Robitzsch <robitzsch@ipn.uni-kiel.de>

1 error  | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘miceadds-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: datlist2mids
> ### Title: Converting a List of Multiply Imputed Data Sets into a 'mids'
> ###   Object
> ### Aliases: datalist2mids datlist2mids
> ### Keywords: mids mice utility function
... 63 lines ...

  1  2  3  4  5  6  7  8

> # plot of observed and imputed data
> plot(a.out)
> 
> # convert list of multiply imputed datasets into a mids object
> a.mids <- miceadds::datlist2mids( a.out$imputations )
Error in 0 * imp1$visitSequence : non-numeric argument to binary operator
Calls: <Anonymous>
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
Packages required but not available:
  ‘gWidgetsRGtk2’ ‘GGally’ ‘cairoDevice’ ‘reshape’

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
  ‘pmml’ ‘ada’ ‘arules’ ‘arulesViz’ ‘biclust’ ‘cba’ ‘corrplot’ ‘descr’
  ‘doBy’ ‘fpc’ ‘ggdendro’ ‘ggraptR’ ‘gWidgetsRGtk2’ ‘hmeasure’
  ‘odfWeave’ ‘playwith’ ‘rattle.data’ ‘reshape’ ‘rggobi’ ‘RODBC’
  ‘SnowballC’ ‘tm’ ‘verification’ ‘wskm’ ‘RGtk2Extras’ ‘xgboost’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## sjmisc (2.7.2)
Maintainer: Daniel Lüdecke <d.luedecke@uke.de>  
Bug reports: https://github.com/strengejacke/sjmisc/issues

1 error  | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘sjmisc-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: frq
> ### Title: Frequencies of labelled variables
> ### Aliases: frq
> 
> ### ** Examples
... 435 lines ...
   4 63-77 216   23.79     23.97   97.11
   5 78-92  26    2.86      2.89  100.00
  NA    NA   7    0.77        NA      NA


> 
> # and with weights
> efc$weights <- abs(rnorm(n = nrow(efc), mean = 1, sd = .5))
> frq(efc, c160age, auto.grp = 5, weight.by = weights)
Error: Package `Hmisc` needed for this function to work. Please install it.
Execution halted
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

