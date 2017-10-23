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

40 packages

|package            |version  | errors| warnings| notes|
|:------------------|:--------|------:|--------:|-----:|
|accelmissing       |1.1      |      0|        0|     0|
|BaBooN             |0.2-0    |      0|        0|     2|
|BaM                |1.0.1    |      0|        0|     0|
|CALIBERrfimpute    |0.1-6    |      0|        0|     1|
|cati               |0.99.1   |      0|        0|     0|
|cobalt             |3.0.0    |      0|        0|     0|
|dynr               |0.1.11-8 |      1|        0|     0|
|genpathmox         |0.3      |      0|        0|     0|
|HardyWeinberg      |1.5.8    |      0|        0|     0|
|hdnom              |4.9      |      0|        0|     0|
|hmi                |0.7.8    |      0|        0|     0|
|Hmisc              |4.0-3    |      1|        0|     0|
|hot.deck           |1.1      |      0|        0|     0|
|HSAUR3             |1.0-8    |      0|        0|     0|
|ImputeRobust       |1.2      |      0|        0|     0|
|JWileymisc         |0.2.1    |      0|        0|     0|
|Lambda4            |3.0      |      0|        0|     3|
|lavaan.survey      |1.1.3.1  |      0|        0|     0|
|logistf            |1.22     |      0|        0|     1|
|LSAmitR            |1.0-0    |      0|        0|     0|
|mdmb               |0.4-15   |      0|        0|     0|
|medflex            |0.6-4    |      0|        0|     0|
|miceadds           |2.7-19   |      0|        0|     0|
|micemd             |1.1.0    |      0|        0|     0|
|midastouch         |1.3      |      0|        0|     0|
|MissingDataGUI     |0.2-5    |      1|        0|     0|
|missMDA            |1.11     |      0|        0|     0|
|mitml              |0.3-5    |      0|        0|     0|
|NNLM               |0.4.1    |      1|        0|     0|
|PathSelectMP       |1.0      |      0|        0|     0|
|Qtools             |1.2      |      1|        0|     0|
|rattle             |5.1.0    |      1|        0|     0|
|roughrf            |1.0      |      0|        0|     1|
|semTools           |0.4-14   |      0|        0|     0|
|sjmisc             |2.6.1    |      0|        0|     0|
|stpm               |1.6.8    |      0|        0|     0|
|SYNCSA             |1.3.2    |      0|        0|     2|
|TestDataImputation |1.0      |      0|        0|     0|
|weights            |0.85     |      0|        0|     1|
|weightTAPSPACK     |0.1      |      1|        0|     0|

## accelmissing (1.1)
Maintainer: Jung Ae Lee <jungaeleeb@gmail.com>

0 errors | 0 warnings | 0 notes

## BaBooN (0.2-0)
Maintainer: Florian Meinfelder <florian.meinfelder@uni-bamberg.de>

0 errors | 0 warnings | 2 notes

```
checking R code for possible problems ... NOTE
BB.mod.stab.glm: no visible global function definition for ‘glm’
BB.mod.stab.glm: no visible global function definition for ‘binomial’
BB.mod.stab.glm: no visible binding for global variable ‘na.exclude’
BB.mod.stab.glm: no visible global function definition for
  ‘glm.control’
BB.mod.stab.glm: no visible global function definition for ‘lm’
BB.mod.stab.mlog: no visible binding for global variable ‘na.exclude’
BBPMM: no visible binding for global variable ‘var’
BBPMM: no visible global function definition for ‘runif’
... 25 lines ...
rowimpPrep: no visible binding for global variable ‘var’
summary.impprep: no visible global function definition for ‘head’
Undefined global functions or variables:
  as.formula binomial glm glm.control head lm median na.exclude na.omit
  predict qt rmultinom runif sd var write.table
Consider adding
  importFrom("stats", "as.formula", "binomial", "glm", "glm.control",
             "lm", "median", "na.exclude", "na.omit", "predict", "qt",
             "rmultinom", "runif", "sd", "var")
  importFrom("utils", "head", "write.table")
to your NAMESPACE file.

checking compiled code ... NOTE
File ‘BaBooN/libs/BaBooN.so’:
  Found no calls to: ‘R_registerRoutines’, ‘R_useDynamicSymbols’

It is good practice to register native routines and to disable symbol
search.

See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
```

## BaM (1.0.1)
Maintainer: Jeff Gill <jgill@wustl.edu>

0 errors | 0 warnings | 0 notes

## CALIBERrfimpute (0.1-6)
Maintainer: Anoop Shah <anoop@doctors.org.uk>

0 errors | 0 warnings | 1 note 

```
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

## cati (0.99.1)
Maintainer: Adrien Taudiere <adrien.taudiere@cefe.cnrs.fr>

0 errors | 0 warnings | 0 notes

## cobalt (3.0.0)
Maintainer: Noah Greifer <noah@unc.edu>

0 errors | 0 warnings | 0 notes

## dynr (0.1.11-8)
Maintainer: Michael D. Hunter <mhunter.ou@gmail.com>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘dynr’ can be installed ... ERROR
Installation failed.
See ‘/Users/buurensv/Package/mice/mice/revdep/checks/dynr.Rcheck/00install.out’ for details.
```

## genpathmox (0.3)
Maintainer: Giuseppe Lamberti <giuseppelamb@hotmail.com>

0 errors | 0 warnings | 0 notes

## HardyWeinberg (1.5.8)
Maintainer: Jan Graffelman <jan.graffelman@upc.edu>

0 errors | 0 warnings | 0 notes

## hdnom (4.9)
Maintainer: Nan Xiao <me@nanx.me>  
Bug reports: https://github.com/road2stat/hdnom/issues

0 errors | 0 warnings | 0 notes

## hmi (0.7.8)
Maintainer: Matthias Speidel <matthias.speidel@googlemail.com>  
Bug reports: http://github.com/matthiasspeidel/hmi/issues

0 errors | 0 warnings | 0 notes

## Hmisc (4.0-3)
Maintainer: Frank E Harrell Jr <f.harrell@vanderbilt.edu>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘Hmisc’ can be installed ... ERROR
Installation failed.
See ‘/Users/buurensv/Package/mice/mice/revdep/checks/Hmisc.Rcheck/00install.out’ for details.
```

## hot.deck (1.1)
Maintainer: Dave Armstrong <dave@quantoid.net>

0 errors | 0 warnings | 0 notes

## HSAUR3 (1.0-8)
Maintainer: Torsten Hothorn <Torsten.Hothorn@R-project.org>

0 errors | 0 warnings | 0 notes

## ImputeRobust (1.2)
Maintainer: Daniel Salfran <daniel.salfran@uni-hamburg.de>

0 errors | 0 warnings | 0 notes

## JWileymisc (0.2.1)
Maintainer: Joshua F. Wiley <jwiley.psych@gmail.com>  
Bug reports: https://github.com/JWiley/JWileymisc/issues

0 errors | 0 warnings | 0 notes

## Lambda4 (3.0)
Maintainer: Tyler Hunt <tyler@psychoanalytix.com>  
Bug reports: https://github.com/JackStat/Lambda4/issues

0 errors | 0 warnings | 3 notes

```
checking DESCRIPTION meta-information ... NOTE
Malformed Title field: should not end in a period.

checking dependencies in R code ... NOTE
'library' or 'require' calls in package code:
  ‘GPArotation’ ‘mice’
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.

checking R code for possible problems ... NOTE
angoff: no visible global function definition for ‘cov2cor’
angoff: no visible global function definition for ‘runif’
cov.lambda4: no visible global function definition for ‘cov2cor’
cov.lambda4: no visible global function definition for ‘median’
guttman: no visible global function definition for ‘cov2cor’
impute.cov: no visible global function definition for ‘cov’
impute.cov: no visible global function definition for ‘complete’
impute.cov: no visible global function definition for ‘mice’
kristof: no visible global function definition for ‘cov2cor’
... 12 lines ...
quant.lambda4: no visible global function definition for ‘quantile’
raju: no visible global function definition for ‘cov2cor’
raju: no visible global function definition for ‘runif’
user.lambda4: no visible global function definition for ‘runif’
user.lambda4: no visible binding for global variable ‘sd’
Undefined global functions or variables:
  complete cov cov2cor factanal median mice quantile runif sd
Consider adding
  importFrom("stats", "cov", "cov2cor", "factanal", "median", "quantile",
             "runif", "sd")
to your NAMESPACE file.
```

## lavaan.survey (1.1.3.1)
Maintainer: Daniel Oberski <daniel.oberski@gmail.com>

0 errors | 0 warnings | 0 notes

## logistf (1.22)
Maintainer: Georg Heinze <georg.heinze@meduniwien.ac.at>

0 errors | 0 warnings | 1 note 

```
checking compiled code ... NOTE
File ‘logistf/libs/logistf.so’:
  Found no calls to: ‘R_registerRoutines’, ‘R_useDynamicSymbols’

It is good practice to register native routines and to disable symbol
search.

See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
```

## LSAmitR (1.0-0)
Maintainer: Thomas Kiefer <t.kiefer@bifie.at>

0 errors | 0 warnings | 0 notes

## mdmb (0.4-15)
Maintainer: Alexander Robitzsch <robitzsch@ipn.uni-kiel.de>

0 errors | 0 warnings | 0 notes

## medflex (0.6-4)
Maintainer: Johan Steen <johan.steen@gmail.com>

0 errors | 0 warnings | 0 notes

## miceadds (2.7-19)
Maintainer: Alexander Robitzsch <robitzsch@ipn.uni-kiel.de>

0 errors | 0 warnings | 0 notes

## micemd (1.1.0)
Maintainer: Vincent Audigier <vincent.audigier@univ-paris-diderot.fr>

0 errors | 0 warnings | 0 notes

## midastouch (1.3)
Maintainer: Philipp Gaffert <philipp.gaffert@web.de>

0 errors | 0 warnings | 0 notes

## MissingDataGUI (0.2-5)
Maintainer: Xiaoyue Cheng <xycheng@unomaha.edu>

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Packages required but not available: ‘gWidgetsRGtk2’ ‘cairoDevice’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## missMDA (1.11)
Maintainer: Francois Husson <husson@agrocampus-ouest.fr>

0 errors | 0 warnings | 0 notes

## mitml (0.3-5)
Maintainer: Simon Grund <grund@ipn.uni-kiel.de>  
Bug reports: https://github.com/simongrund1/mitml/issues

0 errors | 0 warnings | 0 notes

## NNLM (0.4.1)
Maintainer: Xihui Lin <xihuil.silence@gmail.com>  
Bug reports: https://github.com/linxihui/NNLM/issues

1 error  | 0 warnings | 0 notes

```
checking whether package ‘NNLM’ can be installed ... ERROR
Installation failed.
See ‘/Users/buurensv/Package/mice/mice/revdep/checks/NNLM.Rcheck/00install.out’ for details.
```

## PathSelectMP (1.0)
Maintainer: William Terry <wterryj6a214@gmail.com>

0 errors | 0 warnings | 0 notes

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

## roughrf (1.0)
Maintainer: Kuangnan Xiong <kxiong@albany.edu>

0 errors | 0 warnings | 1 note 

```
checking R code for possible problems ... NOTE
mfix : m4m: no visible global function definition for ‘median’
rrfa: no visible global function definition for ‘predict’
rrfb: no visible global function definition for ‘predict’
rrfc1: no visible global function definition for ‘predict’
rrfc2: no visible global function definition for ‘predict’
rrfc3: no visible global function definition for ‘predict’
rrfc4: no visible global function definition for ‘predict’
rrfc5 : regdat: no visible global function definition for ‘predict’
rrfc5 : regdat: no visible global function definition for ‘glm’
rrfc5: no visible global function definition for ‘predict’
rrfc6: no visible global function definition for ‘predict’
rrfc7: no visible global function definition for ‘predict’
rrfd: no visible global function definition for ‘predict’
rrfe : rdms: no visible global function definition for ‘rbinom’
rrfe: no visible global function definition for ‘predict’
Undefined global functions or variables:
  glm median predict rbinom
Consider adding
  importFrom("stats", "glm", "median", "predict", "rbinom")
to your NAMESPACE file.
```

## semTools (0.4-14)
Maintainer: Terrence D. Jorgensen <TJorgensen314@gmail.com>

0 errors | 0 warnings | 0 notes

## sjmisc (2.6.1)
Maintainer: Daniel Lüdecke <d.luedecke@uke.de>  
Bug reports: https://github.com/strengejacke/sjmisc/issues

0 errors | 0 warnings | 0 notes

## stpm (1.6.8)
Maintainer: Ilya Y. Zhbannikov <ilya.zhbannikov@duke.edu>

0 errors | 0 warnings | 0 notes

## SYNCSA (1.3.2)
Maintainer: Vanderlei Julio Debastiani
        <vanderleidebastiani@yahoo.com.br>

0 errors | 0 warnings | 2 notes

```
checking DESCRIPTION meta-information ... NOTE
Malformed Title field: should not end in a period.

checking R code for possible problems ... NOTE
ProgressBAR: no visible global function definition for ‘txtProgressBar’
ProgressBAR: no visible global function definition for
  ‘setTxtProgressBar’
cor.matrix: no visible global function definition for ‘cor’
cor.matrix.partial: no visible global function definition for ‘cor’
optimal: no visible global function definition for ‘combn’
optimal: no visible global function definition for ‘cor’
Undefined global functions or variables:
  combn cor setTxtProgressBar txtProgressBar
Consider adding
  importFrom("stats", "cor")
  importFrom("utils", "combn", "setTxtProgressBar", "txtProgressBar")
to your NAMESPACE file.
```

## TestDataImputation (1.0)
Maintainer: Shenghai Dai <dais@indiana.edu>

0 errors | 0 warnings | 0 notes

## weights (0.85)
Maintainer: Josh Pasek <josh@joshpasek.com>

0 errors | 0 warnings | 1 note 

```
checking compiled code ... NOTE
File ‘weights/libs/weights.so’:
  Found no calls to: ‘R_registerRoutines’, ‘R_useDynamicSymbols’

It is good practice to register native routines and to disable symbol
search.

See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
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

