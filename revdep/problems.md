# accelmissing

<details>

* Version: 1.4
* GitHub: NA
* Source code: https://github.com/cran/accelmissing
* Date/Publication: 2018-04-06 03:21:33 UTC
* Number of recursive dependencies: 61

Run `revdepcheck::revdep_details(, "accelmissing")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) accelimp.Rd:15: Lost braces
        15 | \item[]{...}
           |        ^
    ```

# adjustedCurves

<details>

* Version: 0.11.2
* GitHub: https://github.com/RobinDenz1/adjustedCurves
* Source code: https://github.com/cran/adjustedCurves
* Date/Publication: 2024-07-29 14:30:02 UTC
* Number of recursive dependencies: 174

Run `revdepcheck::revdep_details(, "adjustedCurves")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      Attaching package: 'eventglm'
      
      The following objects are masked from 'package:prodlim':
      
          leaveOneOut.competing.risks, leaveOneOut.survival
      
      The following objects are masked from 'package:survival':
      
          colon, mgus2
      
      > library(rmarkdown)
      > library(mice)
      Error in library(mice) : there is no package called 'mice'
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘mice’
    ```

# alookr

<details>

* Version: 0.3.9
* GitHub: https://github.com/choonghyunryu/alookr
* Source code: https://github.com/cran/alookr
* Date/Publication: 2024-02-11 07:30:02 UTC
* Number of recursive dependencies: 167

Run `revdepcheck::revdep_details(, "alookr")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# autoReg

<details>

* Version: 0.3.3
* GitHub: https://github.com/cardiomoon/autoReg
* Source code: https://github.com/cran/autoReg
* Date/Publication: 2023-11-14 05:53:27 UTC
* Number of recursive dependencies: 212

Run `revdepcheck::revdep_details(, "autoReg")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# BaM

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/BaM
* Date/Publication: 2022-10-14 11:25:17 UTC
* Number of recursive dependencies: 61

Run `revdepcheck::revdep_details(, "BaM")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# basecamb

<details>

* Version: 1.1.5
* GitHub: https://github.com/codeblue-team/basecamb
* Source code: https://github.com/cran/basecamb
* Date/Publication: 2024-04-22 19:10:07 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::revdep_details(, "basecamb")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# betaMC

<details>

* Version: 1.3.2
* GitHub: https://github.com/jeksterslab/betaMC
* Source code: https://github.com/cran/betaMC
* Date/Publication: 2024-04-14 18:00:09 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::revdep_details(, "betaMC")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘betaMC-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: MCMI
    > ### Title: Generate the Sampling Distribution of Regression Parameters
    > ###   Using the Monte Carlo Method for Data with Missing Values
    > ### Aliases: MCMI
    > ### Keywords: betaMC mc
    > 
    > ### ** Examples
    > 
    > # Data ---------------------------------------------------------------------
    > data("nas1982", package = "betaMC")
    > nas1982_missing <- mice::ampute(nas1982)$amp # data set with missing values
    Error in loadNamespace(x) : there is no package called ‘mice’
    Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
    Execution halted
    ```

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Error ('test-betaMC-s-cor-mc-est-mi.R:21:5'): (code run outside of `test_that()`) ──
      <packageNotFoundError/error/condition>
      Error in `loadNamespace(x)`: there is no package called 'mice'
      Backtrace:
          ▆
       1. ├─base::lapply(...) at test-betaMC-s-cor-mc-est-mi.R:2:1
       2. │ └─betaMC (local) FUN(X[[i]], ...)
       3. └─base::loadNamespace(x) at test-betaMC-s-cor-mc-est-mi.R:21:5
       4.   └─base::withRestarts(stop(cond), retry_loadNamespace = function() NULL)
       5.     └─base (local) withOneRestart(expr, restarts[[1L]])
       6.       └─base (local) doWithOneRestart(return(expr), restart)
      
      [ FAIL 8 | WARN 0 | SKIP 0 | PASS 45 ]
      Error: Test failures
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘mice’
    ```

# BGGM

<details>

* Version: 2.1.3
* GitHub: https://github.com/donaldRwilliams/BGGM
* Source code: https://github.com/cran/BGGM
* Date/Publication: 2024-07-05 20:30:02 UTC
* Number of recursive dependencies: 194

Run `revdepcheck::revdep_details(, "BGGM")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ...
    ```
      ‘control.Rmd’ using ‘UTF-8’... OK
      ‘hyp_3_ways.Rmd’ using ‘UTF-8’... OK
      ‘in_tandem.Rmd’ using ‘UTF-8’... OK
      ‘installation.Rmd’ using ‘UTF-8’... OK
      ‘mcmc_diagnostics.Rmd’ using ‘UTF-8’... OK
      ‘netplot.Rmd’ using ‘UTF-8’... OK
      ‘netstat_custom.Rmd’ using ‘UTF-8’... failed
      ‘ppc_custom.Rmd’ using ‘UTF-8’... failed
      ‘predictability.Rmd’ using ‘UTF-8’... OK
      ‘test_sum.Rmd’ using ‘UTF-8’... OK
    ...
    > library(ggplot2)
    
    > library(assortnet)
    
    > library(networktools)
    
      When sourcing ‘ppc_custom.R’:
    Error: package or namespace load failed for ‘networktools’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
     there is no package called ‘mice’
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘mice’
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        doc    3.4Mb
        help   1.1Mb
    ```

# biokNN

<details>

* Version: 0.1.0
* GitHub: https://github.com/mcubillos3/biokNN
* Source code: https://github.com/cran/biokNN
* Date/Publication: 2021-04-22 07:20:15 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::revdep_details(, "biokNN")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘desc’ ‘lme4’ ‘mitml’
      All declared Imports should be used.
    ```

# bipd

<details>

* Version: 0.3
* GitHub: NA
* Source code: https://github.com/cran/bipd
* Date/Publication: 2022-06-05 16:10:05 UTC
* Number of recursive dependencies: 150

Run `revdepcheck::revdep_details(, "bipd")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

## Newly fixed

*   checking dependencies in R code ...sh: line 1: 46040 Segmentation fault: 11  R_DEFAULT_PACKAGES=NULL '/Library/Frameworks/R.framework/Resources/bin/R' --vanilla --no-echo 2>&1 < '/var/folders/5_/g85d42yj50b6lrjq4rzjzg8w0000gn/T//RtmpkYA44J/fileb0ba2764bcb7'
    ```
     NOTE
    
     *** caught segfault ***
    address 0x656d756c6f562f6d, cause 'invalid permissions'
    
    Traceback:
     1: dyn.load(file, DLLpath = DLLpath, ...)
     2: library.dynam(lib, package, package.lib)
     3: loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
     4: asNamespace(ns)
    ...
     7: withCallingHandlers(expr, message = function(c) if (inherits(c,     classes)) tryInvokeRestart("muffleMessage"))
     8: suppressMessages(loadNamespace(p))
     9: withCallingHandlers(expr, warning = function(w) if (inherits(w,     classes)) tryInvokeRestart("muffleWarning"))
    10: suppressWarnings(suppressMessages(loadNamespace(p)))
    11: doTryCatch(return(expr), name, parentenv, handler)
    12: tryCatchOne(expr, names, parentenv, handlers[[1L]])
    13: tryCatchList(expr, classes, parentenv, handlers)
    14: tryCatch(suppressWarnings(suppressMessages(loadNamespace(p))),     error = function(e) e)
    15: tools:::.check_packages_used(package = "bipd")
    An irrecoverable exception occurred. R is aborting now ...
    ```

## In both

*   checking running R code from vignettes ...
    ```
      ‘IPD-meta-analysis-with-missing-data.Rmd’ using ‘UTF-8’... failed
      ‘IPD-meta-analysis.Rmd’ using ‘UTF-8’... failed
      ‘Imputing-missing-values-in-IPD.Rmd’ using ‘UTF-8’... failed
     WARNING
    Errors in running code in vignettes:
    when running code in ‘IPD-meta-analysis-with-missing-data.Rmd’
      ...
    3  2.61 -1.31  1     0     -1.23   0.165      1     1
    4  1.47 -0.590 1     1      0.435  0.345      1     0
    5  4.39  1.20  1     1      0.0561 0.287      1     0
    ...
    [1] "x3" "x4" "x5"
    
    > missP2$spor_covariates
    [1] "x1"
    
    > library(mice)
    
      When sourcing ‘Imputing-missing-values-in-IPD.R’:
    Error: there is no package called ‘mice’
    Execution halted
    ```

# bootImpute

<details>

* Version: 1.2.1
* GitHub: NA
* Source code: https://github.com/cran/bootImpute
* Date/Publication: 2023-06-01 10:30:02 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::revdep_details(, "bootImpute")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# brokenstick

<details>

* Version: 2.5.0
* GitHub: https://github.com/growthcharts/brokenstick
* Source code: https://github.com/cran/brokenstick
* Date/Publication: 2023-03-22 23:00:06 UTC
* Number of recursive dependencies: 117

Run `revdepcheck::revdep_details(, "brokenstick")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘mice’
    ```

# broom.helpers

<details>

* Version: 1.17.0
* GitHub: https://github.com/larmarange/broom.helpers
* Source code: https://github.com/cran/broom.helpers
* Date/Publication: 2024-08-28 12:30:02 UTC
* Number of recursive dependencies: 241

Run `revdepcheck::revdep_details(, "broom.helpers")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# CALIBERrfimpute

<details>

* Version: 1.0-7
* GitHub: NA
* Source code: https://github.com/cran/CALIBERrfimpute
* Date/Publication: 2022-12-04 21:02:32 UTC
* Number of recursive dependencies: 69

Run `revdepcheck::revdep_details(, "CALIBERrfimpute")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# cati

<details>

* Version: 0.99.4
* GitHub: https://github.com/adrientaudiere/cati
* Source code: https://github.com/cran/cati
* Date/Publication: 2022-02-25 10:10:05 UTC
* Number of recursive dependencies: 174

Run `revdepcheck::revdep_details(, "cati")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# censcyt

<details>

* Version: 1.14.0
* GitHub: https://github.com/retogerber/censcyt
* Source code: https://github.com/cran/censcyt
* Date/Publication: 2024-10-29
* Number of recursive dependencies: 202

Run `revdepcheck::revdep_details(, "censcyt")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    License stub is invalid DCF.
    ```

*   checking R code for possible problems ... NOTE
    ```
    mean_residual_life_imputation: no visible binding for global variable
      ‘hazard’
    Undefined global functions or variables:
      hazard
    ```

# CIMPLE

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/CIMPLE
* Date/Publication: 2024-11-12 13:20:10 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::revdep_details(, "CIMPLE")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# ClustAll

<details>

* Version: 1.2.0
* GitHub: NA
* Source code: https://github.com/cran/ClustAll
* Date/Publication: 2024-10-29
* Number of recursive dependencies: 186

Run `revdepcheck::revdep_details(, "ClustAll")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# clusterMI

<details>

* Version: 1.2.2
* GitHub: NA
* Source code: https://github.com/cran/clusterMI
* Date/Publication: 2024-10-23 13:40:02 UTC
* Number of recursive dependencies: 246

Run `revdepcheck::revdep_details(, "clusterMI")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# cmahalanobis

<details>

* Version: 0.4.2
* GitHub: NA
* Source code: https://github.com/cran/cmahalanobis
* Date/Publication: 2024-09-23 22:00:22 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::revdep_details(, "cmahalanobis")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# cobalt

<details>

* Version: 4.5.5
* GitHub: https://github.com/ngreifer/cobalt
* Source code: https://github.com/cran/cobalt
* Date/Publication: 2024-04-02 21:25:01 UTC
* Number of recursive dependencies: 173

Run `revdepcheck::revdep_details(, "cobalt")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ...
    ```
      ‘cobalt.Rmd’ using ‘UTF-8’... OK
      ‘faq.Rmd’ using ‘UTF-8’... OK
      ‘longitudinal-treat.Rmd’ using ‘UTF-8’... OK
      ‘love.plot.Rmd’ using ‘UTF-8’... OK
      ‘optimizing-balance.Rmd’ using ‘UTF-8’... OK
      ‘other-packages.Rmd’ using ‘UTF-8’... OK
      ‘segmented-data.Rmd’ using ‘UTF-8’... failed
     WARNING
    Errors in running code in vignettes:
    when running code in ‘segmented-data.Rmd’
    ...
    
    > set.seed(100)
    
    > m <- 10
    
    > imp.out <- mice::mice(lalonde_mis, m = m, print = FALSE)
    
      When sourcing ‘segmented-data.R’:
    Error: there is no package called ‘mice’
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# dlookr

<details>

* Version: 0.6.3
* GitHub: https://github.com/choonghyunryu/dlookr
* Source code: https://github.com/cran/dlookr
* Date/Publication: 2024-02-07 12:00:06 UTC
* Number of recursive dependencies: 168

Run `revdepcheck::revdep_details(, "dlookr")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# dynamite

<details>

* Version: 1.5.5
* GitHub: https://github.com/ropensci/dynamite
* Source code: https://github.com/cran/dynamite
* Date/Publication: 2024-11-15 20:10:02 UTC
* Number of recursive dependencies: 137

Run `revdepcheck::revdep_details(, "dynamite")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘mice’
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: 'cmdstanr', 'mice'
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 13.1Mb
      sub-directories of 1Mb or more:
        R      3.6Mb
        data   8.1Mb
        doc    1.1Mb
    ```

# dynr

<details>

* Version: 0.1.16-105
* GitHub: https://github.com/mhunter1/dynr
* Source code: https://github.com/cran/dynr
* Date/Publication: 2023-11-28 05:20:05 UTC
* Number of recursive dependencies: 147

Run `revdepcheck::revdep_details(, "dynr")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking whether package ‘dynr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/dynr/old/dynr.Rcheck/00install.out’ for details.
    ```

# eatRep

<details>

* Version: 0.14.7
* GitHub: https://github.com/weirichs/eatRep
* Source code: https://github.com/cran/eatRep
* Date/Publication: 2023-03-26 22:30:10 UTC
* Number of recursive dependencies: 182

Run `revdepcheck::revdep_details(, "eatRep")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# finalfit

<details>

* Version: 1.0.8
* GitHub: https://github.com/ewenharrison/finalfit
* Source code: https://github.com/cran/finalfit
* Date/Publication: 2024-07-24 15:20:01 UTC
* Number of recursive dependencies: 176

Run `revdepcheck::revdep_details(, "finalfit")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        doc   4.9Mb
    ```

# FLAME

<details>

* Version: 2.1.1
* GitHub: https://github.com/vittorioorlandi/FLAME
* Source code: https://github.com/cran/FLAME
* Date/Publication: 2021-12-07 22:50:02 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::revdep_details(, "FLAME")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       4.     └─FLAME:::preprocess(...)
       5.       └─FLAME:::check_args(...)
      ── Error ('test_one_covariate.R:55:3'): runs with 'impute' missingness ─────────
      Error: Package `mice` needed to impute missing values. Please install it or select different options for `missing_data`.
      Backtrace:
          ▆
       1. └─FLAME::FLAME(df, holdout, missing_data = "impute", missing_holdout = "impute") at test_one_covariate.R:55:3
       2.   ├─base::do.call(...)
       3.   └─FLAME (local) `<fn>`(...)
       4.     └─FLAME:::preprocess(...)
       5.       └─FLAME:::check_args(...)
      
      [ FAIL 4 | WARN 5 | SKIP 0 | PASS 4597 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ...
    ```
      ‘intro_to_AME.Rmd’ using ‘UTF-8’... failed
     ERROR
    Errors in running code in vignettes:
    when running code in ‘intro_to_AME.Rmd’
      ...
    20  1  2  2  2  3       0       0
    57  1  2  2  2  3       1       0
    
    
    > DAME_impute <- DAME(data, holdout, missing_data = "impute", 
    +     verbose = 0)
    
      When sourcing ‘intro_to_AME.R’:
    Error: Package `mice` needed to impute missing values. Please install it or select different options for `missing_data`.
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# flevr

<details>

* Version: 0.0.4
* GitHub: https://github.com/bdwilliamson/flevr
* Source code: https://github.com/cran/flevr
* Date/Publication: 2023-11-30 10:20:05 UTC
* Number of recursive dependencies: 148

Run `revdepcheck::revdep_details(, "flevr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      step half ouch...
      warning - model size was reduced
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 10 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-intrinsic_selection.R:7:1'): (code run outside of `test_that()`) ──
      <packageNotFoundError/error/condition>
      Error in `library("mice")`: there is no package called 'mice'
      Backtrace:
          ▆
       1. └─base::library("mice") at test-intrinsic_selection.R:7:1
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 10 ]
      Error: Test failures
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# gerbil

<details>

* Version: 0.1.9
* GitHub: NA
* Source code: https://github.com/cran/gerbil
* Date/Publication: 2023-01-12 11:20:02 UTC
* Number of recursive dependencies: 117

Run `revdepcheck::revdep_details(, "gerbil")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# gFormulaMI

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/gFormulaMI
* Date/Publication: 2023-05-25 20:00:08 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::revdep_details(, "gFormulaMI")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# ggeffects

<details>

* Version: 1.7.2
* GitHub: https://github.com/strengejacke/ggeffects
* Source code: https://github.com/cran/ggeffects
* Date/Publication: 2024-10-13 11:10:02 UTC
* Number of recursive dependencies: 271

Run `revdepcheck::revdep_details(, "ggeffects")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘mice’
    ```

# ggmice

<details>

* Version: 0.1.0
* GitHub: https://github.com/amices/ggmice
* Source code: https://github.com/cran/ggmice
* Date/Publication: 2023-08-07 14:20:02 UTC
* Number of recursive dependencies: 119

Run `revdepcheck::revdep_details(, "ggmice")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# gtsummary

<details>

* Version: 2.0.3
* GitHub: https://github.com/ddsjoberg/gtsummary
* Source code: https://github.com/cran/gtsummary
* Date/Publication: 2024-10-04 19:30:02 UTC
* Number of recursive dependencies: 186

Run `revdepcheck::revdep_details(, "gtsummary")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# HardyWeinberg

<details>

* Version: 1.7.8
* GitHub: NA
* Source code: https://github.com/cran/HardyWeinberg
* Date/Publication: 2024-04-06 09:53:07 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::revdep_details(, "HardyWeinberg")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# hhsmm

<details>

* Version: 0.4.2
* GitHub: https://github.com/mortamini/hhsmm
* Source code: https://github.com/cran/hhsmm
* Date/Publication: 2024-09-04 09:00:10 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::revdep_details(, "hhsmm")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# Hmisc

<details>

* Version: 5.2-0
* GitHub: NA
* Source code: https://github.com/cran/Hmisc
* Date/Publication: 2024-10-28 05:10:05 UTC
* Number of recursive dependencies: 170

Run `revdepcheck::revdep_details(, "Hmisc")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘mice’
    ```

## In both

*   checking R code for possible problems ... NOTE
    ```
    html_describe_con: multiple local function definitions for ‘g’ with
      different formal arguments
    ```

# holodeck

<details>

* Version: 0.2.2
* GitHub: https://github.com/Aariq/holodeck
* Source code: https://github.com/cran/holodeck
* Date/Publication: 2023-08-25 22:00:06 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::revdep_details(, "holodeck")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ...
    ```
      ‘simulating-data.Rmd’ using ‘UTF-8’... failed
     WARNING
    Errors in running code in vignettes:
    when running code in ‘simulating-data.Rmd’
      ...
        intersect, setdiff, setequal, union
    
    
    > library(purrr)
    
    > library(mice)
    
      When sourcing ‘simulating-data.R’:
    Error: there is no package called ‘mice’
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# hot.deck

<details>

* Version: 1.2
* GitHub: NA
* Source code: https://github.com/cran/hot.deck
* Date/Publication: 2021-08-17 16:40:09 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::revdep_details(, "hot.deck")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) hd.ord.Rd:43: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) hd.ord.Rd:44: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) hd.ord.Rd:45: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) hd.ord.Rd:46: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) hd.ord.Rd:47: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) hd.ord.Rd:48: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) hd.ord.Rd:49: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) hd.ord.Rd:50: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) hd.ord.Rd:51: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) hd.ord.Rd:52: Lost braces in \itemize; \value handles \item{}{} directly
    ...
    checkRd: (-1) hd.ord.Rd:53: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) hd.ord.Rd:54: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) hd.ord.Rd:55: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) hd.ord.Rd:56: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) hot.deck.Rd:39: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) hot.deck.Rd:40: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) hot.deck.Rd:41: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) hot.deck.Rd:42: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) hot.deck.Rd:43: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) hot.deck.Rd:44: Lost braces in \itemize; \value handles \item{}{} directly
    ```

# howManyImputations

<details>

* Version: 0.2.5
* GitHub: https://github.com/josherrickson/howManyImputations
* Source code: https://github.com/cran/howManyImputations
* Date/Publication: 2024-03-15 14:30:02 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::revdep_details(, "howManyImputations")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# HSAUR3

<details>

* Version: 1.0-15
* GitHub: NA
* Source code: https://github.com/cran/HSAUR3
* Date/Publication: 2024-08-17 17:30:01 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::revdep_details(, "HSAUR3")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ...
    ```
      ‘Ch_analysing_longitudinal_dataI.Rnw’ using ‘UTF-8’... OK
      ‘Ch_analysing_longitudinal_dataII.Rnw’ using ‘UTF-8’... OK
      ‘Ch_analysis_of_variance.Rnw’ using ‘UTF-8’... OK
      ‘Ch_bayesian_inference.Rnw’ using ‘UTF-8’... OK
      ‘Ch_cluster_analysis.Rnw’ using ‘UTF-8’... OK
      ‘Ch_conditional_inference.Rnw’ using ‘UTF-8’... OK
      ‘Ch_density_estimation.Rnw’ using ‘UTF-8’... OK
      ‘Ch_errata.Rnw’ using ‘UTF-8’... OK
      ‘Ch_gam.Rnw’ using ‘UTF-8’... OK
      ‘Ch_graphical_display.Rnw’ using ‘UTF-8’... OK
    ...
      (10 observations deleted due to missingness)
    Multiple R-squared:  0.1545,	Adjusted R-squared:  0.1122 
    F-statistic: 3.653 on 2 and 40 DF,  p-value: 0.03489
    
    
    > library("mice")
    
      When sourcing 'Ch_missing_values.R':
    Error: there is no package called 'mice'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# idem

<details>

* Version: 5.2
* GitHub: https://github.com/olssol/idem
* Source code: https://github.com/cran/idem
* Date/Publication: 2023-08-09 10:30:09 UTC
* Number of recursive dependencies: 133

Run `revdepcheck::revdep_details(, "idem")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) imInfer.Rd:34-35: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) imInfer.Rd:35-36: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) plot.IDEMDATA.Rd:22-23: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) plot.IDEMDATA.Rd:25: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) plot.IDEMDATA.Rd:27: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) plot.IDEMIMP.Rd:14: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) plot.IDEMIMP.Rd:15: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) plot.IDEMIMP.Rd:27-28: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) plot.IDEMIMP.Rd:30-32: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) plot.IDEMIMP.Rd:34: Lost braces in \itemize; meant \describe ?
    ...
    checkRd: (-1) plot.IDEMIMP.Rd:60-63: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) plot.IDEMIMP.Rd:65-66: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) plot.IDEMIMP.Rd:69-70: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) plot.IDEMIMP.Rd:72-73: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) plot.IDEMIMP.Rd:75: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) summary.IDEMDATA.Rd:15: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) summary.IDEMDATA.Rd:17-18: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) summary.IDEMDATA.Rd:20: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) summary.IDEMINFER.Rd:14: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) summary.IDEMINFER.Rd:15: Lost braces in \itemize; meant \describe ?
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# ImputeRobust

<details>

* Version: 1.3-1
* GitHub: NA
* Source code: https://github.com/cran/ImputeRobust
* Date/Publication: 2018-11-30 12:10:03 UTC
* Number of recursive dependencies: 70

Run `revdepcheck::revdep_details(, "ImputeRobust")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# insight

<details>

* Version: 0.20.5
* GitHub: https://github.com/easystats/insight
* Source code: https://github.com/cran/insight
* Date/Publication: 2024-10-02 07:50:02 UTC
* Number of recursive dependencies: 405

Run `revdepcheck::revdep_details(, "insight")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# intmed

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/intmed
* Date/Publication: 2020-08-27 17:20:02 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::revdep_details(, "intmed")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# IPWboxplot

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/IPWboxplot
* Date/Publication: 2023-10-21 19:30:02 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::revdep_details(, "IPWboxplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘IPWboxplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: IPW.ASYM.boxplot
    > ### Title: Boxplot adapted to skewness and missing values
    > ### Aliases: IPW.ASYM.boxplot
    > ### Keywords: quantile boxplot missing inverse probability weighted
    > 
    > ### ** Examples
    > 
    > 
    > ## A real data example
    > 
    > 
    > library(mice)
    Error in library(mice) : there is no package called ‘mice’
    Execution halted
    ```

*   checking running R code from vignettes ...
    ```
      ‘my-vignette.Rmd’ using ‘UTF-8’... failed
     WARNING
    Errors in running code in vignettes:
    when running code in ‘my-vignette.Rmd’
      ...
    
    > knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
    
    > library(IPWboxplot)
    
    > library(mice)
    
      When sourcing ‘my-vignette.R’:
    Error: there is no package called ‘mice’
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# JWileymisc

<details>

* Version: 1.4.1
* GitHub: https://github.com/JWiley/JWileymisc
* Source code: https://github.com/cran/JWileymisc
* Date/Publication: 2023-10-05 04:50:02 UTC
* Number of recursive dependencies: 166

Run `revdepcheck::revdep_details(, "JWileymisc")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# konfound

<details>

* Version: 1.0.2
* GitHub: https://github.com/konfound-project/konfound
* Source code: https://github.com/cran/konfound
* Date/Publication: 2024-10-17 16:10:02 UTC
* Number of recursive dependencies: 166

Run `revdepcheck::revdep_details(, "konfound")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# lavaan.survey

<details>

* Version: 1.1.3.1
* GitHub: NA
* Source code: https://github.com/cran/lavaan.survey
* Date/Publication: 2016-12-22 23:43:24
* Number of recursive dependencies: 84

Run `revdepcheck::revdep_details(, "lavaan.survey")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

## In both

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      • On CRAN (2): 'test_roosma.R:40:3', 'test_roosma.R:61:3'
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test_roosma.R:56:3'): an estimate matches ─────────────────────────
      coef(fit.cfa.surv)["range~~range"] not equal to 1.892812.
      1/1 mismatches
      [1] 1.89 - 1.89 == 6.51e-05
      ── Failure ('test_roosma.R:57:3'): an estimate matches ─────────────────────────
      coef(fit.cfa.surv.wls.yb)["range~~range"] not equal to 2.369951.
      1/1 mismatches
      [1] 2.37 - 2.37 == 0.000104
      
      [ FAIL 2 | WARN 0 | SKIP 2 | PASS 4 ]
      Error: Test failures
      Execution halted
    ```

# LMMstar

<details>

* Version: 1.1.0
* GitHub: https://github.com/bozenne/LMMstar
* Source code: https://github.com/cran/LMMstar
* Date/Publication: 2024-05-12 21:43:11 UTC
* Number of recursive dependencies: 161

Run `revdepcheck::revdep_details(, "LMMstar")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(lmerTest)
      
      Attaching package: 'lmerTest'
      
      The following object is masked from 'package:lme4':
      
          lmer
      
      The following object is masked from 'package:stats':
      
          step
      
      > library(mice)
      Error in library(mice) : there is no package called 'mice'
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.0Mb
      sub-directories of 1Mb or more:
        R              2.0Mb
        doc-overview   2.2Mb
    ```

# logistf

<details>

* Version: 1.26.0
* GitHub: https://github.com/georgheinze/logistf
* Source code: https://github.com/cran/logistf
* Date/Publication: 2023-08-18 09:52:33 UTC
* Number of recursive dependencies: 66

Run `revdepcheck::revdep_details(, "logistf")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# manymome

<details>

* Version: 0.2.4
* GitHub: https://github.com/sfcheung/manymome
* Source code: https://github.com/cran/manymome
* Date/Publication: 2024-10-04 13:40:02 UTC
* Number of recursive dependencies: 157

Run `revdepcheck::revdep_details(, "manymome")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# marginaleffects

<details>

* Version: 0.23.0
* GitHub: https://github.com/vincentarelbundock/marginaleffects
* Source code: https://github.com/cran/marginaleffects
* Date/Publication: 2024-10-05 14:20:07 UTC
* Number of recursive dependencies: 463

Run `revdepcheck::revdep_details(, "marginaleffects")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# MatchThem

<details>

* Version: 1.2.1
* GitHub: https://github.com/FarhadPishgar/MatchThem
* Source code: https://github.com/cran/MatchThem
* Date/Publication: 2024-04-02 00:30:02 UTC
* Number of recursive dependencies: 89

Run `revdepcheck::revdep_details(, "MatchThem")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# mdapack

<details>

* Version: 0.0.2
* GitHub: NA
* Source code: https://github.com/cran/mdapack
* Date/Publication: 2020-05-20 22:30:02 UTC
* Number of recursive dependencies: 192

Run `revdepcheck::revdep_details(, "mdapack")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘FactoMineR’ ‘covr’ ‘curl’ ‘devtools’ ‘gh’ ‘git2r’ ‘grDevices’ ‘httr’
      ‘knitr’ ‘openssl’ ‘pkgbuild’ ‘rlang’ ‘rmarkdown’ ‘roxygen2’
      ‘spelling’ ‘testthat’ ‘utils’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# medflex

<details>

* Version: 0.6-10
* GitHub: https://github.com/jmpsteen/medflex
* Source code: https://github.com/cran/medflex
* Date/Publication: 2023-06-22 16:52:38 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::revdep_details(, "medflex")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# metavcov

<details>

* Version: 2.1.5
* GitHub: https://github.com/luminwin/metavcov
* Source code: https://github.com/cran/metavcov
* Date/Publication: 2023-06-24 04:00:02 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::revdep_details(, "metavcov")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# mi4p

<details>

* Version: 1.2
* GitHub: https://github.com/mariechion/mi4p
* Source code: https://github.com/cran/mi4p
* Date/Publication: 2024-10-02 22:40:17 UTC
* Number of recursive dependencies: 227

Run `revdepcheck::revdep_details(, "mi4p")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# micd

<details>

* Version: 1.1.1
* GitHub: https://github.com/bips-hb/micd
* Source code: https://github.com/cran/micd
* Date/Publication: 2023-02-17 17:40:02 UTC
* Number of recursive dependencies: 104

Run `revdepcheck::revdep_details(, "micd")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# miceadds

<details>

* Version: 3.17-44
* GitHub: https://github.com/alexanderrobitzsch/miceadds
* Source code: https://github.com/cran/miceadds
* Date/Publication: 2024-01-09 10:10:02 UTC
* Number of recursive dependencies: 155

Run `revdepcheck::revdep_details(, "miceadds")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    Packages which this enhances but not available for checking:
      'Amelia', 'imputeR', 'micemd', 'simputation'
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'Amelia', 'imputeR', 'micemd', 'simputation'
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘imputeR’, ‘simputation’
    ```

# miceafter

<details>

* Version: 0.5.0
* GitHub: https://github.com/mwheymans/miceafter
* Source code: https://github.com/cran/miceafter
* Date/Publication: 2022-10-02 13:30:02 UTC
* Number of recursive dependencies: 139

Run `revdepcheck::revdep_details(, "miceafter")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# miceFast

<details>

* Version: 0.8.2
* GitHub: https://github.com/Polkas/miceFast
* Source code: https://github.com/cran/miceFast
* Date/Publication: 2022-11-17 21:10:02 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::revdep_details(, "miceFast")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ...
    ```
      ‘miceFast-intro.Rmd’ using ‘UTF-8’... failed
     WARNING
    Errors in running code in vignettes:
    when running code in ‘miceFast-intro.Rmd’
      ...
    > knitr::opts_chunk$set(echo = FALSE)
    
    > pkgs <- c("miceFast", "mice", "ggplot2", "dplyr", 
    +     "data.table")
    
    > inst <- lapply(pkgs, library, character.only = TRUE)
    
      When sourcing ‘miceFast-intro.R’:
    Error: there is no package called ‘mice’
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

## In both

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) VIF.Rd:47: Lost braces; missing escapes or markup?
        47 | vif_corrected = vif_basic^{(1/(2*df))}
           |                           ^
    ```

# micemd

<details>

* Version: 1.10.0
* GitHub: NA
* Source code: https://github.com/cran/micemd
* Date/Publication: 2023-11-17 10:40:02 UTC
* Number of recursive dependencies: 138

Run `revdepcheck::revdep_details(, "micemd")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# microeco

<details>

* Version: 1.10.0
* GitHub: https://github.com/ChiLiubio/microeco
* Source code: https://github.com/cran/microeco
* Date/Publication: 2024-10-29 14:20:08 UTC
* Number of recursive dependencies: 136

Run `revdepcheck::revdep_details(, "microeco")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# midastouch

<details>

* Version: 1.3
* GitHub: NA
* Source code: https://github.com/cran/midastouch
* Date/Publication: 2016-02-07 09:35:46
* Number of recursive dependencies: 60

Run `revdepcheck::revdep_details(, "midastouch")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘midastouch-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: mice.impute.midastouch
    > ### Title: Predictive Mean Matching with distance aided selection of donors
    > ### Aliases: mice.impute.midastouch midastouch
    > ### Keywords: mice
    > 
    > ### ** Examples
    > 
    > ## from R:: mice, slightly adapted ##
    > 
    > # do default multiple imputation on a numeric matrix
    > library(midastouch)
    > library(mice)
    Error in library(mice) : there is no package called ‘mice’
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# midoc

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/midoc
* Date/Publication: 2024-10-02 16:40:02 UTC
* Number of recursive dependencies: 133

Run `revdepcheck::revdep_details(, "midoc")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# mifa

<details>

* Version: 0.2.0
* GitHub: https://github.com/teebusch/mifa
* Source code: https://github.com/cran/mifa
* Date/Publication: 2021-01-22 08:40:08 UTC
* Number of recursive dependencies: 116

Run `revdepcheck::revdep_details(, "mifa")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# MIIPW

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/MIIPW
* Date/Publication: 2023-02-13 23:00:02 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::revdep_details(, "MIIPW")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# misaem

<details>

* Version: 1.0.1
* GitHub: https://github.com/julierennes/misaem
* Source code: https://github.com/cran/misaem
* Date/Publication: 2021-04-12 08:10:02 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::revdep_details(, "misaem")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ...
    ```
      ‘misaem.Rmd’ using ‘UTF-8’... failed
     WARNING
    Errors in running code in vignettes:
    when running code in ‘misaem.Rmd’
      ...
    > sigma.eps <- 0.25
    
    > y <- cbind(rep(1, n), X.complete) %*% b + rnorm(n, 
    +     0, sigma.eps)
    
    > library(mice)
    
      When sourcing ‘misaem.R’:
    Error: there is no package called ‘mice’
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# miselect

<details>

* Version: 0.9.2
* GitHub: NA
* Source code: https://github.com/cran/miselect
* Date/Publication: 2024-03-05 17:00:08 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::revdep_details(, "miselect")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ...
    ```
      ‘miselect.Rmd’ using ‘UTF-8’... failed
     WARNING
    Errors in running code in vignettes:
    when running code in ‘miselect.Rmd’
      ...
            X8         X9        X10        X11        X12        X13        X14 
    0.27333333 0.22666667 0.22666667 0.21000000 0.25333333 0.32000000 0.37333333 
           X15        X16        X17        X18        X19        X20          Y 
    0.34000000 0.34666667 0.36333333 0.43000000 0.38000000 0.42666667 0.00000000 
    
    > library(mice)
    
      When sourcing ‘miselect.R’:
    Error: there is no package called ‘mice’
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# missCompare

<details>

* Version: 1.0.3
* GitHub: https://github.com/Tirgit/missCompare
* Source code: https://github.com/cran/missCompare
* Date/Publication: 2020-12-01 08:50:03 UTC
* Number of recursive dependencies: 212

Run `revdepcheck::revdep_details(, "missCompare")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# missDiag

<details>

* Version: 1.0.1
* GitHub: https://github.com/sumtxt/missDiag
* Source code: https://github.com/cran/missDiag
* Date/Publication: 2021-08-06 18:00:02 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::revdep_details(, "missDiag")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘missDiag-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: missDiag
    > ### Title: Comparing Observed and Imputed Values under MAR and MCAR
    > ### Aliases: missDiag
    > 
    > ### ** Examples
    > 
    > 
    ...
    > 
    > diag_rng <- missDiag( 
    +  original=anes08, 
    +  imputed=anes08_rng, 
    +  verbose = 1,
    +  adjust = 'none',
    +  formula = time ~ .)
    Error in loadNamespace(x) : there is no package called ‘mice’
    Calls: missDiag ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘mice’
    ```

# missMDA

<details>

* Version: 1.19
* GitHub: NA
* Source code: https://github.com/cran/missMDA
* Date/Publication: 2023-11-17 10:20:19 UTC
* Number of recursive dependencies: 126

Run `revdepcheck::revdep_details(, "missMDA")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# misty

<details>

* Version: 0.6.8
* GitHub: NA
* Source code: https://github.com/cran/misty
* Date/Publication: 2024-10-24 16:30:02 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::revdep_details(, "misty")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# mitml

<details>

* Version: 0.4-5
* GitHub: https://github.com/simongrund1/mitml
* Source code: https://github.com/cran/mitml
* Date/Publication: 2023-03-08 17:10:02 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::revdep_details(, "mitml")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mitml-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: mids2mitml.list
    > ### Title: Convert objects of class 'mids' to 'mitml.list'
    > ### Aliases: mids2mitml.list
    > 
    > ### ** Examples
    > 
    > data(studentratings)
    > 
    > # imputation using mice
    > require(mice)
    Loading required package: mice
    Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,  :
      there is no package called ‘mice’
    > imp <- mice(studentratings)
    Error in mice(studentratings) : could not find function "mice"
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘mice’
    ```

# miWQS

<details>

* Version: 0.4.4
* GitHub: https://github.com/phargarten2/miWQS
* Source code: https://github.com/cran/miWQS
* Date/Publication: 2021-04-02 21:50:02 UTC
* Number of recursive dependencies: 148

Run `revdepcheck::revdep_details(, "miWQS")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘mice’
    ```

# mixgb

<details>

* Version: 1.0.2
* GitHub: https://github.com/agnesdeng/mixgb
* Source code: https://github.com/cran/mixgb
* Date/Publication: 2023-02-16 11:00:02 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::revdep_details(, "mixgb")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# MixtureMissing

<details>

* Version: 3.0.3
* GitHub: NA
* Source code: https://github.com/cran/MixtureMissing
* Date/Publication: 2024-10-15 21:50:02 UTC
* Number of recursive dependencies: 67

Run `revdepcheck::revdep_details(, "MixtureMissing")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# MKinfer

<details>

* Version: 1.2
* GitHub: https://github.com/stamats/MKinfer
* Source code: https://github.com/cran/MKinfer
* Date/Publication: 2024-04-06 10:42:58 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::revdep_details(, "MKinfer")` for more info

</details>

## Newly broken

*   checking whether package ‘MKinfer’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/MKinfer/new/MKinfer.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

## Installation

### Devel

```
* installing *source* package ‘MKinfer’ ...
** package ‘MKinfer’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘mice’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘MKinfer’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/MKinfer/new/MKinfer.Rcheck/MKinfer’


```
### CRAN

```
* installing *source* package ‘MKinfer’ ...
** package ‘MKinfer’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (MKinfer)


```
# mlim

<details>

* Version: 0.3.0
* GitHub: https://github.com/haghish/mlim
* Source code: https://github.com/cran/mlim
* Date/Publication: 2022-12-16 13:00:02 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::revdep_details(, "mlim")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) mlim.Rd:58: Lost braces
        58 |              Note that code{"XGB"} is only available in Mac OS and Linux. moreover,
           |                            ^
    ```

# modelsummary

<details>

* Version: 2.2.0
* GitHub: https://github.com/vincentarelbundock/modelsummary
* Source code: https://github.com/cran/modelsummary
* Date/Publication: 2024-09-02 21:20:02 UTC
* Number of recursive dependencies: 298

Run `revdepcheck::revdep_details(, "modelsummary")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# monoClust

<details>

* Version: 1.2.1
* GitHub: https://github.com/vinhtantran/monoClust
* Source code: https://github.com/cran/monoClust
* Date/Publication: 2021-02-15 15:00:02 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::revdep_details(, "monoClust")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) circ_dist.Rd:21: Lost braces; missing escapes or markup?
        21 | The distance between two observations {i} and {j} of a circular variable {q}
           |                                       ^
    checkRd: (-1) circ_dist.Rd:21: Lost braces; missing escapes or markup?
        21 | The distance between two observations {i} and {j} of a circular variable {q}
           |                                               ^
    checkRd: (-1) circ_dist.Rd:21: Lost braces; missing escapes or markup?
        21 | The distance between two observations {i} and {j} of a circular variable {q}
           |                                                                          ^
    ```

# MRPC

<details>

* Version: 3.1.0
* GitHub: NA
* Source code: https://github.com/cran/MRPC
* Date/Publication: 2022-04-11 14:32:34 UTC
* Number of recursive dependencies: 169

Run `revdepcheck::revdep_details(, "MRPC")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# MSiP

<details>

* Version: 1.3.7
* GitHub: NA
* Source code: https://github.com/cran/MSiP
* Date/Publication: 2021-06-17 08:20:05 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::revdep_details(, "MSiP")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ranger’
      All declared Imports should be used.
    ```

# mvnimpute

<details>

* Version: 1.0.1
* GitHub: https://github.com/hli226/mvnimpute
* Source code: https://github.com/cran/mvnimpute
* Date/Publication: 2022-07-06 09:40:02 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::revdep_details(, "mvnimpute")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# mvs

<details>

* Version: 2.0.0
* GitHub: NA
* Source code: https://github.com/cran/mvs
* Date/Publication: 2024-08-29 13:00:02 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::revdep_details(, "mvs")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        |====================================================                  |  75%
        |                                                                            
        |======================================================================| 100%[ FAIL 1 | WARN 0 | SKIP 0 | PASS 15 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test_missing_data.R:19:3'): missing_data_StaPLR ─────────────────────
      Error in `StaPLR(X, y, view_index, seed = 123, na.action = "mice", na.arguments = list(m = 10, 
          method = "mean"))`: Package `mice` is required, but not installed.
      Backtrace:
          ▆
       1. └─mvs::StaPLR(...) at test_missing_data.R:19:3
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 15 ]
      Error: Test failures
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# NADIA

<details>

* Version: 0.4.2
* GitHub: https://github.com/ModelOriented/EMMA
* Source code: https://github.com/cran/NADIA
* Date/Publication: 2022-10-02 19:40:02 UTC
* Number of recursive dependencies: 183

Run `revdepcheck::revdep_details(, "NADIA")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking examples ... ERROR
    ```
    Running examples in ‘NADIA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: PipeOpOOR_B
    > ### Title: PipeOpOOR_B
    > ### Aliases: PipeOpOOR_B
    > 
    > ### ** Examples
    > 
    > {
    ...
    +   graph <- PipeOpOOR_B$new() %>>% LearnerClassifDebug$new()
    +   graph_learner <- GraphLearner$new(graph)
    + 
    +   # Task with NA
    +   set.seed(1)
    +   resample(tsk("pima"), graph_learner, rsmp("cv", folds = 3))
    + }
    Error in initialize(...) : object 'ParamLgl' not found
    Calls: %>>% ... .__ParamSet__initialize -> assert_list -> checkList -> %and% -> isTRUE
    Execution halted
    ```

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. └─PipeOpSimulateMissings$new()
        5.   └─NADIA (local) initialize(...)
        6.     └─ParamSet$new(...)
        7.       └─paradox (local) initialize(...)
        8.         └─paradox:::.__ParamSet__initialize(...)
        9.           └─checkmate::assert_list(params, types = "Domain")
       10.             └─checkmate::checkList(...)
       11.               └─... %and% checkListTypes(x, types)
       12.                 └─base::isTRUE(lhs)
      
      [ FAIL 5 | WARN 0 | SKIP 12 | PASS 47 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ...
    ```
      ‘Errors_Statistic_and_Handling.Rmd’ using ‘UTF-8’... failed
      ‘NADIA_examples_and_motivation.Rmd’ using ‘UTF-8’... failed
     ERROR
    Errors in running code in vignettes:
    when running code in ‘Errors_Statistic_and_Handling.Rmd’
      ...
       <td style="text-align:left;font-weight: bold;color: white !important;background-color: rgba(245, 169, 169, 255) !important;"> 13/25 </td>
       <td style="text-align:left;font-weight: bold;color: white !important;background-color: rgba(245, 169, 169, 255) !important;"> 52% </td>
      </tr>
    </tbody>
    ...
    attr(,"class")
    [1] "knit_image_paths" "knit_asis"       
    
    > task_with_missing <- tsk("pima")
    
    > imputation_methods <- PipeOpMice$new()
    
      When sourcing ‘NADIA_examples_and_motivation.R’:
    Error: object 'ParamDbl' not found
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘glmnet’ ‘mlr3learners’ ‘rpart’
      All declared Imports should be used.
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) PipeOpMice.Rd:33: Lost braces; missing escapes or markup?
        33 | set of methods to chose. Avalible methods {"pmm", "midastouch", "sample", "cart", "rf"} Default 'pmm'. If seted on NULL this methods are used predictive mean matching (numeric data) logreg, logistic regression imputation (binary data, factor with 2 levels) polyreg, polytomous regression imputation for unordered categorical data (factor > 2 levels) polr, proportional odds model for (ordered, > 2 levels).
           |                                           ^
    ```

# NIMAA

<details>

* Version: 0.2.1
* GitHub: https://github.com/jafarilab/NIMAA
* Source code: https://github.com/cran/NIMAA
* Date/Publication: 2022-04-11 14:12:45 UTC
* Number of recursive dependencies: 177

Run `revdepcheck::revdep_details(, "NIMAA")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        doc   4.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 24 marked UTF-8 strings
    ```

# nncc

<details>

* Version: 2.0.0
* GitHub: NA
* Source code: https://github.com/cran/nncc
* Date/Publication: 2024-01-11 14:10:02 UTC
* Number of recursive dependencies: 108

Run `revdepcheck::revdep_details(, "nncc")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ...
    ```
      ‘nncc.Rmd’ using ‘UTF-8’... failed
     WARNING
    Errors in running code in vignettes:
    when running code in ‘nncc.Rmd’
      ...
    > results_clogit[["exp27"]] %>% summary() %>% `$`(conf.int)
         exp(coef)   exp(-coef) lower .95 upper .95
    exp1  11766455 8.498736e-08         0       Inf
    
    > library(logistf)
    
      When sourcing ‘nncc.R’:
    Error: package or namespace load failed for ‘logistf’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
     there is no package called ‘mice’
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# ordbetareg

<details>

* Version: 0.7.2
* GitHub: https://github.com/saudiwin/ordbetareg_pack
* Source code: https://github.com/cran/ordbetareg
* Date/Publication: 2023-08-10 07:30:02 UTC
* Number of recursive dependencies: 177

Run `revdepcheck::revdep_details(, "ordbetareg")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ...
    ```
      ‘package_introduction.Rmd’ using ‘UTF-8’... failed
     WARNING
    Errors in running code in vignettes:
    when running code in ‘package_introduction.Rmd’
      ...
    +     cutpoints = c(-2, 2))
    
    > X[runif(n = 100) < 0.1] <- NA
    
    > mult_impute <- mice::mice(data = tibble(outcome = outcome, 
    +     X = X), m = 2, printFlag = FALSE) %>% mice::complete(action = "all")
    
      When sourcing ‘package_introduction.R’:
    Error: there is no package called ‘mice’
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        data   5.1Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 36 marked UTF-8 strings
    ```

# OTrecod

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/OTrecod
* Date/Publication: 2022-10-05 10:40:02 UTC
* Number of recursive dependencies: 175

Run `revdepcheck::revdep_details(, "OTrecod")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) imput_cov.Rd:79: Lost braces
        79 | \item van Buuren, S., Groothuis-Oudshoorn, K. (2011). mice: Multivariate Imputation by Chained Equations in R. Journal of Statistical Software, 45(3), 1–67. url{https://www.jstatsoft.org/v45/i03/}
           |                                                                                                                                                                 ^
    checkRd: (-1) indiv_grp_closest.Rd:47: Lost braces; missing escapes or markup?
        47 | The function \code{indiv_grp_closest} is an intermediate function used in the implementation of an algorithm called {OUTCOME} (and its enrichment {R-OUTCOME}, see the reference (2) for more details) dedicated to the solving of recoding problems in data fusion using Optimal Transportation theory.
           |                                                                                                                     ^
    checkRd: (-1) indiv_grp_closest.Rd:47: Lost braces; missing escapes or markup?
        47 | The function \code{indiv_grp_closest} is an intermediate function used in the implementation of an algorithm called {OUTCOME} (and its enrichment {R-OUTCOME}, see the reference (2) for more details) dedicated to the solving of recoding problems in data fusion using Optimal Transportation theory.
           |                                                                                                                                                   ^
    checkRd: (-1) merge_dbs.Rd:179: Lost braces
       179 | \item van Buuren, S., Groothuis-Oudshoorn, K. (2011). mice: Multivariate Imputation by Chained Equations in R. Journal of Statistical Software, 45(3), 1–67. url{https://www.jstatsoft.org/v45/i03/}
           |                                                                                                                                                                 ^
    ```

# parameters

<details>

* Version: 0.23.0
* GitHub: https://github.com/easystats/parameters
* Source code: https://github.com/cran/parameters
* Date/Publication: 2024-10-18 11:10:06 UTC
* Number of recursive dependencies: 448

Run `revdepcheck::revdep_details(, "parameters")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# pema

<details>

* Version: 0.1.3
* GitHub: https://github.com/cjvanlissa/pema
* Source code: https://github.com/cran/pema
* Date/Publication: 2023-03-16 11:40:02 UTC
* Number of recursive dependencies: 131

Run `revdepcheck::revdep_details(, "pema")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Chain 4:                0.01 seconds (Total)
      Chain 4: 
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 19 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-multiple_imp.R:8:1'): (code run outside of `test_that()`) ──────
      <packageNotFoundError/error/condition>
      Error in `library(mice)`: there is no package called 'mice'
      Backtrace:
          ▆
       1. └─base::library(mice) at test-multiple_imp.R:8:1
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 19 ]
      Error: Test failures
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘mice’
    ```

## In both

*   checking running R code from vignettes ...
    ```
      ‘using-brma.Rmd’ using ‘UTF-8’... failed
     ERROR
    Errors in running code in vignettes:
    when running code in ‘using-brma.Rmd’
      ...
    domainsocial                   0.1702  0.1183  -0.0249  0.4232  1600    1
    domainnoMeta                  -0.3615  0.1843  -0.7299 -0.0184  1212    1
    sexM                           0.1205  0.0689  -0.0017  0.2605  1602    1
    tau2                           0.4291  0.0394   0.3524  0.5098  1112    1
    
    > I2(fit_lasso)
    
      When sourcing ‘using-brma.R’:
    Error: object 'fit_lasso' not found
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 10.1Mb
      sub-directories of 1Mb or more:
        libs   9.1Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# pguIMP

<details>

* Version: 0.0.0.3
* GitHub: https://github.com/SMLMS/pguIMP
* Source code: https://github.com/cran/pguIMP
* Date/Publication: 2021-09-30 11:50:02 UTC
* Number of recursive dependencies: 231

Run `revdepcheck::revdep_details(, "pguIMP")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking whether package ‘pguIMP’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/pguIMP/old/pguIMP.Rcheck/00install.out’ for details.
    ```

# pre

<details>

* Version: 1.0.7
* GitHub: https://github.com/marjoleinF/pre
* Source code: https://github.com/cran/pre
* Date/Publication: 2024-01-12 19:30:02 UTC
* Number of recursive dependencies: 150

Run `revdepcheck::revdep_details(, "pre")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ...
    ```
      ‘missingness.Rmd’ using ‘UTF-8’... failed
      ‘relaxed.Rmd’ using ‘UTF-8’... OK
      ‘speed.Rmd’ using ‘UTF-8’... OK
      ‘tuning.Rmd’ using ‘UTF-8’... OK
     WARNING
    Errors in running code in vignettes:
    when running code in ‘missingness.Rmd’
      ...
    6    28      NA 14.9   66     5   6
    
    > nrow(airquality)
    [1] 153
    
    > library("mice")
    
      When sourcing ‘missingness.R’:
    Error: there is no package called ‘mice’
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# psfmi

<details>

* Version: 1.4.0
* GitHub: https://github.com/mwheymans/psfmi
* Source code: https://github.com/cran/psfmi
* Date/Publication: 2023-06-17 22:40:02 UTC
* Number of recursive dependencies: 163

Run `revdepcheck::revdep_details(, "psfmi")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# qgcomp

<details>

* Version: 2.15.2
* GitHub: https://github.com/alexpkeil1/qgcomp
* Source code: https://github.com/cran/qgcomp
* Date/Publication: 2023-08-10 09:10:06 UTC
* Number of recursive dependencies: 155

Run `revdepcheck::revdep_details(, "qgcomp")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘mice’
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) qgcomp.survcurve.boot.Rd:17-19: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) qgcomp.survcurve.boot.Rd:20-21: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) qgcomp.survcurve.boot.Rd:22-23: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) qgcomp.survcurve.boot.Rd:24-25: Lost braces in \itemize; \value handles \item{}{} directly
    ```

# Qtools

<details>

* Version: 1.5.9
* GitHub: NA
* Source code: https://github.com/cran/Qtools
* Date/Publication: 2023-10-28 15:10:02 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::revdep_details(, "Qtools")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# RBtest

<details>

* Version: 1.1
* GitHub: NA
* Source code: https://github.com/cran/RBtest
* Date/Publication: 2020-03-03 15:00:03 UTC
* Number of recursive dependencies: 63

Run `revdepcheck::revdep_details(, "RBtest")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# realTimeloads

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/realTimeloads
* Date/Publication: 2023-10-18 14:00:02 UTC
* Number of recursive dependencies: 117

Run `revdepcheck::revdep_details(, "realTimeloads")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking examples ... ERROR
    ```
    Running examples in ‘realTimeloads-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: impute_data
    > ### Title: Returns x with gaps imputed using ARIMA and Decision Trees,
    > ###   optional uncertainty estimation using Monte Carlo resampling
    > ### Aliases: impute_data
    > 
    > ### ** Examples
    > 
    ...
    > idata <- sample(1:length(xo),round(length(xo)*0.5),replace=FALSE)
    > x <- rep(NA,length(xo))
    > x[idata] <- xo[idata] # simulated samples
    > flow_concentrtion_ratio <- imputeTS::na_interpolation(Q/x)
    > Xreg <- cbind(Q,flow_concentrtion_ratio)
    > Output <- impute_data(time,x,Xreg,MC = 10,ptrain = 0.8)
    Error in optim(init[mask], getLike, method = "L-BFGS-B", lower = rep(0,  : 
      L-BFGS-B needs finite values of 'fn'
    Calls: impute_data -> <Anonymous> -> <Anonymous> -> optim
    Execution halted
    ```

# RefBasedMI

<details>

* Version: 0.2.0
* GitHub: https://github.com/UCL/RefBasedMI
* Source code: https://github.com/cran/RefBasedMI
* Date/Publication: 2024-09-09 09:10:01 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::revdep_details(, "RefBasedMI")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# regmedint

<details>

* Version: 1.0.1
* GitHub: https://github.com/kaz-yos/regmedint
* Source code: https://github.com/cran/regmedint
* Date/Publication: 2024-01-13 00:50:02 UTC
* Number of recursive dependencies: 153

Run `revdepcheck::revdep_details(, "regmedint")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ...
    ```
      ‘vig_01_introduction.Rmd’ using ‘UTF-8’... OK
      ‘vig_02_formulas.Rmd’ using ‘UTF-8’... OK
      ‘vig_03_bootstrap.Rmd’ using ‘UTF-8’... OK
      ‘vig_04_mi.Rmd’ using ‘UTF-8’... failed
      ‘vig_05_emm.Rmd’ using ‘UTF-8’... OK
      ‘vig_06_delta_boot.Rmd’ using ‘UTF-8’... OK
     WARNING
    Errors in running code in vignettes:
    when running code in ‘vig_04_mi.Rmd’
      ...
    ...
    cvar: c
     c_cond (covariate vector value) = 0.5
    
    Note that effect estimates can vary over m_cde and c_cond values when interaction = TRUE.
    
    > library(mice)
    
      When sourcing ‘vig_04_mi.R’:
    Error: there is no package called ‘mice’
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Deriv’
      All declared Imports should be used.
    ```

# RegularizedSCA

<details>

* Version: 0.5.4
* GitHub: NA
* Source code: https://github.com/cran/RegularizedSCA
* Date/Publication: 2018-06-07 17:43:45 UTC
* Number of recursive dependencies: 142

Run `revdepcheck::revdep_details(, "RegularizedSCA")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking Rd metadata ... NOTE
    ```
    Invalid package aliases in Rd file 'RSCA.Rd':
      ‘RSCA-package’
    ```

# Replication

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/Replication
* Date/Publication: 2020-04-09 12:10:02 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::revdep_details(, "Replication")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking whether package ‘Replication’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/Replication/old/Replication.Rcheck/00install.out’ for details.
    ```

# rexposome

<details>

* Version: 1.28.0
* GitHub: NA
* Source code: https://github.com/cran/rexposome
* Date/Publication: 2024-10-29
* Number of recursive dependencies: 175

Run `revdepcheck::revdep_details(, "rexposome")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking for portable file names ... WARNING
    ```
    Found the following file with a non-portable file name:
      vignettes/rsconnect/documents/exposome_data_analysis.Rmd/rpubs.com/rpubs/Publish Document.dcf
    These are not fully portable file names.
    See section ‘Package structure’ in the ‘Writing R Extensions’ manual.
    Found the following non-portable file path:
      rexposome/vignettes/rsconnect/documents/exposome_data_analysis.Rmd/rpubs.com/rpubs/Publish Document.dcf
    
    Tarballs are only required to store paths of up to 100 bytes and cannot
    store those of more than 256 bytes, with restrictions including to 100
    bytes for the final component.
    See section ‘Package structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking dependencies in R code ... WARNING
    ```
    '::' or ':::' imports not declared from:
      ‘missMDA’ ‘nnet’ ‘sandwich’
    ```

*   checking Rd metadata ... WARNING
    ```
    Rd files with duplicated alias 'extract':
      ‘extract-methods.Rd’ ‘get_robust_sd-methods.Rd’
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    S4 class codoc mismatches from Rd file 'ExWAS-class.Rd':
    Slots for class 'ExWAS'
      Code: comparison description effective formula robust.std.err
      Docs: comparison description effective formula
    ```

*   checking Rd \usage sections ... WARNING
    ```
    Undocumented arguments in Rd file 'ExposomeSet-class.Rd'
      ‘robust’
    
    Undocumented arguments in Rd file 'exwas-methods.Rd'
      ‘robust’
    
    Undocumented arguments in Rd file 'imExposomeSet-class.Rd'
      ‘robust’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 12.4Mb
      sub-directories of 1Mb or more:
        data      2.8Mb
        doc       6.4Mb
        extdata   2.2Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    License stub is invalid DCF.
    ```

*   checking R code for possible problems ... NOTE
    ```
    imputeLOD: multiple local function definitions for ‘faux’ with
      different formal arguments
    invExWAS,ExposomeSet : <anonymous>: no visible global function
      definition for ‘reformulate’
    invExWAS,ExposomeSet : <anonymous>: no visible global function
      definition for ‘terms’
    invExWAS,ExposomeSet : <anonymous>: no visible global function
      definition for ‘confint’
    invExWAS,ExposomeSet: no visible global function definition for
      ‘reformulate’
    invExWAS,ExposomeSet: no visible global function definition for ‘terms’
    plotHistogram,ExposomeSet: no visible binding for global variable
      ‘..density..’
    Undefined global functions or variables:
      ..density.. confint reformulate terms
    Consider adding
      importFrom("stats", "confint", "reformulate", "terms")
    to your NAMESPACE file.
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) exwas-methods.Rd:50: Lost braces
        50 | An code{ExWAS} object with the result of the association study
           |        ^
    checkRd: (-1) invExWAS-methods.Rd:31: Lost braces
        31 | An code{ExWAS} object with the result of the association study
           |        ^
    checkRd: (-1) standardize-methods.Rd:13: Lost braces
        13 | #' @param object code{ExposomeSet} with 'set' will be summarized.
           |                      ^
    ```

# RfEmpImp

<details>

* Version: 2.1.8
* GitHub: https://github.com/shangzhi-hong/RfEmpImp
* Source code: https://github.com/cran/RfEmpImp
* Date/Publication: 2022-10-20 07:17:54 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::revdep_details(, "RfEmpImp")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# rms

<details>

* Version: 6.8-2
* GitHub: https://github.com/harrelfe/rms
* Source code: https://github.com/cran/rms
* Date/Publication: 2024-08-23 05:10:02 UTC
* Number of recursive dependencies: 145

Run `revdepcheck::revdep_details(, "rms")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# RNAseqCovarImpute

<details>

* Version: 1.4.0
* GitHub: https://github.com/brennanhilton/RNAseqCovarImpute
* Source code: https://github.com/cran/RNAseqCovarImpute
* Date/Publication: 2024-10-29
* Number of recursive dependencies: 145

Run `revdepcheck::revdep_details(, "RNAseqCovarImpute")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking R code for possible problems ... NOTE
    ```
    combine_rubins: no visible binding for global variable ‘probe’
    combine_rubins: no visible binding for global variable ‘i’
    combine_rubins: no visible binding for global variable ‘coef_combined’
    combine_rubins: no visible binding for global variable ‘rubins_t_bayes’
    combine_rubins: no visible binding for global variable ‘combined_p’
    combine_rubins: no visible binding for global variable
      ‘combined_p_bayes’
    limmavoom_imputed_data_list_helper: no visible binding for global
      variable ‘i’
    limmavoom_imputed_data_list_helper: no visible binding for global
    ...
    voom_sx_sy: no visible global function definition for ‘is’
    voom_sx_sy: no visible global function definition for ‘new’
    Undefined global functions or variables:
      approxfun coef_combined combined_p combined_p_bayes gene_bin i is
      lm_predictor lowess new probe rubins_t_bayes
    Consider adding
      importFrom("methods", "is", "new")
      importFrom("stats", "approxfun", "lowess")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

*   checking Rd \usage sections ... NOTE
    ```
    Documented arguments not in \usage in Rd file 'impute_gene_bin_helper.Rd':
      ‘DGE’ ‘param’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

# rqlm

<details>

* Version: 2.1-1
* GitHub: NA
* Source code: https://github.com/cran/rqlm
* Date/Publication: 2024-05-23 21:50:03 UTC
* Number of recursive dependencies: 62

Run `revdepcheck::revdep_details(, "rqlm")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# RSquaredMI

<details>

* Version: 0.1.1
* GitHub: https://github.com/karchjd/RsquaredMI
* Source code: https://github.com/cran/RSquaredMI
* Date/Publication: 2024-09-19 16:00:07 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::revdep_details(, "RSquaredMI")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# semmcci

<details>

* Version: 1.1.4
* GitHub: https://github.com/jeksterslab/semmcci
* Source code: https://github.com/cran/semmcci
* Date/Publication: 2024-03-17 18:30:02 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::revdep_details(, "semmcci")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# semTools

<details>

* Version: 0.5-6
* GitHub: https://github.com/simsem/semTools
* Source code: https://github.com/cran/semTools
* Date/Publication: 2022-05-10 07:00:02 UTC
* Number of recursive dependencies: 129

Run `revdepcheck::revdep_details(, "semTools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘semTools-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: combinequark
    > ### Title: Combine the results from the quark function
    > ### Aliases: combinequark
    > 
    > ### ** Examples
    > 
    > 
    ...
    Data Check Passed.
    ID Check Passed.
    Loading required namespace: mice
    Failed with error:  ‘there is no package called ‘mice’’
    The quark function requires the "mice" package to be installed.
    > 
    > final.data <- combinequark(quark = quark.list, percent = 80)
    Error in if (pct[i] >= percent) { : argument is of length zero
    Calls: combinequark
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘mice’
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) PAVranking.Rd:77-79: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) PAVranking.Rd:80-82: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) PAVranking.Rd:83-85: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) PAVranking.Rd:86-87: Lost braces in \itemize; \value handles \item{}{} directly
    ```

# seqimpute

<details>

* Version: 2.1.0
* GitHub: https://github.com/emerykevin/seqimpute
* Source code: https://github.com/cran/seqimpute
* Date/Publication: 2024-11-13 12:20:02 UTC
* Number of recursive dependencies: 152

Run `revdepcheck::revdep_details(, "seqimpute")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# shapeNA

<details>

* Version: 0.0.2
* GitHub: NA
* Source code: https://github.com/cran/shapeNA
* Date/Publication: 2021-03-15 09:10:02 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::revdep_details(, "shapeNA")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘shapeNA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: barplot.shapeNA
    > ### Title: Barplot Showcasing Missingness Proportion of the Original Data
    > ### Aliases: barplot.shapeNA
    > 
    > ### ** Examples
    > 
    >     S <- toeplitz(seq(1, 0.1, length.out = 3))
    >     x <- mvtnorm::rmvt(100, S, df = 5)
    >     y <- mice::ampute(x, mech='MCAR')$amp
    Error in loadNamespace(x) : there is no package called ‘mice’
    Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stats’
      All declared Imports should be used.
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) powerShape.Rd:30: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) powerShape.Rd:31: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) powerShape.Rd:32-33: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) powerShapeNA.Rd:30: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) powerShapeNA.Rd:31: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) powerShapeNA.Rd:32-33: Lost braces in \itemize; meant \describe ?
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# sjmisc

<details>

* Version: 2.8.10
* GitHub: https://github.com/strengejacke/sjmisc
* Source code: https://github.com/cran/sjmisc
* Date/Publication: 2024-05-13 13:50:02 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::revdep_details(, "sjmisc")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘mice’
    ```

# smdi

<details>

* Version: 0.3.1
* GitHub: NA
* Source code: https://github.com/cran/smdi
* Date/Publication: 2024-10-04 07:10:02 UTC
* Number of recursive dependencies: 217

Run `revdepcheck::revdep_details(, "smdi")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking running R code from vignettes ...
    ```
      ‘a_data_generation.Rmd’ using ‘UTF-8’... failed
      ‘b_routine_diagnostics.Rmd’ using ‘UTF-8’... failed
      ‘c_multivariate_missingness.Rmd’ using ‘UTF-8’... OK
      ‘d_narfcs_sensitivity_analysis.Rmd’ using ‘UTF-8’... OK
      ‘smdi.Rmd’ using ‘UTF-8’... OK
     ERROR
    Errors in running code in vignettes:
    when running code in ‘a_data_generation.Rmd’
      ...
      restarting interrupted promise evaluation
    ...
    > library(here)
    here() starts at /private/var/folders/5_/g85d42yj50b6lrjq4rzjzg8w0000gn/T/RtmpTlJyql/filebdb5236ac387/vignettes
    
    > library(knitr)
    
    > include_graphics(here("vignettes", "smdi_diagnose_table.png"))
    
      When sourcing ‘b_routine_diagnostics.R’:
    Error: Cannot find the file(s): "/private/var/folders/5_/g85d42yj50b6lrjq4rzjzg8w0000gn/T/RtmpTlJyql/filebdb5236ac387/vignettes/vignettes/smdi_diagnose_table.png"
    Execution halted
    ```

# sociome

<details>

* Version: 2.2.5
* GitHub: https://github.com/ClevelandClinicQHS/sociome
* Source code: https://github.com/cran/sociome
* Date/Publication: 2023-12-06 12:10:02 UTC
* Number of recursive dependencies: 117

Run `revdepcheck::revdep_details(, "sociome")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# StackImpute

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/StackImpute
* Date/Publication: 2021-09-10 11:10:02 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::revdep_details(, "StackImpute")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MASS’ ‘mice’ ‘sandwich’ ‘zoo’
      All declared Imports should be used.
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) stackExample.Rd:10: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) stackExample.Rd:11: Lost braces in \itemize; meant \describe ?
    ```

# superMICE

<details>

* Version: 1.1.1
* GitHub: NA
* Source code: https://github.com/cran/superMICE
* Date/Publication: 2022-05-04 20:00:02 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::revdep_details(, "superMICE")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    Package suggested but not available for checking: ‘extraTrees’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘extraTrees’
    ```

# svyweight

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/svyweight
* Date/Publication: 2022-05-03 10:00:02 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::revdep_details(, "svyweight")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘mice’
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) eff_n.Rd:32: Lost braces; missing escapes or markup?
        32 | \code{\link[survey:surveysummary]{survey::svymean()}} and related functions from the {survey}
           |                                                                                      ^
    ```

# SynDI

<details>

* Version: 0.1.0
* GitHub: https://github.com/umich-biostatistics/SynDI
* Source code: https://github.com/cran/SynDI
* Date/Publication: 2022-05-25 07:50:05 UTC
* Number of recursive dependencies: 75

Run `revdepcheck::revdep_details(, "SynDI")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MASS’ ‘StackImpute’ ‘arm’ ‘boot’ ‘broom’ ‘knitr’ ‘mvtnorm’
      ‘randomForest’
      All declared Imports should be used.
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) create_synthetic_example.Rd:10: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) create_synthetic_example.Rd:11: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) create_synthetic_example.Rd:12: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) initial_estimates_example.Rd:10: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) initial_estimates_example.Rd:11: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) initial_estimates_example.Rd:12: Lost braces in \itemize; meant \describe ?
    ```

# synergyfinder

<details>

* Version: 3.14.0
* GitHub: NA
* Source code: https://github.com/cran/synergyfinder
* Date/Publication: 2024-10-29
* Number of recursive dependencies: 196

Run `revdepcheck::revdep_details(, "synergyfinder")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

## Newly fixed

*   checking dependencies in R code ... WARNING
    ```
    '::' or ':::' import not declared from: ‘scales’
    Namespaces in Imports field not imported from:
      ‘future’ ‘gstat’ ‘nleqslv’ ‘sp’ ‘tidyverse’
      All declared Imports should be used.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        doc   5.8Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    .Extract2DrugPlotData: no visible binding for global variable
      ‘input_type’
    .Extract2DrugPlotData: no visible binding for global variable
      ‘block_id’
    .Extract2DrugPlotData: no visible binding for global variable ‘value’
    .Extract2DrugPlotData: no visible binding for global variable ‘left’
    .Extract2DrugPlotData: no visible binding for global variable ‘right’
    .Extract2DrugPlotData: no visible binding for global variable ‘conc1’
    .Extract2DrugPlotData: no visible binding for global variable ‘conc2’
    .Extract2DrugPlotData: no visible binding for global variable ‘text’
    ...
      response_CI95 response_mean response_origin response_origin_CI95
      response_origin_mean response_origin_sd response_origin_sem
      response_sd response_sem right start synergy t.test text theta value
      x y
    Consider adding
      importFrom("grDevices", "dev.list", "dev.off")
      importFrom("graphics", "text")
      importFrom("stats", "end", "predict", "start", "t.test")
      importFrom("utils", "data", "head")
    to your NAMESPACE file.
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) FitDoseResponse.Rd:54: Lost braces
        54 | href{https://onlinelibrary.wiley.com/doi/book/10.1002/0471725315}{Nonlinear
           |     ^
    checkRd: (-1) FitDoseResponse.Rd:54-55: Lost braces
        54 | href{https://onlinelibrary.wiley.com/doi/book/10.1002/0471725315}{Nonlinear
           |                                                                  ^
    checkRd: (-1) FitDoseResponse.Rd:55: Escaped LaTeX specials: \&
    checkRd: (-1) PlotBarometer.Rd:118: Lost braces
       118 | href{https://www.frontiersin.org/articles/10.3389/fphar.2015.00181/full}{What
           |     ^
    checkRd: (-1) PlotBarometer.Rd:118-119: Lost braces
       118 | href{https://www.frontiersin.org/articles/10.3389/fphar.2015.00181/full}{What
           |                                                                         ^
    checkRd: (-1) PlotDoseResponseCurve.Rd:84: Lost braces
        84 | link[drc]{plot.drc} function. For example, use xlim = c(0.5, 500) or
           |          ^
    ```

# TestDataImputation

<details>

* Version: 2.3
* GitHub: NA
* Source code: https://github.com/cran/TestDataImputation
* Date/Publication: 2021-10-18 18:10:11 UTC
* Number of recursive dependencies: 63

Run `revdepcheck::revdep_details(, "TestDataImputation")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# tidySEM

<details>

* Version: 0.2.7
* GitHub: https://github.com/cjvanlissa/tidySEM
* Source code: https://github.com/cran/tidySEM
* Date/Publication: 2024-06-04 09:46:01 UTC
* Number of recursive dependencies: 229

Run `revdepcheck::revdep_details(, "tidySEM")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tidySEM-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pseudo_class
    > ### Title: Estimate an Auxiliary Model using the Pseudo-Class Method
    > ### Aliases: pseudo_class pseudo_class.MxModel
    > 
    > ### ** Examples
    > 
    > set.seed(2)
    ...
    The degrees of freedom are assumed to be equal to the total number of observations used in the model ( 17 ) minus the number of parameters estimated ( 5 ). This may not be correct. If necessary, provide a better value via the 'df_complete' argument
    > 
    > pct_lm <- pseudo_class(x = fit,
    +              model = lm( SL ~ class, data = data),
    +              data = dat,
    +              m = 2)
    Error in pseudo_class_pool.default(fits, ...) : 
      Cannot pool fit objects, because package 'mice' is not installed
    Calls: pseudo_class ... pseudo_class.class_draws -> pseudo_class_pool -> pseudo_class_pool.default
    Execution halted
    ```

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      • On CRAN (1): 'test-plot_profiles_varyingcov.R:2:3'
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-pseudo_class_technique.R:8:3'): pseudo_class works ─────────────
      Error in `pseudo_class_pool.default(fits, ...)`: Cannot pool fit objects, because package 'mice' is not installed
      Backtrace:
          ▆
       1. ├─tidySEM::pseudo_class(...)
       2. └─tidySEM:::pseudo_class.class_draws(...)
       3.   ├─tidySEM:::pseudo_class_pool(fits, ...)
       4.   └─tidySEM:::pseudo_class_pool.default(fits, ...)
      
      [ FAIL 1 | WARN 1 | SKIP 6 | PASS 101 ]
      Error: Test failures
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

# vsmi

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/vsmi
* Date/Publication: 2024-05-25 17:20:02 UTC
* Number of recursive dependencies: 61

Run `revdepcheck::revdep_details(, "vsmi")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# weights

<details>

* Version: 1.0.4
* GitHub: NA
* Source code: https://github.com/cran/weights
* Date/Publication: 2021-06-10 21:50:02 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::revdep_details(, "weights")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘mice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

