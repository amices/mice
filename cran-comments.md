cran-comments
================

## mice 3.16.0

## Overview

This submission

- is a third resubmission, using
  `devtools::check(env_vars = c('_R_CHECK_DEPENDS_ONLY_' = "true"))` to
  mimmick CRAN incoming checks
- is a second resubmission, with `\donttest` changed in `\dontrun` to
  evade `_R_CHECK_DEPENDS_ONLY_=true` errors;
- is a resubmission of `mice 3.16.0` after the orphaned `ucminf` CRAN
  package got a new maintainer;
- is tested with the `_R_CHECK_DEPENDS_ONLY_=true` flag, as requested;
- solves the problems at
  `https://cran.r-project.org/web/checks/check_results_mice.html`;
- contains new features and bug fixes, as described in the `NEWS.md`.

## Test environments

``` r
R.Version()
```

    ## $platform
    ## [1] "aarch64-apple-darwin20"
    ## 
    ## $arch
    ## [1] "aarch64"
    ## 
    ## $os
    ## [1] "darwin20"
    ## 
    ## $system
    ## [1] "aarch64, darwin20"
    ## 
    ## $status
    ## [1] ""
    ## 
    ## $major
    ## [1] "4"
    ## 
    ## $minor
    ## [1] "3.0"
    ## 
    ## $year
    ## [1] "2023"
    ## 
    ## $month
    ## [1] "04"
    ## 
    ## $day
    ## [1] "21"
    ## 
    ## $`svn rev`
    ## [1] "84292"
    ## 
    ## $language
    ## [1] "R"
    ## 
    ## $version.string
    ## [1] "R version 4.3.0 (2023-04-21)"
    ## 
    ## $nickname
    ## [1] "Already Tomorrow"

## Checks

### win-builder

``` r
devtools::check_win_devel()

Status: OK
```

### Rhub

``` r
devtools::check_rhub()
```

#### Windows

    ── mice 3.16.0: NOTE

      Build ID:   mice_3.16.0.tar.gz-018f40fa4cdc492d8c9f43b7e5f5e0e7
      Platform:   Windows Server 2022, R-devel, 64 bit
      Submitted:  11m 27s ago
      Build time: 11m 25s

    ❯ checking HTML version of manual ... [27s] NOTE
      Skipping checking math rendering: package 'V8' unavailable

    ❯ checking for non-standard things in the check directory ... NOTE
      Found the following files/directories:
        ''NULL''

    ❯ checking for detritus in the temp directory ... NOTE
      Found the following files/directories:
        'lastMiKTeXException'

    0 errors ✔ | 0 warnings ✔ | 3 notes ✖

I believe these NOTES are benign.

#### Fedora Linux

    Status: succes

#### Ubuntu 20.04

    Status: succes

#### Debian

    FAILURE 

    Running `R CMD build`...
    * checking for file ‘/tmp/Rtmp5Lhzhn/remotes13171d81356/mice/DESCRIPTION’ ... OK
    * preparing ‘mice’:
    * checking DESCRIPTION meta-information ... OK
    * cleaning src
    * checking for LF line-endings in source and make files and shell scripts
    * checking for empty or unneeded directories
    * building ‘mice_3.16.0.tar.gz’
    Installing package into ‘/home/docker/R’
    (as ‘lib’ is unspecified)
    Error : Bioconductor does not yet build and check packages for R version 4.4; see
      https://bioconductor.org/install
    ERROR: dependency ‘mitml’ is not available for package ‘mice’
    * removing ‘/home/docker/R/mice’
    > There were 20 warnings (use warnings() to see them)

    > 
    Error : Bioconductor does not yet build and check packages for R version 4.4; see
      https://bioconductor.org/install
    > library(mice)
    Error in library(mice) : there is no package called ‘mice’
    Execution halted
    Build step 'Execute shell' marked build as failure
    Pinging https://builder.r-hub.io/build/FAILURE/mice_3.16.0.tar.gz-82dc7cda08814b25ac48be4b5c9c940b/2023-06-01T11:37:52Z
    {"status":"ok"}
    Finished: FAILURE

I believe I cannot do something to make this problem disappear.

### Local checks

Package built by

``` r
library("devtools")
build()
```

``` bash
R CMD CHECK mice_3.16.0.tar.gz

Status: OK
```

### Local check using `_R_CHECK_DEPENDS_ONLY_=true` flag.

NOTE: Run in OSX terminal, not in Rstudio terminal.

``` bash
env _R_CHECK_DEPENDS_ONLY_=true R CMD check mice_3.16.0.tar.gz

Status: OK
```

``` r
devtools::check(env_vars = c('_R_CHECK_DEPENDS_ONLY_' = "true"))

...
   Status: OK
   
── R CMD check results ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── mice 3.16.0 ────
Duration: 1m 13s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔
```

## Downstream dependencies

### Overview

`mice` has 114 downstream dependencies

``` r
# NOTE: Temporarily remove credentials line from .Rprofile
library(revdepcheck)
revdep_reset()
revdep_check(pkg = ".", num_workers = 10, quiet = FALSE)
```

``` r
revdepcheck::revdep_summary()
```

    ## ✔ accelmissing 1.4                       ── E: 0     | W: 0     | N: 0    
    ## ✔ adjustedCurves 0.10.1                  ── E: 0     | W: 0     | N: 0    
    ## ✔ alookr 0.3.7                           ── E: 0     | W: 0     | N: 0    
    ## ✔ autoReg 0.3.2                          ── E: 0     | W: 0     | N: 0    
    ## ✔ BaM 1.0.3                              ── E: 0     | W: 0     | N: 0    
    ## ✔ basecamb 1.1.2                         ── E: 0     | W: 0     | N: 0    
    ## ✔ BGGM 2.0.4                             ── E: 0     | W: 0     | N: 2    
    ## ✔ binaryTimeSeries 1.0.2                 ── E: 0     | W: 0     | N: 0    
    ## ✔ biokNN 0.1.0                           ── E: 0     | W: 0     | N: 1    
    ## ✖ bipd 0.3                               ── E: 0     | W: 0     | N: 0-1+1
    ## ✔ bootImpute 1.2.0                       ── E: 0     | W: 0     | N: 0    
    ## ✔ brms 2.19.0                            ── E: 0     | W: 0     | N: 2    
    ## ✔ brokenstick 2.5.0                      ── E: 0     | W: 0     | N: 0    
    ## ✔ broom.helpers 1.13.0                   ── E: 0     | W: 0     | N: 0    
    ## ✔ bucky 1.0.7                            ── E: 0     | W: 0     | N: 2    
    ## ✔ CALIBERrfimpute 1.0.7                  ── E: 0     | W: 0     | N: 0    
    ## ✔ cati 0.99.4                            ── E: 0     | W: 0     | N: 0    
    ## ✔ censcyt 1.8.0                          ── E: 0     | W: 0     | N: 2    
    ## ✔ cobalt 4.5.1                           ── E: 0     | W: 0     | N: 0    
    ## ✔ dlookr 0.6.1                           ── E: 0     | W: 0     | N: 0    
    ## I dynr 0.1.16.91                         ── E: 1     | W: 0     | N: 0    
    ## ✔ eatRep 0.14.7                          ── E: 0     | W: 0     | N: 0    
    ## ✔ finalfit 1.0.6                         ── E: 0     | W: 0     | N: 2    
    ## ✔ FLAME 2.1.1                            ── E: 0     | W: 0     | N: 0    
    ## ✔ gerbil 0.1.9                           ── E: 0     | W: 0     | N: 0    
    ## ✔ gFormulaMI 1.0.0                       ── E: 0     | W: 0     | N: 0    
    ## ✔ ggeffects 1.2.2                        ── E: 0     | W: 0     | N: 0    
    ## ✔ ggmice 0.0.1                           ── E: 0     | W: 0     | N: 0    
    ## ✔ gtsummary 1.7.1                        ── E: 0     | W: 0     | N: 0    
    ## ✔ HardyWeinberg 1.7.5                    ── E: 0     | W: 0     | N: 1    
    ## ✔ hhsmm 0.3.5                            ── E: 0     | W: 0     | N: 0    
    ## ✔ Hmisc 5.1.0                            ── E: 0     | W: 0     | N: 1    
    ## ✔ holodeck 0.2.1                         ── E: 0     | W: 0     | N: 1    
    ## ✔ hot.deck 1.2                           ── E: 0     | W: 0     | N: 0    
    ## ✔ howManyImputations 0.2.4               ── E: 0     | W: 0     | N: 0    
    ## ✔ HSAUR3 1.0.14                          ── E: 0     | W: 0     | N: 0    
    ## I idem 5.1                               ── E: 1     | W: 0     | N: 0    
    ## ✔ ImputeRobust 1.3.1                     ── E: 0     | W: 0     | N: 0    
    ## ✔ insight 0.19.2                         ── E: 0     | W: 0     | N: 1    
    ## ✔ intmed 0.1.2                           ── E: 0     | W: 0     | N: 0    
    ## ✔ IPWboxplot 0.1.1                       ── E: 0     | W: 0     | N: 0    
    ## ✔ JWileymisc 1.4.0                       ── E: 0     | W: 0     | N: 0    
    ## ✔ konfound 0.4.0                         ── E: 0     | W: 0     | N: 1    
    ## ✔ lavaan.survey 1.1.3.1                  ── E: 1     | W: 0     | N: 0    
    ## ✔ LMMstar 0.9.0                          ── E: 0     | W: 0     | N: 1    
    ## ✔ logistf 1.25.0                         ── E: 0     | W: 0     | N: 0    
    ## ✔ LSAmitR 1.0.3                          ── E: 0     | W: 0     | N: 2    
    ## ✔ manydata 0.8.2                         ── E: 0     | W: 0     | N: 1    
    ## ✔ marginaleffects 0.12.0                 ── E: 0     | W: 0     | N: 1    
    ## ✖ MatchThem 1.0.1                        ── E: 0     | W: 0  +1 | N: 0    
    ## ✔ mdapack 0.0.2                          ── E: 0     | W: 0     | N: 2    
    ## ✔ medflex 0.6.7                          ── E: 1     | W: 1     | N: 0    
    ## ✔ metavcov 2.1.4                         ── E: 0     | W: 0     | N: 0    
    ## ✔ mi4p 1.1                               ── E: 0     | W: 0     | N: 0    
    ## ✔ micd 1.1.1                             ── E: 0     | W: 0     | N: 0    
    ## ✔ miceadds 3.16.18                       ── E: 0     | W: 0     | N: 3    
    ## ✔ miceafter 0.5.0                        ── E: 0     | W: 0     | N: 0    
    ## ✔ miceFast 0.8.2                         ── E: 0     | W: 0     | N: 1    
    ## ✔ micemd 1.8.0                           ── E: 0     | W: 0     | N: 1    
    ## ✔ microeco 0.19.0                        ── E: 0     | W: 0     | N: 0    
    ## ✔ midastouch 1.3                         ── E: 0     | W: 0     | N: 1    
    ## ✔ mifa 0.2.0                             ── E: 0     | W: 0     | N: 1    
    ## ✔ MIIPW 0.1.1                            ── E: 0     | W: 0     | N: 0    
    ## ✔ misaem 1.0.1                           ── E: 0     | W: 0     | N: 0    
    ## ✔ miselect 0.9.0                         ── E: 0     | W: 0     | N: 0    
    ## ✔ missCompare 1.0.3                      ── E: 0     | W: 0     | N: 0    
    ## ✔ missDiag 1.0.1                         ── E: 0     | W: 0     | N: 0    
    ## ✔ missMDA 1.18                           ── E: 0     | W: 0     | N: 0    
    ## ✔ mitml 0.4.5                            ── E: 0     | W: 0     | N: 0    
    ## ✔ miWQS 0.4.4                            ── E: 0     | W: 0     | N: 0    
    ## ✔ mixgb 1.0.2                            ── E: 0     | W: 0     | N: 0    
    ## ✔ MixtureMissing 2.0.0                   ── E: 0     | W: 0     | N: 0    
    ## ✔ MKinfer 1.1                            ── E: 0     | W: 0     | N: 0    
    ## ✔ mlim 0.3.0                             ── E: 0     | W: 0     | N: 0    
    ## ✔ modelsummary 1.4.1                     ── E: 0     | W: 0     | N: 0    
    ## ✔ monoClust 1.2.1                        ── E: 0     | W: 0     | N: 0    
    ## ✔ MRPC 3.1.0                             ── E: 0     | W: 0     | N: 0    
    ## ✔ MSiP 1.3.7                             ── E: 0     | W: 0     | N: 1    
    ## ✔ mvnimpute 1.0.1                        ── E: 0     | W: 0     | N: 0    
    ## ✔ NADIA 0.4.2                            ── E: 0     | W: 0     | N: 1    
    ## ✔ NIMAA 0.2.1                            ── E: 0     | W: 0     | N: 2    
    ## ✔ nncc 1.0.1                             ── E: 0     | W: 0     | N: 0    
    ## ✔ ordbetareg 0.7.1                       ── E: 0     | W: 0     | N: 2    
    ## ✔ OTrecod 0.1.2                          ── E: 0     | W: 0     | N: 0    
    ## ✔ parameters 0.21.1                      ── E: 0     | W: 0     | N: 1    
    ## ✔ pema 0.1.3                             ── E: 0     | W: 0     | N: 2    
    ## I pguIMP 0.0.0.3                         ── E: 1     | W: 0     | N: 0    
    ## ✖ pre 1.0.6                              ── E: 0  +1 | W: 0     | N: 0    
    ## ✔ psfmi 1.1.0                            ── E: 0     | W: 0     | N: 0    
    ## ✔ qgcomp 2.10.1                          ── E: 0     | W: 0     | N: 0    
    ## ✔ Qtools 1.5.6                           ── E: 0     | W: 0     | N: 0    
    ## ✔ rattle 5.5.1                           ── E: 0     | W: 0     | N: 3    
    ## ✔ RBtest 1.1                             ── E: 0     | W: 0     | N: 1    
    ## ✔ RefBasedMI 0.1.0                       ── E: 0     | W: 0     | N: 0    
    ## ✔ regmedint 1.0.0                        ── E: 0     | W: 0     | N: 1    
    ## ✔ RegularizedSCA 0.5.4                   ── E: 0     | W: 0     | N: 0    
    ## I Replication 0.1.2                      ── E: 1     | W: 0     | N: 0    
    ## ✔ rexposome 1.22.0                       ── E: 1     | W: 5     | N: 2    
    ## ✔ RfEmpImp 2.1.8                         ── E: 0     | W: 0     | N: 0    
    ## ✔ rms 6.7.0                              ── E: 0     | W: 0     | N: 0    
    ## ✔ rmsb 0.1.0                             ── E: 0     | W: 0     | N: 3    
    ## ✔ semTools 0.5.6                         ── E: 0     | W: 0     | N: 0    
    ## ✔ seqimpute 1.8                          ── E: 0     | W: 0     | N: 0    
    ## ✔ shapeNA 0.0.2                          ── E: 0     | W: 0     | N: 2    
    ## ✔ sjmisc 2.8.9                           ── E: 0     | W: 0     | N: 0    
    ## ✔ SLOPE 0.5.0                            ── E: 0     | W: 0     | N: 2    
    ## ✔ sociome 2.2.1                          ── E: 0     | W: 0     | N: 0    
    ## ✔ StackImpute 0.1.0                      ── E: 0     | W: 0     | N: 1    
    ## ✔ superMICE 1.1.1                        ── E: 0     | W: 0     | N: 1    
    ## ✔ svyweight 0.1.0                        ── E: 0     | W: 0     | N: 0    
    ## ✔ SynDI 0.1.0                            ── E: 0     | W: 0     | N: 1    
    ## ✔ synergyfinder 3.8.2                    ── E: 0     | W: 1     | N: 3    
    ## ✔ TestDataImputation 2.3                 ── E: 0     | W: 0     | N: 0    
    ## ✔ weights 1.0.4                          ── E: 0     | W: 0     | N: 0

### New issues

#### `MatchThem`

There is one new warning, for the `MatchThem` package:

``` r
revdepcheck::revdep_details(revdep = "MatchThem")
```

    ## ══ Reverse dependency check ═════════════════════════════════ MatchThem 1.0.1 ══
    ## 
    ## Status: BROKEN
    ## 
    ## ── Newly failing
    ## 
    ## ✖ checking Rd cross-references ... WARNING
    ## 
    ## ── Before ──────────────────────────────────────────────────────────────────────
    ## 0 errors ✔ | 0 warnings ✔ | 0 notes ✔
    ## 
    ## ── After ───────────────────────────────────────────────────────────────────────
    ## ❯ checking Rd cross-references ... WARNING
    ##   Missing link or links in documentation object 'cbind.Rd':
    ##     ‘[mice:cbind.mids]{mice::cbind.mids}’
    ##   
    ##   See section 'Cross-references' in the 'Writing R Extensions' manual.
    ## 
    ## 0 errors ✔ | 1 warning ✖ | 0 notes ✔

- I alerted the maintainer of the `MatchThem` package that the
  documentation for `cbind.mids()` was removed from the `mice` package
  to conform to CRAN guidelines. The maintainer responded this will be
  fixed in the next release.

#### `bipd`

``` r
revdepcheck::revdep_details(revdep = "bipd")
```

    ## ══ Reverse dependency check ════════════════════════════════════════ bipd 0.3 ══
    ## 
    ## Status: BROKEN
    ## 
    ## ── Fixed
    ## 
    ## ✔ checking dependencies in R code ...sh: line 1: 64022 Segmentation fault: 11  R_DEFAULT_PACKAGES=NULL '/Library/Frameworks/R.framework/Resources/bin/R' --vanilla --no-echo 2>&1 < '/var/folders/5_/g85d42yj50b6lrjq4rzjzg8w0000gn/T//RtmpPAGHHl/filef76c784da58a'
    ## 
    ## ── Newly failing
    ## 
    ## ✖ checking dependencies in R code ...sh: line 1: 64105 Segmentation fault: 11  R_DEFAULT_PACKAGES=NULL '/Library/Frameworks/R.framework/Resources/bin/R' --vanilla --no-echo 2>&1 < '/var/folders/5_/g85d42yj50b6lrjq4rzjzg8w0000gn/T//RtmpcESnnw/filef7d530e954ed'
    ## 
    ## ── Before ──────────────────────────────────────────────────────────────────────
    ## ❯ checking dependencies in R code ...sh: line 1: 64022 Segmentation fault: 11  R_DEFAULT_PACKAGES=NULL '/Library/Frameworks/R.framework/Resources/bin/R' --vanilla --no-echo 2>&1 < '/var/folders/5_/g85d42yj50b6lrjq4rzjzg8w0000gn/T//RtmpPAGHHl/filef76c784da58a'
    ##    NOTE
    ##   
    ##    *** caught segfault ***
    ##   address 0x6c65432f6c61636f, cause 'invalid permissions'
    ##   
    ##   Traceback:
    ##    1: dyn.load(file, DLLpath = DLLpath, ...)
    ##    2: library.dynam(lib, package, package.lib)
    ##    3: loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
    ##    4: asNamespace(ns)
    ##    5: namespaceImportFrom(ns, loadNamespace(j <- i[[1L]], c(lib.loc,     .libPaths()), versionCheck = vI[[j]]), i[[2L]], from = package)
    ##    6: loadNamespace(p)
    ##    7: withCallingHandlers(expr, message = function(c) if (inherits(c,     classes)) tryInvokeRestart("muffleMessage"))
    ##    8: suppressMessages(loadNamespace(p))
    ##    9: withCallingHandlers(expr, warning = function(w) if (inherits(w,     classes)) tryInvokeRestart("muffleWarning"))
    ##   10: suppressWarnings(suppressMessages(loadNamespace(p)))
    ##   11: doTryCatch(return(expr), name, parentenv, handler)
    ##   12: tryCatchOne(expr, names, parentenv, handlers[[1L]])
    ##   13: tryCatchList(expr, classes, parentenv, handlers)
    ##   14: tryCatch(suppressWarnings(suppressMessages(loadNamespace(p))),     error = function(e) e)
    ##   15: tools:::.check_packages_used(package = "bipd")
    ##   An irrecoverable exception occurred. R is aborting now ...
    ## 
    ## 0 errors ✔ | 0 warnings ✔ | 1 note ✖
    ## 
    ## ── After ───────────────────────────────────────────────────────────────────────
    ## ❯ checking dependencies in R code ...sh: line 1: 64105 Segmentation fault: 11  R_DEFAULT_PACKAGES=NULL '/Library/Frameworks/R.framework/Resources/bin/R' --vanilla --no-echo 2>&1 < '/var/folders/5_/g85d42yj50b6lrjq4rzjzg8w0000gn/T//RtmpcESnnw/filef7d530e954ed'
    ##    NOTE
    ##   
    ##    *** caught segfault ***
    ##   address 0x6c65432f6c61636f, cause 'invalid permissions'
    ##   
    ##   Traceback:
    ##    1: dyn.load(file, DLLpath = DLLpath, ...)
    ##    2: library.dynam(lib, package, package.lib)
    ##    3: loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
    ##    4: asNamespace(ns)
    ##    5: namespaceImportFrom(ns, loadNamespace(j <- i[[1L]], c(lib.loc,     .libPaths()), versionCheck = vI[[j]]), i[[2L]], from = package)
    ##    6: loadNamespace(p)
    ##    7: withCallingHandlers(expr, message = function(c) if (inherits(c,     classes)) tryInvokeRestart("muffleMessage"))
    ##    8: suppressMessages(loadNamespace(p))
    ##    9: withCallingHandlers(expr, warning = function(w) if (inherits(w,     classes)) tryInvokeRestart("muffleWarning"))
    ##   10: suppressWarnings(suppressMessages(loadNamespace(p)))
    ##   11: doTryCatch(return(expr), name, parentenv, handler)
    ##   12: tryCatchOne(expr, names, parentenv, handlers[[1L]])
    ##   13: tryCatchList(expr, classes, parentenv, handlers)
    ##   14: tryCatch(suppressWarnings(suppressMessages(loadNamespace(p))),     error = function(e) e)
    ##   15: tools:::.check_packages_used(package = "bipd")
    ##   An irrecoverable exception occurred. R is aborting now ...
    ## 
    ## 0 errors ✔ | 0 warnings ✔ | 1 note ✖

- The novel NOTE for `bipd` is equivalent to the old one. I contacted
  the maintainer previously. Did not repeat it now.

#### `pre`

- There is a new error generated by `test_pre_misc.R` of the `pre`
  package (`mice` is on `pre` Suggests):

``` r
revdepcheck::revdep_details(revdep = "pre")
```

    ## ══ Reverse dependency check ═══════════════════════════════════════ pre 1.0.6 ══
    ## 
    ## Status: BROKEN
    ## 
    ## ── Newly failing
    ## 
    ## ✖ checking tests ...
    ## 
    ## ── Before ──────────────────────────────────────────────────────────────────────
    ## 0 errors ✔ | 0 warnings ✔ | 0 notes ✔
    ## 
    ## ── After ───────────────────────────────────────────────────────────────────────
    ## ❯ checking tests ...
    ##   See below...
    ## 
    ## ── Test failures ───────────────────────────────────────────────── testthat ────
    ## 
    ## > library(testthat)
    ## > library(pre)
    ## > 
    ## > #####
    ## > # partykit and earth is loaded as failures of tests may be caused by the version
    ## > # of either package. Thus, we print the sessionInfo
    ## > 
    ## > library(earth)
    ## Loading required package: Formula
    ## Loading required package: plotmo
    ## Loading required package: plotrix
    ## Loading required package: TeachingDemos
    ## > library(partykit)
    ## Loading required package: grid
    ## Loading required package: libcoin
    ## Loading required package: mvtnorm
    ## > print(sessionInfo())
    ## R version 4.3.0 (2023-04-21)
    ## Platform: aarch64-apple-darwin20 (64-bit)
    ## Running under: macOS Ventura 13.4
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib/libRblas.0.dylib 
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0
    ## 
    ## locale:
    ## [1] C/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## time zone: Europe/Amsterdam
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] grid      stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ##  [1] partykit_1.2-20    mvtnorm_1.1-3      libcoin_1.0-9      earth_5.3.2       
    ##  [5] plotmo_3.6.2       TeachingDemos_2.12 plotrix_3.8-2      Formula_1.2-5     
    ##  [9] pre_1.0.6          testthat_3.1.8    
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] cli_3.6.1          rlang_1.1.1        stringi_1.7.12     MatrixModels_0.5-1
    ##  [5] glue_1.6.2         glmnet_4.1-7       brio_1.1.3         lifecycle_1.0.3   
    ##  [9] foreach_1.5.2      stringr_1.5.0      compiler_4.3.0     codetools_0.2-19  
    ## [13] Rcpp_1.0.10        lattice_0.21-8     R6_2.5.1           splines_4.3.0     
    ## [17] inum_1.0-5         shape_1.4.6        magrittr_2.0.3     rpart_4.1.19      
    ## [21] Matrix_1.5-4.1     tools_4.3.0        iterators_1.0.14   survival_3.5-5    
    ## > 
    ## > test_check("pre")
    ## [ FAIL 1 | WARN 0 | SKIP 0 | PASS 122 ]
    ## 
    ## ══ Failed tests ════════════════════════════════════════════════════════════════
    ## ── Error ('test_pre_misc.R:94:3'): cvpre gives previous results with airquality data ──
    ## Error in `serverSocket(port = port)`: creation of server socket failed: port 11471 cannot be opened
    ## Backtrace:
    ##     ▆
    ##  1. └─parallel::makeCluster(2L) at test_pre_misc.R:94:2
    ##  2.   └─parallel::makePSOCKcluster(names = spec, ...)
    ##  3.     └─base::serverSocket(port = port)
    ## 
    ## [ FAIL 1 | WARN 0 | SKIP 0 | PASS 122 ]
    ## Error: Test failures
    ## Execution halted
    ## 
    ## 1 error ✖ | 0 warnings ✔ | 0 notes ✔

- I tried reproducing the error but stopped my attempt due to the
  complex structure of the testing script. I contacted the `pre`
  maintainer by means of issue
  <https://github.com/marjoleinF/pre/issues/30>.
