cran-comments
================

## mice 3.16.0

## Reason

New submission in response to mail of CRAN team:

    Dear maintainer,

    Please see the problems shown on
    <https://cran.r-project.org/web/checks/check_results_mice.html>.

    Please correct before 2023-05-31 to safely retain your package on CRAN.

    Packages in Suggests should be used conditionally: see 'Writing R Extensions'.
    This needs to be corrected even if the missing package(s) become available.
    It can be tested by checking with _R_CHECK_DEPENDS_ONLY_=true.

    The CRAN Team

## Actions

- All issues noted in mail resolved
- Several changes and enhancements since `mice 3.15.0`

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
...
WARNING   Requires (indirectly) orphaned package: 'ucminf'
```

Solving this is beyond my powers.

### Rhub

``` r
devtools::check_rhub()
```

Everything OK except for the orphaned package: ‘ucminf’

### Local check

Package built by

``` r
library("devtools")
build()
```

``` bash
R CMD CHECK mice_3.16.0.tar.gz
```

Status: OK

## Downstream dependencies

### Overview

`mice` has 113 downstream dependencies

``` r
library(revdepcheck)
revdep_reset()
revdep_check(num_workers = 10)
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
    ## ✔ idem 5.1                               ── E: 0     | W: 0     | N: 2    
    ## ✔ ImputeRobust 1.3.1                     ── E: 0     | W: 0     | N: 0    
    ## ✔ insight 0.19.2                         ── E: 0     | W: 0     | N: 0    
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
    ## ✔ microeco 0.17.0                        ── E: 0     | W: 0     | N: 0    
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
    ## ✔ parameters 0.21.0                      ── E: 0     | W: 0     | N: 0    
    ## ✔ pema 0.1.3                             ── E: 0     | W: 0     | N: 2    
    ## I pguIMP 0.0.0.3                         ── E: 1     | W: 0     | N: 0    
    ## ✔ pre 1.0.6                              ── E: 0     | W: 0     | N: 0    
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

### Actions taken

- I alerted the maintainer of the `MatchThem` package that the
  documentation for `cbind.mids()` was removed from the `mice` package
  to conform to CRAN guidelines.

- The novel issue for `bipd` is equivalent to the old one. I contacted
  the maintainer previously. Did not repeat it now.

- In the past, I alerted/contacted the maintainers of the packages with
  existing reverse dependencies issues. I did not repeat for this
  release of `mice`.
