cran-comments
================

## Release summary

This is the resubmission of `mice` version 3.17.0. The previous
submission was rejected due a problem with the downstream `autoReg` and
`finalfit` packages. The problem was a change in the reporting of the
confidence intervals. In the previous version confidence intervals were
named `2.5 %` and `97.5 %`, a convention inherited from
`stats::confint.default`. The update `mice 3.17.0` adopts the more
flexible `broom` convention of naming these boundaries `conf.low` and
`conf.high`, so that these names work for any probability level. This
change caused the two downstream packages to fail. The issue has been
fixed in this version of `mice` by duplicating the two conf columns, and
naming them `2.5 %` and `97.5 %`. I will alert the maintainers of the
`autoReg` and `finalfit` packages to this change, but for now, having
the duplicate columns fixes the problem.

This release of `mice` adds new features, bug fixes, and documentation
improvements. See <https://github.com/amices/mice/blob/master/NEWS.md>

## Test environments

``` r
packageVersion("mice")
```

    ## [1] '3.17.0'

``` r
R.Version()$version.string
```

    ## [1] "R version 4.4.2 (2024-10-31)"

## Local checks

NOTE: Run in OSX terminal, not in Rstudio terminal.

``` bash
env _R_CHECK_DEPENDS_ONLY_=true R CMD check mice_3.17.0.tar.gz

Status: OK
```

``` r
devtools::check(env_vars = c('_R_CHECK_DEPENDS_ONLY_' = "true"))
...
Status: OK
```

## Remote checks

### win-builder

``` r
devtools::check_win_devel()

Status: OK
```

### Rhub

``` r
rhub::rhub_doctor()
rhub::rhub_check()
```

Status: OK \<\>

For details: <https://github.com/amices/mice/actions>

## Downstream dependencies

### Code

Note: Inactivate `credentials::set_github_pat()`

``` r
library(revdepcheck)
revdep_reset()
revdep_check(pkg = ".", num_workers = 12, quiet = FALSE)
```

``` r
revdepcheck::revdep_summary()
```

    ## ✔ accelmissing 1.4                       ── E: 0     | W: 0     | N: 1    
    ## ✔ adjustedCurves 0.11.2                  ── E: 0     | W: 0     | N: 0    
    ## ✔ alookr 0.3.9                           ── E: 0     | W: 0     | N: 0    
    ## ✔ autoReg 0.3.3                          ── E: 0     | W: 0     | N: 0    
    ## ✔ BaM 1.0.3                              ── E: 0     | W: 0     | N: 0    
    ## ✔ basecamb 1.1.5                         ── E: 0     | W: 0     | N: 0    
    ## ✔ betaMC 1.3.2                           ── E: 0     | W: 0     | N: 0    
    ## ✔ BGGM 2.1.3                             ── E: 0     | W: 0     | N: 1    
    ## ✖ bipd 0.3                               ── E: 1     | W: 0     | N: 0-1+1
    ## ✔ bootImpute 1.2.1                       ── E: 1     | W: 0     | N: 0    
    ## T brms 2.22.0                            ── E: 1     | W: 0     | N: 2    
    ## ✔ brokenstick 2.5.0                      ── E: 0     | W: 0     | N: 0    
    ## ✔ broom.helpers 1.17.0                   ── E: 0     | W: 0     | N: 0    
    ## ✔ cati 0.99.4                            ── E: 0     | W: 0     | N: 0    
    ## ✔ censcyt 1.14.0                         ── E: 0     | W: 0     | N: 3    
    ## ✔ CIMPLE 0.1.0                           ── E: 0     | W: 0     | N: 0    
    ## ✔ ClustAll 1.2.0                         ── E: 0     | W: 0     | N: 0    
    ## ✔ clusterMI 1.2.2                        ── E: 0     | W: 0     | N: 0    
    ## ✔ cmahalanobis 0.4.2                     ── E: 0     | W: 0     | N: 0    
    ## ✔ cobalt 4.5.5                           ── E: 0     | W: 0     | N: 0    
    ## ✔ dlookr 0.6.3                           ── E: 0     | W: 0     | N: 0    
    ## ✔ dynamite 1.5.5                         ── E: 0     | W: 0     | N: 2    
    ## I dynr 0.1.16.105                        ── E: 1     | W: 0     | N: 0    
    ## ✔ eatRep 0.14.7                          ── E: 0     | W: 0     | N: 0    
    ## ✔ finalfit 1.0.8                         ── E: 0     | W: 0     | N: 1    
    ## ✔ FLAME 2.1.1                            ── E: 0     | W: 0     | N: 0    
    ## ✔ flevr 0.0.4                            ── E: 0     | W: 0     | N: 0    
    ## ✔ gerbil 0.1.9                           ── E: 0     | W: 0     | N: 0    
    ## ✔ gFormulaMI 1.0.0                       ── E: 0     | W: 0     | N: 0    
    ## ✔ ggeffects 2.0.0                        ── E: 0     | W: 0     | N: 0    
    ## ✔ ggmice 0.1.0                           ── E: 0     | W: 0     | N: 0    
    ## ✔ gtsummary 2.0.3                        ── E: 0     | W: 0     | N: 0    
    ## ✔ HardyWeinberg 1.7.8                    ── E: 0     | W: 0     | N: 0    
    ## ✔ hhsmm 0.4.2                            ── E: 0     | W: 0     | N: 0    
    ## ✔ Hmisc 5.2.0                            ── E: 0     | W: 0     | N: 1    
    ## ✔ holodeck 0.2.2                         ── E: 0     | W: 0     | N: 0    
    ## ✔ hot.deck 1.2                           ── E: 0     | W: 0     | N: 1    
    ## ✔ howManyImputations 0.2.5               ── E: 0     | W: 0     | N: 0    
    ## ✔ HSAUR3 1.0.15                          ── E: 0     | W: 0     | N: 0    
    ## ✔ idem 5.2                               ── E: 0     | W: 0     | N: 2    
    ## ✔ ImputeRobust 1.3.1                     ── E: 0     | W: 0     | N: 0    
    ## ✔ insight 1.0.0                          ── E: 0     | W: 0     | N: 0    
    ## ✔ intmed 0.1.2                           ── E: 0     | W: 0     | N: 0    
    ## ✔ IPWboxplot 0.1.2                       ── E: 0     | W: 0     | N: 0    
    ## ✔ joinet 1.0.0                           ── E: 1     | W: 0     | N: 1    
    ## ✔ JWileymisc 1.4.1                       ── E: 0     | W: 0     | N: 0    
    ## ✔ konfound 1.0.2                         ── E: 0     | W: 0     | N: 0    
    ## ✔ LMMstar 1.1.0                          ── E: 0     | W: 0     | N: 1    
    ## ✔ logistf 1.26.0                         ── E: 0     | W: 0     | N: 0    
    ## ✔ LSAmitR 1.0.3                          ── E: 0     | W: 0     | N: 2    
    ## ✔ manymome 0.2.4                         ── E: 0     | W: 0     | N: 0    
    ## ✔ marginaleffects 0.24.0                 ── E: 0     | W: 0     | N: 0    
    ## ✔ MatchThem 1.2.1                        ── E: 0     | W: 0     | N: 0    
    ## ✔ medflex 0.6.10                         ── E: 0     | W: 0     | N: 0    
    ## ✔ metavcov 2.1.5                         ── E: 0     | W: 0     | N: 0    
    ## ✔ mi4p 1.2                               ── E: 0     | W: 0     | N: 0    
    ## ✔ micd 1.1.1                             ── E: 0     | W: 0     | N: 0    
    ## ✔ miceadds 3.17.44                       ── E: 0     | W: 0     | N: 2    
    ## ✔ miceafter 0.5.0                        ── E: 0     | W: 0     | N: 0    
    ## ✔ miceFast 0.8.2                         ── E: 0     | W: 0     | N: 2    
    ## ✔ micemd 1.10.0                          ── E: 0     | W: 0     | N: 0    
    ## ✔ microeco 1.10.0                        ── E: 0     | W: 0     | N: 0    
    ## ✔ midastouch 1.3                         ── E: 0     | W: 0     | N: 1    
    ## ✔ midfieldr 1.0.2                        ── E: 1     | W: 0     | N: 1    
    ## ✔ midoc 1.0.0                            ── E: 0     | W: 0     | N: 0    
    ## ✔ mifa 0.2.0                             ── E: 0     | W: 0     | N: 1    
    ## ✔ MIIPW 0.1.1                            ── E: 0     | W: 0     | N: 0    
    ## ✔ misaem 1.0.1                           ── E: 0     | W: 0     | N: 0    
    ## ✔ miselect 0.9.2                         ── E: 0     | W: 0     | N: 0    
    ## ✔ missCompare 1.0.3                      ── E: 0     | W: 0     | N: 0    
    ## ✔ missDiag 1.0.1                         ── E: 0     | W: 0     | N: 0    
    ## ✔ missMDA 1.19                           ── E: 0     | W: 0     | N: 0    
    ## ✔ misty 0.6.8                            ── E: 0     | W: 0     | N: 0    
    ## ✔ mitml 0.4.5                            ── E: 0     | W: 0     | N: 0    
    ## ✔ miWQS 0.4.4                            ── E: 0     | W: 0     | N: 0    
    ## ✔ mixgb 1.0.2                            ── E: 0     | W: 0     | N: 0    
    ## ✔ MixtureMissing 3.0.3                   ── E: 0     | W: 0     | N: 0    
    ## ✔ MKinfer 1.2                            ── E: 0     | W: 0     | N: 0    
    ## ✔ mlim 0.3.0                             ── E: 0     | W: 0     | N: 1    
    ## ✔ modelsummary 2.2.0                     ── E: 0     | W: 0     | N: 0    
    ## ✔ monoClust 1.2.1                        ── E: 0     | W: 0     | N: 1    
    ## ✔ MRPC 3.1.0                             ── E: 0     | W: 0     | N: 0    
    ## ✔ MSiP 1.3.7                             ── E: 0     | W: 0     | N: 1    
    ## ✔ mvnimpute 1.0.1                        ── E: 0     | W: 0     | N: 0    
    ## ✔ mvs 2.0.0                              ── E: 0     | W: 0     | N: 0    
    ## ✔ NIMAA 0.2.1                            ── E: 0     | W: 0     | N: 2    
    ## ✔ nncc 2.0.0                             ── E: 0     | W: 0     | N: 0    
    ## ✔ ordbetareg 0.7.2                       ── E: 0     | W: 0     | N: 2    
    ## ✔ OTrecod 0.1.2                          ── E: 0     | W: 0     | N: 1    
    ## ✔ parameters 0.24.0                      ── E: 0     | W: 0     | N: 0    
    ## ✔ pema 0.1.3                             ── E: 1     | W: 0     | N: 2    
    ## ✔ pre 1.0.7                              ── E: 0     | W: 0     | N: 0    
    ## ✔ psfmi 1.4.0                            ── E: 0     | W: 0     | N: 0    
    ## ✔ qgcomp 2.15.2                          ── E: 0     | W: 0     | N: 1    
    ## ✔ Qtools 1.5.9                           ── E: 0     | W: 0     | N: 0    
    ## ✔ rattle 5.5.1                           ── E: 0     | W: 0     | N: 3    
    ## ✔ RBtest 1.1                             ── E: 0     | W: 0     | N: 1    
    ## ✔ realTimeloads 1.0.0                    ── E: 1     | W: 0     | N: 0    
    ## ✔ RefBasedMI 0.2.0                       ── E: 0     | W: 0     | N: 0    
    ## ✔ regmedint 1.0.1                        ── E: 0     | W: 0     | N: 1    
    ## ✔ rexposome 1.28.0                       ── E: 0     | W: 5     | N: 4    
    ## ✔ RfEmpImp 2.1.8                         ── E: 0     | W: 0     | N: 0    
    ## ✔ rms 6.8.2                              ── E: 0     | W: 0     | N: 0    
    ## I rmsb 1.1.1                             ── E: 1     | W: 0     | N: 1    
    ## ✔ RNAseqCovarImpute 1.4.0                ── E: 0     | W: 0     | N: 2    
    ## ✔ rqlm 2.1.1                             ── E: 0     | W: 0     | N: 0    
    ## ✔ RSquaredMI 0.1.1                       ── E: 0     | W: 0     | N: 0    
    ## ✔ semmcci 1.1.4                          ── E: 0     | W: 0     | N: 0    
    ## ✔ semTools 0.5.6                         ── E: 0     | W: 0     | N: 1    
    ## ✔ seqimpute 2.1.0                        ── E: 0     | W: 0     | N: 0    
    ## ✔ shapeNA 0.0.2                          ── E: 0     | W: 0     | N: 3    
    ## ✔ sjmisc 2.8.10                          ── E: 0     | W: 0     | N: 0    
    ## ✔ smdi 0.3.1                             ── E: 1     | W: 0     | N: 0    
    ## ✔ sociome 2.2.5                          ── E: 0     | W: 0     | N: 0    
    ## ✔ StackImpute 0.1.0                      ── E: 0     | W: 0     | N: 2    
    ## ✔ superMICE 1.1.1                        ── E: 0     | W: 0     | N: 1    
    ## ✔ svyweight 0.1.0                        ── E: 0     | W: 0     | N: 1    
    ## ✔ SynDI 0.1.0                            ── E: 0     | W: 0     | N: 2    
    ## ✔ synergyfinder 3.14.0                   ── E: 0     | W: 1     | N: 3    
    ## ✔ TestDataImputation 2.3                 ── E: 0     | W: 0     | N: 0    
    ## ✔ tidySEM 0.2.7                          ── E: 0     | W: 0     | N: 0    
    ## ✔ vsmi 0.1.0                             ── E: 0     | W: 0     | N: 0    
    ## ✔ weights 1.0.4                          ── E: 0     | W: 0     | N: 0

### revdepcheck results

We checked 123 reverse dependencies (118 from CRAN + 5 from
Bioconductor), comparing R CMD check results across CRAN and dev
versions of this package.

- We saw 1 new problems
- We failed to check 3 packages

Issues with CRAN packages are summarised below.

### New problems

(This reports the first line of each new failure)

- bipd checking dependencies in R code …sh: line 1: 58727 Segmentation
  fault: 11 R_DEFAULT_PACKAGES=NULL
  ‘/Library/Frameworks/R.framework/Resources/bin/R’ –vanilla –no-echo
  2\>&1 \<
  ‘/var/folders/5\_/g85d42yj50b6lrjq4rzjzg8w0000gn/T//RtmpM38C1S/filee1cf761be8f7’

### Failed to check

- brms (NA)
- dynr (NA)
- rmsb (NA)

mice developer comment: These failures are unrelated to mice

See <https://github.com/amices/mice/tree/master/revdep> for additional
details.
