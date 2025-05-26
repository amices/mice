cran-comments
================

## Release summary

`mice 3.18.0` adds new features, bug fixes, and documentation
improvements. See <https://github.com/amices/mice/blob/master/NEWS.md>

## Test environments

``` r
packageVersion("mice")
```

    ## [1] '3.17.7'

``` r
R.Version()$version.string
```

    ## [1] "R version 4.5.0 (2025-04-11)"

## Local checks

NOTE: Run in OSX terminal, not in Rstudio terminal.

``` bash
env _R_CHECK_DEPENDS_ONLY_=true 
R CMD check mice_3.17.7.tar.gz

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

Upload error: response reading failed (errno: 35)
```

### Platform checks

We checked the following platforms:

``` r
macos-latest (release)
windows-latest (release)
ubuntu-latest (devel)
ubuntu-latest (release)
ubuntu-latest (oldrel)
```

Status: OK

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
    ## I BGGM 2.1.5                             ── E: 1     | W: 0     | N: 0    
    ## ✖ bipd 0.3                               ── E: 1     | W: 0     | N: 0-1+1
    ## ✔ bootImpute 1.2.2                       ── E: 1     | W: 0     | N: 0    
    ## T brms 2.22.0                            ── E: 1     | W: 1     | N: 0    
    ## ✔ brokenstick 2.6.0                      ── E: 0     | W: 0     | N: 0    
    ## ✔ broom.helpers 1.21.0                   ── E: 0     | W: 0     | N: 0    
    ## ✔ cati 0.99.4                            ── E: 0     | W: 0     | N: 0    
    ## ✔ censcyt 1.16.0                         ── E: 0     | W: 0     | N: 3    
    ## ✔ CIMPLE 0.1.0                           ── E: 0     | W: 0     | N: 1    
    ## ✔ ClustAll 1.4.0                         ── E: 0     | W: 0     | N: 0    
    ## I clusterMI 1.5                          ── E: 1     | W: 0     | N: 0    
    ## ✔ cmahalanobis 0.5.0                     ── E: 0     | W: 0     | N: 0    
    ## ✔ cobalt 4.6.0                           ── E: 0     | W: 0     | N: 0    
    ## ✔ dlookr 0.6.3                           ── E: 0     | W: 0     | N: 0    
    ## ✔ dynamite 1.5.6                         ── E: 0     | W: 1     | N: 0    
    ## I dynr 0.1.16.105                        ── E: 1     | W: 0     | N: 0    
    ## ✔ eatRep 0.15.2                          ── E: 0     | W: 0     | N: 0    
    ## ✔ fastml 0.5.0                           ── E: 0     | W: 0     | N: 0    
    ## ✔ finalfit 1.0.8                         ── E: 0     | W: 0     | N: 0    
    ## ✔ FLAME 2.1.1                            ── E: 0     | W: 0     | N: 0    
    ## ✔ flevr 0.0.4                            ── E: 0     | W: 0     | N: 0    
    ## ✔ FuzzyImputationTest 0.5.1              ── E: 0     | W: 0     | N: 0    
    ## ✔ gerbil 0.1.9                           ── E: 0     | W: 0     | N: 0    
    ## ✔ gFormulaMI 1.0.1                       ── E: 0     | W: 0     | N: 0    
    ## ✔ ggeffects 2.2.1                        ── E: 0     | W: 0     | N: 0    
    ## ✔ ggmice 0.1.0                           ── E: 0     | W: 0     | N: 0    
    ## ✔ gtsummary 2.2.0                        ── E: 0     | W: 0     | N: 0    
    ## ✔ HardyWeinberg 1.7.8                    ── E: 0     | W: 0     | N: 0    
    ## ✔ hhsmm 0.4.2                            ── E: 0     | W: 0     | N: 0    
    ## I Hmisc 5.2.3                            ── E: 1     | W: 0     | N: 0    
    ## ✔ holodeck 0.2.2                         ── E: 0     | W: 0     | N: 0    
    ## ✔ hot.deck 1.2                           ── E: 0     | W: 0     | N: 1    
    ## ✔ howManyImputations 0.2.5               ── E: 0     | W: 0     | N: 0    
    ## ✔ HSAUR3 1.0.15                          ── E: 0     | W: 0     | N: 0    
    ## ✔ idem 5.2                               ── E: 0     | W: 0     | N: 1    
    ## ✔ ImputeRobust 1.3.1                     ── E: 0     | W: 0     | N: 0    
    ## ✔ insight 1.3.0                          ── E: 0     | W: 0     | N: 0    
    ## ✔ intmed 0.1.2                           ── E: 0     | W: 0     | N: 0    
    ## ✔ IPWboxplot 0.1.2                       ── E: 0     | W: 0     | N: 0    
    ## ✔ joinet 1.0.0                           ── E: 1     | W: 0     | N: 0    
    ## ✔ JWileymisc 1.4.3                       ── E: 0     | W: 0     | N: 0    
    ## ✔ konfound 1.0.2                         ── E: 0     | W: 0     | N: 0    
    ## ✔ lavaan.mi 0.1.0                        ── E: 0     | W: 0     | N: 0    
    ## ✔ LMMstar 1.1.0                          ── E: 0     | W: 0     | N: 0    
    ## I logistf 1.26.1                         ── E: 1     | W: 0     | N: 0    
    ## ✔ LSAmitR 1.0.3                          ── E: 0     | W: 0     | N: 0    
    ## ✔ manymome 0.2.8                         ── E: 0     | W: 0     | N: 0    
    ## ✔ marginaleffects 0.26.0                 ── E: 0     | W: 0     | N: 0    
    ## ✔ MatchThem 1.2.1                        ── E: 0     | W: 0     | N: 0    
    ## ✔ medflex 0.6.10                         ── E: 0     | W: 0     | N: 0    
    ## ✔ metafor 4.8.0                          ── E: 0     | W: 0     | N: 0    
    ## ✔ metavcov 2.1.5                         ── E: 0     | W: 0     | N: 0    
    ## ✔ mi4p 1.2                               ── E: 0     | W: 0     | N: 0    
    ## ✔ micd 1.1.1                             ── E: 0     | W: 0     | N: 0    
    ## I miceadds 3.17.44                       ── E: 1     | W: 0     | N: 0    
    ## ✔ miceafter 0.5.0                        ── E: 0     | W: 0     | N: 0    
    ## I miceFast 0.8.5                         ── E: 1     | W: 0     | N: 0    
    ## ✔ micemd 1.10.0                          ── E: 0     | W: 0     | N: 0    
    ## ✔ microeco 1.15.0                        ── E: 0     | W: 0     | N: 0    
    ## ✔ midastouch 1.3                         ── E: 0     | W: 0     | N: 0    
    ## ✔ midfieldr 1.0.2                        ── E: 1     | W: 0     | N: 0    
    ## ✔ midoc 1.0.0                            ── E: 0     | W: 0     | N: 0    
    ## ✔ mifa 0.2.1                             ── E: 0     | W: 0     | N: 0    
    ## ✔ MIGEE 0.1.0                            ── E: 0     | W: 0     | N: 1    
    ## ✔ MIIPW 0.1.2                            ── E: 0     | W: 0     | N: 0    
    ## ✔ misaem 1.0.1                           ── E: 0     | W: 0     | N: 0    
    ## ✔ miselect 0.9.2                         ── E: 0     | W: 0     | N: 0    
    ## ✔ missCompare 1.0.3                      ── E: 0     | W: 0     | N: 0    
    ## ✔ missDiag 1.0.1                         ── E: 0     | W: 0     | N: 0    
    ## ✔ missMDA 1.19                           ── E: 0     | W: 0     | N: 0    
    ## ✔ misty 0.7.2                            ── E: 0     | W: 0     | N: 0    
    ## ✔ mitml 0.4.5                            ── E: 0     | W: 0     | N: 0    
    ## ✔ miWQS 0.4.4                            ── E: 1     | W: 0     | N: 1    
    ## I mixgb 1.5.3                            ── E: 1     | W: 0     | N: 0    
    ## ✔ MixtureMissing 3.0.4                   ── E: 0     | W: 0     | N: 0    
    ## ✔ MKinfer 1.2                            ── E: 0     | W: 0     | N: 0    
    ## ✔ mlim 0.3.0                             ── E: 0     | W: 0     | N: 1    
    ## ✔ modelbased 0.11.0                      ── E: 0     | W: 0     | N: 0    
    ## ✔ modelsummary 2.3.0                     ── E: 0     | W: 0     | N: 0    
    ## ✔ monoClust 1.2.1                        ── E: 0     | W: 0     | N: 1    
    ## ✔ MRPC 3.1.0                             ── E: 0     | W: 0     | N: 0    
    ## ✔ MSiP 1.3.7                             ── E: 0     | W: 0     | N: 1    
    ## ✔ multilevelPSA 1.3.0                    ── E: 0     | W: 0     | N: 0    
    ## ✔ mvnimpute 1.0.1                        ── E: 0     | W: 0     | N: 0    
    ## ✔ mvs 2.1.0                              ── E: 0     | W: 0     | N: 0    
    ## ✔ NIMAA 0.2.1                            ── E: 0     | W: 0     | N: 0    
    ## ✔ nncc 2.0.0                             ── E: 0     | W: 0     | N: 0    
    ## ✔ ordbetareg 0.8                         ── E: 0     | W: 0     | N: 0    
    ## ✔ OTrecod 0.1.2                          ── E: 0     | W: 0     | N: 1    
    ## ✔ parameters 0.26.0                      ── E: 0     | W: 0     | N: 0    
    ## ✔ pema 0.1.4                             ── E: 1     | W: 0     | N: 0    
    ## ✔ pminternal 0.1.0                       ── E: 0     | W: 0     | N: 0    
    ## ✔ pre 1.0.7                              ── E: 0     | W: 0     | N: 1    
    ## ✔ psfmi 1.4.0                            ── E: 0     | W: 0     | N: 0    
    ## ✔ qgcomp 2.18.4                          ── E: 0     | W: 0     | N: 0    
    ## ✔ Qtools 1.5.9                           ── E: 0     | W: 0     | N: 0    
    ## ✔ rattle 5.5.1                           ── E: 0     | W: 0     | N: 1    
    ## ✔ RBtest 1.1                             ── E: 0     | W: 0     | N: 0    
    ## ✔ realTimeloads 1.0.0                    ── E: 1     | W: 0     | N: 0    
    ## I RefBasedMI 0.2.0                       ── E: 1     | W: 0     | N: 0    
    ## ✔ regmedint 1.0.1                        ── E: 0     | W: 0     | N: 1    
    ## ✔ rexposome 1.30.0                       ── E: 0     | W: 5     | N: 3    
    ## ✔ RfEmpImp 2.1.8                         ── E: 0     | W: 0     | N: 0    
    ## I rms 8.0.0                              ── E: 1     | W: 0     | N: 0    
    ## I rmsb 1.1.2                             ── E: 1     | W: 0     | N: 0    
    ## ✔ RNAseqCovarImpute 1.6.0                ── E: 0     | W: 0     | N: 2    
    ## ✔ rqlm 2.3.1                             ── E: 0     | W: 0     | N: 0    
    ## ✔ RSquaredMI 0.2.0                       ── E: 0     | W: 0     | N: 0    
    ## ✔ RulesTools 0.1.1                       ── E: 0     | W: 0     | N: 0    
    ## ✔ SBMTrees 1.2                           ── E: 0     | W: 0     | N: 0    
    ## ✔ scHiCcompare 1.0.0                     ── E: 0     | W: 0     | N: 0    
    ## ✔ semmcci 1.1.4                          ── E: 0     | W: 0     | N: 0    
    ## ✔ semTools 0.5.7                         ── E: 0     | W: 0     | N: 0    
    ## ✔ seqimpute 2.2.0                        ── E: 0     | W: 0     | N: 0    
    ## ✔ shapeNA 0.0.2                          ── E: 0     | W: 0     | N: 2    
    ## ✔ sjmisc 2.8.10                          ── E: 0     | W: 0     | N: 0    
    ## ✔ smdi 0.3.1                             ── E: 1     | W: 0     | N: 0    
    ## ✔ sociome 2.2.5                          ── E: 0     | W: 0     | N: 0    
    ## ✔ SSVS 2.1.0                             ── E: 0     | W: 0     | N: 0    
    ## ✔ StackImpute 0.1.0                      ── E: 0     | W: 0     | N: 2    
    ## ✔ superMICE 1.1.1                        ── E: 0     | W: 0     | N: 0    
    ## ✔ svyweight 0.1.0                        ── E: 0     | W: 0     | N: 1    
    ## ✔ SynDI 0.1.0                            ── E: 0     | W: 0     | N: 2    
    ## ✔ synergyfinder 3.16.0                   ── E: 0     | W: 1     | N: 2    
    ## ✔ TestDataImputation 2.3                 ── E: 0     | W: 0     | N: 0    
    ## ✔ tidySEM 0.2.8                          ── E: 0     | W: 0     | N: 0    
    ## ✔ vsmi 0.1.0                             ── E: 0     | W: 0     | N: 0    
    ## ✔ weights 1.0.4                          ── E: 0     | W: 0     | N: 0

### revdepcheck results

We checked 135 reverse dependencies, comparing R CMD check results
across CRAN and dev versions of this package.

- We saw 1 new problems
- We failed to check 12 packages

All problems were due to installation issues. As far as I call tell,
none of these problem is related to `mice`

See <https://github.com/amices/mice/tree/master/revdep> for details.
