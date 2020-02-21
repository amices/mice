cran-comments
================

## This is resubmission 2

This is resubmitted after the resubmission 1 was cancelled due to a
miscommunication.

There are two remaining problems (problems 2 (`hot.deck`) and 3
(`miceadds`) below). I have contacted the package maintainers two weeks
ago, and both promised me to update. There is indeed a new version of
`miceadds` on CRAN, which supposedly solves the problem. I believe that
there should also be an update of `hot.deck`, though I haven’t yet seen
it on CRAN so that one is probably also in submission (I cant’t see
that).

Hope this helps to get thing going again.

## This is resubmission 1

### Problem 1

    Changes to worse in reverse depends:
    
    Package: HardyWeinberg
    Check: re-building of vignette outputs
    New result: WARNING
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘HardyWeinberg.Rnw’ using Sweave
     Loading required package: mice
    
     Attaching package: 'mice'
    
     The following objects are masked from 'package:base':
    
         cbind, rbind
    
     Loading required package: Rsolnp
     Installing package into '/home/hornik/tmp/scratch/Rtmp2Owdzw/RLIBS_49de7c0e46e9'
     (as 'lib' is unspecified)
    
     Error: processing vignette 'HardyWeinberg.Rnw' failed with diagnostics:
      chunk 14 
     Error in contrib.url(repos, type) : 
       trying to use CRAN without setting a mirror
    
     --- failed re-building 'HardyWeinberg.Rnw'
    
     SUMMARY: processing the following file failed:
       'HardyWeinberg.Rnw'
    
     Error: Vignette re-building failed.
     Execution halted
     
    Package: MatchIt.mice
    Check: examples
    New result: ERROR
     Running examples in ‘MatchIt.mice-Ex.R’ failed
     The error most likely occurred in:
    
    base::assign(".ptime", proc.time(), pos = "CheckExEnv")
    ### Name: matchitmice
    ### Title: Matches Multiply Imputed Datasets
    ### Aliases: matchitmice
    ### Keywords: function
    
    ### ** Examples
    
    #Loading the 'dt.osa' dataset
    data(dt.osa)
    
    #Imputing missing data points in the'dt.osa' dataset
    datasets <- mice(dt.osa, m = 5, maxit = 1,
     +                  method = c("", "", "mean", "", "polyreg", "logreg", "logreg"))
    
      iter imp variable
       1   1  BMI  RAC
     Installing 'nnet' package...
     Installing package into ‘/home/hornik/tmp/scratch/Rtmpfgdy3y/RLIBS_4a054125c9bb’
     (as ‘lib’ is unspecified)
     Error in contrib.url(repos, type) : 
       trying to use CRAN without setting a mirror
     Calls: mice ... install.on.demand -> install.packages -> startsWith -> contrib.url
     Execution halted
     
    Package: mipred
    Check: tests
    New result: ERROR
       Running ‘testthat.R’ [3s/3s]
     Running the tests in ‘tests/testthat.R’ failed.
     Complete output:
    library(testthat)
    library(mipred)
    
    test_check("mipred")
       NULL
    
       Installing 'nnet' package...
       ── 1. Error: argument m in mice.options is ignored (@test-inputchecking-mipred.R
       trying to use CRAN without setting a mirror
       Backtrace:
         1. testthat::expect_warning(...)
         6. mipred::mipred(...)
         7. mipred:::.glm.mipred.cmb1(...)
         8. mipred:::.impute(...)
         9. mice::mice(...)
        10. mice:::sampler(...)
        11. mice:::sampler.univ(...)
        13. mice::mice.impute.polyreg(...)
        14. mice:::install.on.demand("nnet", ...)
        15. utils::install.packages(pkg, quiet = quiet)
        17. utils::contrib.url(repos, type)
    
       NULL
    
       Installing 'nnet' package...
       ── 2. Error: argument m in mice.options is ignored (@test-inputchecking-mipred.c
       trying to use CRAN without setting a mirror
       Backtrace:
         1. testthat::expect_warning(...)
         6. mipred::mipred.cv(...)
         7. mipred:::.glm.mipred.cmb1.cv(...)
         8. mipred:::.impute(...)
         9. mice::mice(...)
        10. mice:::sampler(...)
        11. mice:::sampler.univ(...)
        13. mice::mice.impute.polyreg(...)
        14. mice:::install.on.demand("nnet", ...)
        15. utils::install.packages(pkg, quiet = quiet)
        17. utils::contrib.url(repos, type)
    
       ══ testthat results  ════════════════ 

I fixed the `trying to use CRAN without setting a mirror` error in
`mice:::install.on.demand()` by including the `repos` argument.

### Problem 2

    Package: hot.deck
    Check: re-building of vignette outputs
    New result: WARNING
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘Using_Hot_Deck_Data.Rnw’ using Sweave
     Loading required package: mice
    
     Attaching package: 'mice'
    
     The following objects are masked from 'package:base':
    
         cbind, rbind
    
     Warning in hot.deck(isq99, sdCutoff = 3, IDvars = c("IDORIGIN", "YEAR")) :
       52 observations with no observed data.  These observations were removed
    
     Warning in hot.deck(isq99, sdCutoff = 3, IDvars = c("IDORIGIN", "YEAR")) :
       45 of 4661 imputations with # donors < 5, consider increasing sdCutoff or using method='p.draw'
    
     Loading required package: survival
     * miceadds 3.7-6 (2019-12-15 13:38:43)
     Warning: Number of logged events: 1
    
     Error: processing vignette 'Using_Hot_Deck_Data.Rnw' failed with diagnostics:
      chunk 10 (label = conv) 
     Error in Math.data.frame(structure(list(term = structure(1:12, .Label = c("(Intercept)",  : 
       non-numeric variable(s) in data frame: term
    
     --- failed re-building 'Using_Hot_Deck_Data.Rnw'
    
     SUMMARY: processing the following file failed:
       'Using_Hot_Deck_Data.Rnw'
    
     Error: Vignette re-building failed.
     Execution halted

I found this error before, and am in contact with the package maintainer
for a solution (which is simple).

### Problem 3

    Package: miceadds
    Check: examples
    New result: ERROR
     Running examples in ‘miceadds-Ex.R’ failed
     The error most likely occurred in:
    
    base::assign(".ptime", proc.time(), pos = "CheckExEnv")
    ### Name: jomo2datlist
    ### Title: Converts a 'jomo' Data Frame in Long Format into a List of
    ###   Datasets or an Object of Class 'mids'
    ### Aliases: jomo2datlist jomo2mids
    
    ### ** Examples
    
    #############################################################################
    # EXAMPLE 1: Dataset nhanes | jomo imputation and conversion into a data list
    #############################################################################
    
    data( nhanes, package="mice")
    dat <- nhanes
    
    # impute under multivariate normal model in jomo
    imp1 <- jomo::jomo1con(Y=dat, nburn=100, nbetween=10, nimp=5)
     Error in loadNamespace(name) : there is no package called ‘jomo’
     Calls: :: ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
     Execution halted

`mice 3.8.0` removes dependencies, and does not automatically install
anymore the `jomo` package. I have alerted the `miceadds` package
maintainer, and asked for a fix (which is simple). The maintainer
responded to have it fixed by next week.

### Problem 4

`mice 3.7.0` gives an error in
<https://www.r-project.org/nosvn/R.check/r-patched-solaris-x86/mice-00check.html>

This error is particular to `r-patches-solaris-x86`. I cannot reproduce
and have therefore put the example code in `dontrun`.

### Problem 5

`testthat` produced four warnings. This is now fixed.

## Reason

This update `mice 3.8.0` adds new functionality, reduces dependencies
and provides solutions for issues found in `mice 3.7.0` and before.

## Checks

Package built by

``` r
library("devtools")
build()
```

``` bash
R CMD CHECK /Users/buurensv/Package/mice/mice_3.8.0.tar.gz
```

Status: OK

## Test environments

  - local OS X install, 10.15.3, R 3.6.2
  - win-builder, using `devtools::check_win_devel()`

Status:

    Failed to upload/or report

``` r
check_rhub()
```

Status: Two “succes”, two “error”, but it’s not clear to me whether
these are related to `mice`.

## Downstream dependencies

I have run

``` r
library("revdepcheck")
revdepcheck::revdep_reset()
revdep_check(num_workers = 3)
revdep_summary()
```

There were 63 reverse dependencies.

## Failed to check (2)

| package        | version   | error | warning | note |
| :------------- | :-------- | :---- | :------ | :--- |
| dynr           | 0.1.15-25 | 1     |         |      |
| MissingDataGUI | 0.2-5     | 1     |         |      |

The reason is that these package do not install locally on my
system.

## New problems (3)

| package                                        | version | error  | warning | note |
| :--------------------------------------------- | :------ | :----- | :------ | :--- |
| [CALIBERrfimpute](problems.md#caliberrfimpute) | 1.0-1   | **+1** |         |      |
| [cobalt](problems.md#cobalt)                   | 4.0.0   | **+1** |         |      |
| [hot.deck](problems.md#hotdeck)                | 1.1     | **+1** |         |      |

`CALIBERrfimpute` is archived on CRAN.

The `cobalt` problem is caused by a change in `mice::complete()`. There
is a new name clash between the `MatchThem` and `mice 3.8.0`, and
`cobalt` uses both packages. I have noted the `MatchThem` maintainer,
and suggested a fix. The maintainer responded that he would look into
it.

The `hot.deck` problem is caused by a change in the
`mice::summary.mira()` function. I have noted the `hot.deck` maintainer,
and suggested a fix. The maintainer said he’d submitted an update to
CRAN.
