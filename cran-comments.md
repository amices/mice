cran-comments
================

## `mice 3.9.0`, new submission

## Reason

This update `mice 3.9.0` adds new functionality, reduces dependencies
and provides solutions for issues found in `mice 3.8.0` and before.

## Test environments

  - local OS X install, 10.15.4, R 4.0.0
  - win-builder
  - Rhub

## Local check

Package built by

``` r
library("devtools")
build()
```

``` bash
R CMD CHECK /Users/buurensv/Package/mice/mice_3.9.0.tar.gz
```

Fails, although interactive building in Rstudio works fine

    * installing *source* package ‘mice’ ...
    ** using staged installation
    ** libs
    clang++ -mmacosx-version-min=10.13 -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/buurensv/Library/R/3.6/library/Rcpp/include' -I/usr/local/include   -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
    clang++ -mmacosx-version-min=10.13 -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/buurensv/Library/R/3.6/library/Rcpp/include' -I/usr/local/include   -fPIC  -Wall -g -O2  -c match.cpp -o match.o
    Error: package ‘Rcpp’ was installed before R 4.0.0: please re-install it
    Execution halted
    clang++ -mmacosx-version-min=10.13 -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o mice.so RcppExports.o match.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
    Error: package ‘Rcpp’ was installed before R 4.0.0: please re-install it
    Execution halted
    installing to /Users/buurensv/Dropbox/Package/mice/mice/mice.Rcheck/00LOCK-mice/00new/mice/libs
    ** R
    ** data
    *** moving datasets to lazyload DB
    ** inst
    ** byte-compile and prepare package for lazy loading
    Error: package ‘broom’ was installed before R 4.0.0: please re-install it
    Execution halted
    ERROR: lazy loading failed for package ‘mice’
    * removing ‘/Users/buurensv/Dropbox/Package/mice/mice/mice.Rcheck/mice’

I do not understand this message. After upgrading the `R 4.0.0`, I
refreshed all packages, as required. Re-installing `Rcpp` manually did
not solve the problem. I will assume it is a false positive.

## win-builder

``` r
devtools::check_win_devel()
```

Status: OK

## Rhub checks

``` r
check_rhub()
```

    mice 3.9.0: NOTE
    Build ID:   mice_3.9.0.tar.gz-96a5a0f7e247483493124db3ee412d47
    Platform:   Windows Server 2008 R2 SP1, R-devel, 32/64 bit
    
    * checking examples ...
    ** running examples for arch 'i386' ... NOTE
    Examples with CPU (user + system) or elapsed time > 5s
    D1 5.58   0.05    5.68
       user system elapsed
    ** running examples for arch 'x64' ... NOTE
    Examples with CPU (user + system) or elapsed time > 5s
       user system elapsed
    D1  5.7   0.06    5.77

    mice 3.9.0: NOTE
    Build ID:   mice_3.9.0.tar.gz-76d8ae5abbb941fc816b6716061457f4
    Platform:   Ubuntu Linux 16.04 LTS, R-release, GCC
    
    * checking examples ... NOTE
    Examples with CPU or elapsed time > 5s
        user system elapsed
    D1 3.908  0.136   7.817
    D3 3.036  0.044   6.117
    D2 3.000  0.020   5.797
    ** found \donttest examples: check also with --run-donttest

    mice 3.9.0: ERROR
    Build ID:   mice_3.9.0.tar.gz-91b0be74f9784bf484650eebf312a8b8
    Platform:   Fedora Linux, R-devel, clang, gfortran
    
    * checking examples ... ERROR
    Running examples in ‘mice-Ex.R’ failed
    The error most likely occurred in:
    
    > base::assign(".ptime", proc.time(), pos = "CheckExEnv")
    > ### Name: mice.impute.2l.bin
    > ### Title: Imputation by a two-level logistic model using 'glmer'
    > ### Aliases: mice.impute.2l.bin
    > ### Keywords: datagen
    > 
    > ### ** Examples
    > 
    > library(tidyr)
    > library(dplyr)
    
    Attaching package: ‘dplyr’
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > data("toenail2")
    > data <- tidyr::complete(toenail2, patientID, visit) %>% 
    +  tidyr::fill(treatment) %>% 
    +  dplyr::select(-time) %>%
    +  dplyr::mutate(patientID = as.integer(patientID))
    Error in bquote(on.exit({ : 'where' must be an environment
    Calls: %>% ... replace_na -> <Anonymous> -> eval_bare -> exit_handler -> bquote
    Execution halted

This error is in `dplyr` code execution. I don’t think I can solve it.

    mice 3.9.0: PREPERROR
    Build ID:   mice_3.9.0.tar.gz-9aa8a9462c524b0dad4e60c9f2684fed
    Platform:   Debian Linux, R-devel, GCC ASAN/UBSAN
    
    Status for build mice_3.9.0.tar.gz-9aa8a9462c524b0dad4e60c9f2684fed
    Status: success 
    Duration: 1 hour 34 minutes 26.3 seconds

## Downstream dependencies

I have run

``` r
library(revdepcheck)
revdep_reset()
revdep_check(num_workers = 3)
revdep_summary()
```

### `problems.md`

    *Wow, no problems at all. :)*

### README.md

# Platform

| field    | value                        |
| :------- | :--------------------------- |
| version  | R version 4.0.0 (2020-04-24) |
| os       | macOS Catalina 10.15.4       |
| system   | x86\_64, darwin17.0          |
| ui       | RStudio                      |
| language | (EN)                         |
| collate  | en\_US.UTF-8                 |
| ctype    | en\_US.UTF-8                 |
| tz       | Europe/Amsterdam             |
| date     | 2020-05-14                   |

# Dependencies

| package    | old      | new      | Δ  |
| :--------- | :------- | :------- | :- |
| mice       | 3.8.0    | 3.9.0    | \* |
| assertthat | 0.2.1    | 0.2.1    |    |
| backports  | 1.1.7    | 1.1.7    |    |
| BH         | 1.72.0-3 | 1.72.0-3 |    |
| broom      | 0.5.6    | 0.5.6    |    |
| cli        | 2.0.2    | 2.0.2    |    |
| crayon     | 1.3.4    | 1.3.4    |    |
| digest     | 0.6.25   | 0.6.25   |    |
| dplyr      | 0.8.5    | 0.8.5    |    |
| ellipsis   | 0.3.0    | 0.3.0    |    |
| fansi      | 0.4.1    | 0.4.1    |    |
| generics   | 0.0.2    | 0.0.2    |    |
| glue       | 1.4.1    | 1.4.1    |    |
| lifecycle  | 0.2.0    | 0.2.0    |    |
| magrittr   | 1.5      | 1.5      |    |
| pillar     | 1.4.4    | 1.4.4    |    |
| pkgconfig  | 2.0.3    | 2.0.3    |    |
| plogr      | 0.2.0    | 0.2.0    |    |
| plyr       | 1.8.6    | 1.8.6    |    |
| purrr      | 0.3.4    | 0.3.4    |    |
| R6         | 2.4.1    | 2.4.1    |    |
| Rcpp       | 1.0.4.6  | 1.0.4.6  |    |
| reshape2   | 1.4.4    | 1.4.4    |    |
| rlang      | 0.4.6    | 0.4.6    |    |
| stringi    | 1.4.6    | 1.4.6    |    |
| stringr    | 1.4.0    | 1.4.0    |    |
| tibble     | 3.0.1    | 3.0.1    |    |
| tidyr      | 1.0.3    | 1.0.3    |    |
| tidyselect | 1.1.0    | 1.1.0    |    |
| utf8       | 1.1.4    | 1.1.4    |    |
| vctrs      | 0.3.0    | 0.3.0    |    |

## Revdeps

### Failed to check (4)

| package        | version   | error | warning | note |
| :------------- | :-------- | :---- | :------ | :--- |
| brms           | 2.12.0    | 1     |         | 1    |
| dynr           | 0.1.15-25 | 1     |         |      |
| MissingDataGUI | 0.2-5     | 1     |         |      |
| Replication    | 0.1.2     | 1     |         |      |

These package require additional software to be installed. See
<https://github.com/stefvanbuuren/mice/blob/master/revdep/failures.md>
for details.
