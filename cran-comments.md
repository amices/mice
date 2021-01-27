cran-comments
================

## mice 3.13.0

New submission.

## Reason

After signal sent by Brian Ripley on Jan26. `mice 3.13.0` implements a
fix for a bug that produced errors with `mitml 0.4-0`.

## Test environments

-   local OS X install, 11.11, R 4.0.3
-   win-builder
-   Rhub

## Local check

Package built by

``` r
library("devtools")
build()
```

``` bash
R CMD CHECK mice_3.13.0.tar.gz
```

Status: OK

## win-builder

``` r
devtools::check_win_devel()
```

Status: OK

## Rhub checks

``` r
check_rhub()
```

Results:

1.  Debian Linux, R-devel, GCC ASAN/UBSAN: Error, compilation failed for
    package `miceadds`
2.  Windows Server 2008 R2 SP1, R-devel, 32/64 bit: Finished: SUCCESS
3.  Ubuntu Linux 20.04.1 LTS, R-release, GCC: Success
4.  Fedora Linux, R-devel, clang, gfortran: Success, with four (benign)
    warnings about CPU time for examples

## Downstream dependencies

I have run

``` r
library(revdepcheck)
revdep_reset()
revdep_check(num_workers = 10)
```

### `failures.md`

There is one old failure (`dynr`):
`configure: error: gsl-config not found, is GSL installed?` Not related
to `mice`.

### `problems.md`

*Wow, no problems at all. :)*

### README.md

    # Platform

    |field    |value                        |
    |:--------|:----------------------------|
    |version  |R version 4.0.3 (2020-10-10) |
    |os       |macOS Big Sur 10.16          |
    |system   |x86_64, darwin17.0           |
    |ui       |RStudio                      |
    |language |(EN)                         |
    |collate  |en_US.UTF-8                  |
    |ctype    |en_US.UTF-8                  |
    |tz       |Europe/Amsterdam             |
    |date     |2021-01-26                   |

    # Dependencies

    |package    |old    |new    |Δ  |
    |:----------|:------|:------|:--|
    |mice       |3.12.0 |3.13.0 |*  |
    |assertthat |0.2.1  |0.2.1  |   |
    |backports  |1.2.1  |1.2.1  |   |
    |broom      |0.7.3  |0.7.3  |   |
    |cli        |2.2.0  |2.2.0  |   |
    |cpp11      |0.2.5  |0.2.5  |   |
    |crayon     |1.3.4  |1.3.4  |   |
    |digest     |0.6.27 |0.6.27 |   |
    |dplyr      |1.0.3  |1.0.3  |   |
    |ellipsis   |0.3.1  |0.3.1  |   |
    |fansi      |0.4.2  |0.4.2  |   |
    |generics   |0.1.0  |0.1.0  |   |
    |glue       |1.4.2  |1.4.2  |   |
    |lifecycle  |0.2.0  |0.2.0  |   |
    |magrittr   |2.0.1  |2.0.1  |   |
    |pillar     |1.4.7  |1.4.7  |   |
    |pkgconfig  |2.0.3  |2.0.3  |   |
    |purrr      |0.3.4  |0.3.4  |   |
    |R6         |2.5.0  |2.5.0  |   |
    |Rcpp       |1.0.6  |1.0.6  |   |
    |rlang      |0.4.10 |0.4.10 |   |
    |stringi    |1.5.3  |1.5.3  |   |
    |stringr    |1.4.0  |1.4.0  |   |
    |tibble     |3.0.5  |3.0.5  |   |
    |tidyr      |1.1.2  |1.1.2  |   |
    |tidyselect |1.1.0  |1.1.0  |   |
    |utf8       |1.1.4  |1.1.4  |   |
    |vctrs      |0.3.6  |0.3.6  |   |

    # Revdeps

    ## Failed to check (1)

    |package |version   |error |warning |note |
    |:-------|:---------|:-----|:-------|:----|
    |dynr    |0.1.15-25 |1     |        |     |
