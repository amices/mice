cran-comments
================

## mice 3.11.00

New submission.

## Reasons

1.  `tidyr` stopped importing `Rccp`, and because of that `mice` failed
    to build on CRAN
2.  Various changes and enhancements since `mice 3.10.0`

## Test environments

  - local OS X install, 10.15.6, R 4.0.2
  - win-builder
  - Rhub

## Local check

Package built by

``` r
library("devtools")
build()
```

``` bash
R CMD CHECK /Users/buurensv/Package/mice/mice_3.11.0.tar.gz
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

    Build ID:   mice_3.11.0.tar.gz-2a50957ce854442eb831250603c96457
    Platform:   Debian Linux, R-devel, GCC ASAN/UBSAN
    Submitted:  2 hours 55.4 seconds ago
    Build time: 2 hours 52.2 seconds
    
    Status: error

There’s a runtime compiler error in `test.check("mice")`, but everything
seems to run. I am not able to infer what to cause is, and assume it is
not related to `mice`.

    Build ID:   mice_3.11.0.tar.gz-0cbfa4e6d65a454692af941423254260
    Platform:   Ubuntu Linux 16.04 LTS, R-release, GCC
    Submitted:  57 minutes 40.7 seconds ago
    Build time: 57 minutes 32.5 seconds
    NOTES:
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘Stef van Buuren <stef.vanbuuren@tno.nl>’
    
    Days since last update: 1
    
    Possibly mis-spelled words in DESCRIPTION:
      Buuren (40:59)
      FCS (39:73)
      Groothuis (41:5)
      Oudshoorn (41:15)
      unordered (44:18)
    * checking examples ... NOTE
    Examples with CPU or elapsed time > 5s
                 user system elapsed
    D3          4.667  0.006   9.076
    D2          4.349  0.048   7.846
    bwplot.mids 2.755  0.009   5.401
    xyplot.mids 2.513  0.012   5.040
    ** found \donttest examples: check also with --run-donttest
    
    Status: success

    mice 3.11.0: NOTE
    Build ID:   mice_3.11.0.tar.gz-ffac7df971234c789e0f33d7915f0167
    Platform:   Windows Server 2008 R2 SP1, R-devel, 32/64 bit
    Submitted:  11 minutes 39.2 seconds ago
    Build time: 11 minutes 31.6 seconds
    NOTES:
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: 'Stef van Buuren <stef.vanbuuren@tno.nl>'
    
    Days since last update: 1
    
    Status: success

    mice 3.11.0: NOTE
    Build ID:   mice_3.11.0.tar.gz-28729f5367314214b71e14247d9ba920
    Platform:   Fedora Linux, R-devel, clang, gfortran
    Submitted:  1 hour 22 minutes 29.1 seconds ago
    Build time: 1 hour 10 minutes 18.8 seconds
    
    NOTES:
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘Stef van Buuren <stef.vanbuuren@tno.nl>’
    
    Days since last update: 1
    
    Possibly mis-spelled words in DESCRIPTION:
      Buuren (40:59)
      FCS (39:73)
      Groothuis (41:5)
      Oudshoorn (41:15)
    * checking examples ... NOTE
    Examples with CPU (user + system) or elapsed time > 5s
                 user system elapsed
    D3          4.549  0.005   7.833
    D2          4.038  0.091   6.457
    bwplot.mids 2.594  0.003   5.048
    
    Status: success

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
    
    |field    |value                        |
    |:--------|:----------------------------|
    |version  |R version 4.0.2 (2020-06-22) |
    |os       |macOS Catalina 10.15.6       |
    |system   |x86_64, darwin17.0           |
    |ui       |RStudio                      |
    |language |(EN)                         |
    |collate  |en_US.UTF-8                  |
    |ctype    |en_US.UTF-8                  |
    |tz       |Europe/Amsterdam             |
    |date     |2020-08-03                   |
    
    # Dependencies
    
    |package    |old      |new    |Δ  |
    |:----------|:--------|:------|:--|
    |mice       |3.10.0.1 |3.11.0 |*  |
    |assertthat |0.2.1    |0.2.1  |   |
    |backports  |1.1.8    |1.1.8  |   |
    |broom      |0.7.0    |0.7.0  |   |
    |cli        |2.0.2    |2.0.2  |   |
    |cpp11      |0.1.0    |0.1.0  |   |
    |crayon     |1.3.4    |1.3.4  |   |
    |digest     |0.6.25   |0.6.25 |   |
    |dplyr      |1.0.1    |1.0.1  |   |
    |ellipsis   |0.3.1    |0.3.1  |   |
    |fansi      |0.4.1    |0.4.1  |   |
    |generics   |0.0.2    |0.0.2  |   |
    |glue       |1.4.1    |1.4.1  |   |
    |lifecycle  |0.2.0    |0.2.0  |   |
    |magrittr   |1.5      |1.5    |   |
    |pillar     |1.4.6    |1.4.6  |   |
    |pkgconfig  |2.0.3    |2.0.3  |   |
    |purrr      |0.3.4    |0.3.4  |   |
    |R6         |2.4.1    |2.4.1  |   |
    |Rcpp       |1.0.5    |1.0.5  |   |
    |rlang      |0.4.7    |0.4.7  |   |
    |stringi    |1.4.6    |1.4.6  |   |
    |stringr    |1.4.0    |1.4.0  |   |
    |tibble     |3.0.3    |3.0.3  |   |
    |tidyr      |1.1.1    |1.1.1  |   |
    |tidyselect |1.1.0    |1.1.0  |   |
    |utf8       |1.1.4    |1.1.4  |   |
    |vctrs      |0.3.2    |0.3.2  |   |
    
    # Revdeps
    
    ## Failed to check (1)
    
    |package |version   |error |warning |note |
    |:-------|:---------|:-----|:-------|:----|
    |dynr    |0.1.15-25 |1     |        |     |

This package requires additional software to be installed. See
<https://github.com/stefvanbuuren/mice/blob/master/revdep/failures.md>
for details.
