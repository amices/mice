cran-comments
================

## mice 3.12.0

New submission.

## Reason

`mice 3.12.0` implements three new features and contains bug fixes.

## Test environments

  - local OS X install, 10.15.7, R 4.0.2
  - win-builder
  - Rhub

## Local check

Package built by

``` r
library("devtools")
build()
```

``` bash
R CMD CHECK mice_3.12.0.tar.gz
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

1.  <https://builder.r-hub.io/status/mice_3.12.0.tar.gz-c1ee4204486443fe91ff1a562649428b>:
    Success
2.  <https://builder.r-hub.io/status/mice_3.12.0.tar.gz-97a2521ab76f45a480967534d56bf9c2>:
    Success
3.  <https://builder.r-hub.io/status/mice_3.12.0.tar.gz-e7d1b6eaeddd463c977514a65908d40e>:
    Success
4.  <https://builder.r-hub.io/status/mice_3.12.0.tar.gz-d1415825ea594cd2befbae2228885d71>:
    Success

## Downstream dependencies

I have run

``` r
library(revdepcheck)
revdep_reset()
revdep_check(num_workers = 10)
```

### `failures.md`

There are one new failure (`brms`) and one old (`dynr`). Both failures
are caused by installation problems on the test servers. These failure
do not seem to be related to `mice`.

### `problems.md`

There are new warnings in several packages because `mice 3.12.0` exports
a new `filter.mids()` method for the `dplyr::filter()` generic. For
example, we get `Warning: replacing previous import ‘mice::filter’ by
‘stats::filter’ when loading ‘BaM’`. Sometimes we get the reverse
message. I noted the package authors the upcoming change in mice on Oct
29. Affected packages: `BaM`, `genpathmox`, `idem`, `misaem`, `Qtools`,
`TestDataImputation`

Here’s the mail:

-----

R packages: BaM, genpathmox, idem, misaem, Qtools, TestDataImputation

Dear package maintainer,

A reversed dependency check revealed a new warning in your package that
results from the upcoming release mice 3.12.0. The new release exports a
new `filter.mids()` method for the `dplyr::filter()` generic.

I get: `Warning: replacing previous import ‘mice::filter’ by
‘stats::filter’ when loading ‘BaM’` and others. For Qtools I get the
reverse: Warning: replacing previous import ‘stats::filter’ by
‘mice::filter’ when loading ‘Qtools’.

The warning is probably benign, but please check in your package if the
introduction of `mice::filter.mids()` would inadvertent change a call to
`stats::filter()` to `dplyr::filter()` or vice versa. If this happens,
please update your import directive.

I intend to submit to CRAN in about one week. Hope this helps to evade
surprises when mice 3.12.0 hits CRAN.

Best wishes, Stef

-----

A second problem concerned to `miceadds` package. This package uses
`mice::.pmm.match`, which I had removed because I thought nobody used it
anymore. I will put it back in.

### README.md

    # Platform
    
    |field    |value                        |
    |:--------|:----------------------------|
    |version  |R version 4.0.2 (2020-06-22) |
    |os       |macOS Catalina 10.15.7       |
    |system   |x86_64, darwin17.0           |
    |ui       |RStudio                      |
    |language |(EN)                         |
    |collate  |en_US.UTF-8                  |
    |ctype    |en_US.UTF-8                  |
    |tz       |Europe/Amsterdam             |
    |date     |2020-10-29                   |
    
    # Dependencies
    
    |package    |old    |new    |Δ  |
    |:----------|:------|:------|:--|
    |mice       |3.11.0 |3.12.0 |*  |
    |assertthat |0.2.1  |0.2.1  |   |
    |backports  |1.1.10 |1.1.10 |   |
    |broom      |0.7.2  |0.7.2  |   |
    |cli        |2.1.0  |2.1.0  |   |
    |cpp11      |0.2.3  |0.2.3  |   |
    |crayon     |1.3.4  |1.3.4  |   |
    |digest     |0.6.27 |0.6.27 |   |
    |dplyr      |1.0.2  |1.0.2  |   |
    |ellipsis   |0.3.1  |0.3.1  |   |
    |fansi      |0.4.1  |0.4.1  |   |
    |generics   |0.0.2  |0.0.2  |   |
    |glue       |1.4.2  |1.4.2  |   |
    |lifecycle  |0.2.0  |0.2.0  |   |
    |magrittr   |1.5    |1.5    |   |
    |pillar     |1.4.6  |1.4.6  |   |
    |pkgconfig  |2.0.3  |2.0.3  |   |
    |purrr      |0.3.4  |0.3.4  |   |
    |R6         |2.5.0  |2.5.0  |   |
    |Rcpp       |1.0.5  |1.0.5  |   |
    |rlang      |0.4.8  |0.4.8  |   |
    |stringi    |1.5.3  |1.5.3  |   |
    |stringr    |1.4.0  |1.4.0  |   |
    |tibble     |3.0.4  |3.0.4  |   |
    |tidyr      |1.1.2  |1.1.2  |   |
    |tidyselect |1.1.0  |1.1.0  |   |
    |utf8       |1.1.4  |1.1.4  |   |
    |vctrs      |0.3.4  |0.3.4  |   |
    
    # Revdeps
    
    ## Failed to check (2)
    
    |package |version   |error |warning |note |
    |:-------|:---------|:-----|:-------|:----|
    |brms    |?         |      |        |     |
    |dynr    |0.1.15-25 |1     |        |     |
    
    ## New problems (7)
    
    |package                                              |version |error |warning |note     |
    |:----------------------------------------------------|:-------|:-----|:-------|:--------|
    |[BaM](problems.md#bam)                               |1.0.1   |      |__+1__  |         |
    |[genpathmox](problems.md#genpathmox)                 |0.4     |      |__+1__  |         |
    |[idem](problems.md#idem)                             |5.0     |      |__+1__  |2        |
    |[miceadds](problems.md#miceadds)                     |3.10-28 |      |        |2 __+1__ |
    |[misaem](problems.md#misaem)                         |1.0.0   |      |__+1__  |         |
    |[Qtools](problems.md#qtools)                         |1.5.2   |      |__+1__  |         |
    |[TestDataImputation](problems.md#testdataimputation) |1.1     |      |__+1__  |         |

This package requires additional software to be installed. See
<https://github.com/amices/mice/blob/master/revdep/failures.md> for
details.
