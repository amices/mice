cran-comments
================

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
and suggested a fix.

The `hot.deck` problem is caused by a change in the
`mice::summary.mira()` function. I have noted the `hot.deck` maintainer,
and suggested a fix. The maintainer said he’d submitted an update to
CRAN.
