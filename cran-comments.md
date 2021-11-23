cran-comments
================

## mice 3.14.0

New submission.

## Reason

A report from CRAN indicates that `install.on.demand()` breaks the CRAN
work flow, so an urgent update is needed. In addition, there are many
changes and improvements since the last CRAN version 3.13.0 published in
Jan 2021.

## Test environments

-   local OS X install, 11.6, R 4.1.2
-   win-builder
-   Rhub

## Local check

Package built by

``` r
library("devtools")
build()
```

``` bash
R CMD CHECK mice_3.14.0.tar.gz
```

Status: OK

## win-builder

``` r
devtools::check_win_devel()
```

Status: OK

## Rhub checks

``` r
devtools::check_rhub()
```

Results:

1.  Debian Linux, R-devel, GCC ASAN/UBSAN: Success
2.  Ubuntu Linux 20.04.1 LTS, R-release, GCC: Success
3.  Fedora Linux, R-devel, clang, gfortran: Success

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

    # CALIBERrfimpute

    <details>

    * Version: 1.0-5
    * GitHub: NA
    * Source code: https://github.com/cran/CALIBERrfimpute
    * Date/Publication: 2021-05-05 09:00:04
    * Number of recursive dependencies: 53

    Run `revdep_details(, "CALIBERrfimpute")` for more info

    </details>

    ## Newly broken

    *   checking running R code from vignettes ...
          ‘simstudy_survival.Rnw’ using ‘UTF-8’... failed
         ERROR
        Errors in running code in vignettes:
        when running code in ‘simstudy_survival.Rnw’
          ...
          x2 & 0.0391  & 0.0254  & 0.0138  \\ 
          x3 & -0.0314  & -0.00854  & -0.0228  \\ 
           \hline
        \end{tabular}
        
        \vspace{1em}
        
          When sourcing ‘simstudy_survival.R’:
        Error: missing value where TRUE/FALSE needed
        Execution halted

Not sure whether it’s related to `mice`, and seems relatively benign if
it is. The package maintainer of `CALIBERrfimpute` is aware of the
problem, and will look into the issue.
