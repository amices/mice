cran-comments
================

Package built by

``` r
library("devtools")
build()
```

`"/Users/buurensv/Package/mice/mice_3.4.0.tar.gz"`

Test environments
-----------------

-   local OS X install, 10.14.3, R 3.5.2
-   win-builder, using `devtools::check_win_devel()`

Status: OK

R CMD check results
-------------------

local checks of tarball:

`$ R CMD CHECK mice_3.4.0.tar.gz`

Status: OK

Downstream dependencies
-----------------------

I have run

``` r
library("revdepcheck")
revdep_check(num_workers = 3)
revdep_summary()
```

There were 52 reverse dependencies. There were 6 packages with errors. None of these errors is mice-related.

See <https://github.com/stefvanbuuren/mice/tree/master/revdep>
