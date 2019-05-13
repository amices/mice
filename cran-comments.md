cran-comments
================

Package built by

``` r
library("devtools")
build()
```

``` bash
R CMD CHECK /Users/buurensv/Package/mice/mice_3.5.0.tar.gz
```

    ## * using log directory ‘/Users/buurensv/Package/mice/mice/mice.Rcheck’
    ## * using R version 3.6.0 (2019-04-26)
    ## * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    ## * using session charset: UTF-8
    ## * checking for file ‘mice/DESCRIPTION’ ... OK
    ## * checking extension type ... Package
    ## * this is package ‘mice’ version ‘3.5.0’
    ## * package encoding: UTF-8
    ## * checking package namespace information ... OK
    ## * checking package dependencies ... OK
    ## * checking if this is a source package ... OK
    ## * checking if there is a namespace ... OK
    ## * checking for executable files ... OK
    ## * checking for hidden files and directories ... OK
    ## * checking for portable file names ... OK
    ## * checking for sufficient/correct file permissions ... OK
    ## * checking whether package ‘mice’ can be installed ... OK
    ## * checking installed package size ... OK
    ## * checking package directory ... OK
    ## * checking DESCRIPTION meta-information ... OK
    ## * checking top-level files ... OK
    ## * checking for left-over files ... OK
    ## * checking index information ... OK
    ## * checking package subdirectories ... OK
    ## * checking R files for non-ASCII characters ... OK
    ## * checking R files for syntax errors ... OK
    ## * checking whether the package can be loaded ... OK
    ## * checking whether the package can be loaded with stated dependencies ... OK
    ## * checking whether the package can be unloaded cleanly ... OK
    ## * checking whether the namespace can be loaded with stated dependencies ... OK
    ## * checking whether the namespace can be unloaded cleanly ... OK
    ## * checking dependencies in R code ... OK
    ## * checking S3 generic/method consistency ... OK
    ## * checking replacement functions ... OK
    ## * checking foreign function calls ... OK
    ## * checking R code for possible problems ... OK
    ## * checking Rd files ... OK
    ## * checking Rd metadata ... OK
    ## * checking Rd cross-references ... OK
    ## * checking for missing documentation entries ... OK
    ## * checking for code/documentation mismatches ... OK
    ## * checking Rd \usage sections ... OK
    ## * checking Rd contents ... OK
    ## * checking for unstated dependencies in examples ... OK
    ## * checking contents of ‘data’ directory ... OK
    ## * checking data for non-ASCII characters ... OK
    ## * checking data for ASCII and uncompressed saves ... OK
    ## * checking line endings in C/C++/Fortran sources/headers ... OK
    ## * checking line endings in Makefiles ... OK
    ## * checking compilation flags in Makevars ... OK
    ## * checking for GNU extensions in Makefiles ... OK
    ## * checking for portable use of $(BLAS_LIBS) and $(LAPACK_LIBS) ... OK
    ## * checking compiled code ... OK
    ## * checking examples ... OK
    ## * checking for unstated dependencies in ‘tests’ ... OK
    ## * checking tests ...
    ##   Running ‘testthat.R’
    ##  OK
    ## * checking PDF version of manual ... OK
    ## * DONE
    ## 
    ## Status: OK

## Test environments

  - local OS X install, 10.14.4, R 3.6.0
  - win-builder, using `devtools::check_win_devel()`

Status: OK

## Downstream dependencies

I have run

``` r
library("revdepcheck")
revdep_check(num_workers = 3)
revdep_summary()
```

There were 55 reverse dependencies. There were 4 packages that failed to
check. None of these errors is mice-related.

See <https://github.com/stefvanbuuren/mice/tree/master/revdep>
