cran-comments
================

## Reason

This update is requested by Kurt Hornik on Nov 27, 2019, who found that
the dependency `CALIBERrfimpute` is not on CRAN anymore. `mice 3.7.0`
removes the dependency, an incorporates various modifications that
accumulated over time.

## Checks

Package built by

``` r
library("devtools")
build()
```

``` bash
R CMD CHECK /Users/buurensv/Package/mice/mice_3.7.0.tar.gz
```

## Test environments

  - local OS X install, 10.15.1, R 3.6.1
  - win-builder, using `devtools::check_win_devel()`

Status: `devtools::check_win_devel()` resulted in:

    * using log directory 'd:/RCompile/CRANguest/R-devel/mice.Rcheck'
    * using R Under development (unstable) (2019-12-09 r77545)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'mice/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'mice' version '3.7.0'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'Stef van Buuren <stef.vanbuuren@tno.nl>'
    * checking package namespace information ... OK
    * checking package dependencies ... OK
    * checking if this is a source package ... OK
    * checking if there is a namespace ... OK
    * checking for hidden files and directories ... OK
    * checking for portable file names ... OK
    * checking serialization versions ... OK
    * checking whether package 'mice' can be installed ... OK
    * checking installed package size ... OK
    * checking package directory ... OK
    * checking for future file timestamps ... OK
    * checking DESCRIPTION meta-information ... OK
    * checking top-level files ... OK
    * checking for left-over files ... OK
    * checking index information ... OK
    * checking package subdirectories ... OK
    * checking R files for non-ASCII characters ... OK
    * checking R files for syntax errors ... OK
    * loading checks for arch 'i386'
    ** checking whether the package can be loaded ... OK
    ** checking whether the package can be loaded with stated dependencies ... OK
    ** checking whether the package can be unloaded cleanly ... OK
    ** checking whether the namespace can be loaded with stated dependencies ... OK
    ** checking whether the namespace can be unloaded cleanly ... OK
    ** checking loading without being on the library search path ... OK
    ** checking use of S3 registration ... OK
    * loading checks for arch 'x64'
    ** checking whether the package can be loaded ... OK
    ** checking whether the package can be loaded with stated dependencies ... OK
    ** checking whether the package can be unloaded cleanly ... OK
    ** checking whether the namespace can be loaded with stated dependencies ... OK
    ** checking whether the namespace can be unloaded cleanly ... OK
    ** checking loading without being on the library search path ... OK
    ** checking use of S3 registration ... OK
    * checking dependencies in R code ... OK
    * checking S3 generic/method consistency ... OK
    * checking replacement functions ... OK
    * checking foreign function calls ... OK
    * checking R code for possible problems ... [32s] OK
    * checking Rd files ... OK
    * checking Rd metadata ... OK
    * checking Rd line widths ... OK
    * checking Rd cross-references ... OK
    * checking for missing documentation entries ... OK
    * checking for code/documentation mismatches ... OK
    * checking Rd \usage sections ... OK
    * checking Rd contents ... OK
    * checking for unstated dependencies in examples ... OK
    * checking contents of 'data' directory ... OK
    * checking data for non-ASCII characters ... OK
    * checking data for ASCII and uncompressed saves ... OK
    * checking line endings in C/C++/Fortran sources/headers ... OK
    * checking line endings in Makefiles ... OK
    * checking compilation flags in Makevars ... OK
    * checking for GNU extensions in Makefiles ... OK
    * checking for portable use of $(BLAS_LIBS) and $(LAPACK_LIBS) ... OK
    * checking use of PKG_*FLAGS in Makefiles ... OK
    * checking use of SHLIB_OPENMP_*FLAGS in Makefiles ... OK
    * checking pragmas in C/C++ headers and code ... OK
    * checking compiled code ... OK
    * checking examples ...
    ** running examples for arch 'i386' ... [53s] OK
    ** running examples for arch 'x64' ... [57s] OK
    * checking for unstated dependencies in 'tests' ... OK
    * checking tests ...
    ** running tests for arch 'i386' ... [85s] ERROR
      Running 'testthat.R' [83s]
    Running the tests in 'tests/testthat.R' failed.
    Complete output:
      > library(testthat)
      > library(mice)
      Loading required package: lattice
      
      Attaching package: 'mice'
      
      The following objects are masked from 'package:base':
      
          cbind, rbind
      
      > 
      > test_check("mice")
      Error in loadNamespace(name) : there is no package called 'cli'
      Calls: test_check ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
      Execution halted
    ** running tests for arch 'x64' ... [95s] ERROR
      Running 'testthat.R' [94s]
    Running the tests in 'tests/testthat.R' failed.
    Complete output:
      > library(testthat)
      > library(mice)
      Loading required package: lattice
      
      Attaching package: 'mice'
      
      The following objects are masked from 'package:base':
      
          cbind, rbind
      
      > 
      > test_check("mice")
      Error in loadNamespace(name) : there is no package called 'cli'
      Calls: test_check ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
      Execution halted
    * checking PDF version of manual ... OK
    * checking for detritus in the temp directory ... OK
    * DONE
    Status: 2 ERRORs

I believe that these errors are due to the win-builder library. I am
afraid I cannot solve these.

## Downstream dependencies

I have run

``` r
library("revdepcheck")
revdepcheck::revdep_reset()
revdep_check(num_workers = 3)
revdep_summary()
```

There were 57 reverse dependencies. There were 5 packages that failed to
check. None of these errors is mice-related.

## Failed to check (5)

| package                  | version  | error  | warning | note |
| :----------------------- | :------- | :----- | :------ | :--- |
| [brms](failures.md#brms) | 2.10.0   | **+1** |         | 1    |
| dynr                     | 0.1.15-1 | 1      |         |      |
| Hmisc                    | 4.3-0    | 1      |         |      |
| MissingDataGUI           | 0.2-5    | 1      |         |      |
| Replication              | 0.1.1    | 1      |         |      |

See <https://github.com/stefvanbuuren/mice/tree/master/revdep>
