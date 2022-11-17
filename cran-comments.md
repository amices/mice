cran-comments
================

## mice 3.15.0

New submission.

## Reason

`mice 3.15.0` contains many changes and enhancements over `mice 3.14.0`

## Test environments

### Local

``` r
R.Version()
```

    ## $platform
    ## [1] "aarch64-apple-darwin20"
    ## 
    ## $arch
    ## [1] "aarch64"
    ## 
    ## $os
    ## [1] "darwin20"
    ## 
    ## $system
    ## [1] "aarch64, darwin20"
    ## 
    ## $status
    ## [1] ""
    ## 
    ## $major
    ## [1] "4"
    ## 
    ## $minor
    ## [1] "2.2"
    ## 
    ## $year
    ## [1] "2022"
    ## 
    ## $month
    ## [1] "10"
    ## 
    ## $day
    ## [1] "31"
    ## 
    ## $`svn rev`
    ## [1] "83211"
    ## 
    ## $language
    ## [1] "R"
    ## 
    ## $version.string
    ## [1] "R version 4.2.2 (2022-10-31)"
    ## 
    ## $nickname
    ## [1] "Innocent and Trusting"

### win-builder

### \* Rhub

## Local check

Package built by

``` r
library("devtools")
build()
```

``` bash
R CMD CHECK mice_3.15.0.tar.gz
```

Status: OK

## win-builder

``` r
devtools::check_win_devel()
```

Status: 1 NOTE

    Found the following (possibly) invalid URLs:
      URL: https://journals.sagepub.com/doi/10.1177/0049124118799376
        From: man/ampute.Rd
        Status: 503
        Message: Service Unavailable
      URL: https://journals.sagepub.com/doi/full/10.1177/0049124118799376
        From: man/ampute.Rd
        Status: 503
        Message: Service Unavailable

SvB: The URLs exist. I believe the NOTE is caused by blocking by SAGE.

## Rhub checks

``` r
devtools::check_rhub()
```

Results:

1.  Debian Linux, R-devel, GCC ASAN/UBSAN: Build timed out (after 20
    minutes). Marking the build as failed.
2.  Windows Server 2022, R-devel, 64 bit: 0 errors ✔ \| 0 warnings ✔ \|
    0 notes ✔
3.  Ubuntu Linux 20.04.1 LTS, R-release, GCC: SUCCESS
4.  Fedora Linux, R-devel, clang, gfortran: SUCCES

## Downstream dependencies

`mice` has 110 downstream dependencies

``` r
library(revdepcheck)
revdep_reset()
revdep_check(num_workers = 10)
```

    OK: 108                                                                                                                                                    
    BROKEN: 2
    Total time: 22 min
    ── REPORT ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    Writing summary to 'revdep/README.md'
    Writing problems to 'revdep/problems.md'
    Writing failures to 'revdep/failures.md'
    Writing CRAN report to 'revdep/cran.md'

### `failures.md`

    # dynr

    <details>

    * Version: 0.1.16-91
    * GitHub: https://github.com/mhunter1/dynr
    * Source code: https://github.com/cran/dynr
    * Date/Publication: 2022-10-17 07:02:35 UTC
    * Number of recursive dependencies: 120

    Run `revdepcheck::revdep_details(, "dynr")` for more info

    </details>

    ## In both

    *   checking whether package ‘dynr’ can be installed ... ERROR

        Installation failed.
        See ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/dynr/new/dynr.Rcheck/00install.out’ for details.

    ## Installation

    ### Devel

    * installing *source* package ‘dynr’ ...
    ** package ‘dynr’ successfully unpacked and MD5 sums checked
    ** using staged installation
    checking for gsl-config... no
    configure: error: gsl-config not found, is GSL installed?
    ERROR: configuration failed for package ‘dynr’
    * removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/dynr/new/dynr.Rcheck/dynr’


    ### CRAN

    * installing *source* package ‘dynr’ ...
    ** package ‘dynr’ successfully unpacked and MD5 sums checked
    ** using staged installation
    checking for gsl-config... no
    configure: error: gsl-config not found, is GSL installed?
    ERROR: configuration failed for package ‘dynr’
    * removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/dynr/old/dynr.Rcheck/dynr’


    # pguIMP

    <details>

    * Version: 0.0.0.3
    * GitHub: https://github.com/SMLMS/pguIMP
    * Source code: https://github.com/cran/pguIMP
    * Date/Publication: 2021-09-30 11:50:02 UTC
    * Number of recursive dependencies: 221

    Run `revdepcheck::revdep_details(, "pguIMP")` for more info

    </details>

    ## In both

    *   checking whether package ‘pguIMP’ can be installed ... ERROR

        Installation failed.
        See ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/pguIMP/new/pguIMP.Rcheck/00install.out’ for details.

    ## Installation

    ### Devel

    * installing *source* package ‘pguIMP’ ...
    ** package ‘pguIMP’ successfully unpacked and MD5 sums checked
    ** using staged installation
    ** R
    ** inst
    ** byte-compile and prepare package for lazy loading
    Error: .onLoad failed in loadNamespace() for 'rJava', details:
      call: dyn.load(jli, FALSE)
      error: unable to load shared object '/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jli/libjli.dylib':
      dlopen(/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jli/libjli.dylib, 0x000A): tried: '/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jli/libjli.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64')), '/System/Volumes/Preboot/Cryptexes/OS/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jli/libjli.dylib' (no such file), '/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jli/libjli.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64'))
    Execution halted
    ERROR: lazy loading failed for package ‘pguIMP’
    * removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/pguIMP/new/pguIMP.Rcheck/pguIMP’


    ### CRAN

    * installing *source* package ‘pguIMP’ ...
    ** package ‘pguIMP’ successfully unpacked and MD5 sums checked
    ** using staged installation
    ** R
    ** inst
    ** byte-compile and prepare package for lazy loading
    Error: .onLoad failed in loadNamespace() for 'rJava', details:
      call: dyn.load(jli, FALSE)
      error: unable to load shared object '/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jli/libjli.dylib':
      dlopen(/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jli/libjli.dylib, 0x000A): tried: '/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jli/libjli.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64')), '/System/Volumes/Preboot/Cryptexes/OS/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jli/libjli.dylib' (no such file), '/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jli/libjli.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64'))
    Execution halted
    ERROR: lazy loading failed for package ‘pguIMP’
    * removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/pguIMP/old/pguIMP.Rcheck/pguIMP’


    # Replication

    <details>

    * Version: 0.1.2
    * GitHub: NA
    * Source code: https://github.com/cran/Replication
    * Date/Publication: 2020-04-09 12:10:02 UTC
    * Number of recursive dependencies: 90

    Run `revdepcheck::revdep_details(, "Replication")` for more info

    </details>

    ## In both

    *   checking whether package ‘Replication’ can be installed ... ERROR
        Installation failed.
        See ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/Replication/new/Replication.Rcheck/00install.out’ for details.

    ## Installation

    ### Devel

    * installing *source* package ‘Replication’ ...
    ** package ‘Replication’ successfully unpacked and MD5 sums checked
    ** using staged installation
    ** R
    ** byte-compile and prepare package for lazy loading
    Error: .onLoad failed in loadNamespace() for 'rjags', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/Replication/rjags/libs/rjags.so':
      dlopen(/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/Replication/rjags/libs/rjags.so, 0x000A): Library not loaded: /usr/local/lib/libjags.4.dylib
      Referenced from: <82D1D1F8-CC85-3273-A9BC-B86843329438> /Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/Replication/rjags/libs/rjags.so
      Reason: tried: '/usr/local/lib/libjags.4.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64')), '/System/Volumes/Preboot/Cryptexes/OS/usr/local/lib/libjags.4.dylib' (no such file), '/usr/local/lib/libjags.4.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64')), '/Library/Frameworks/R.framework/Resources/lib/libjags.4.dylib' (no such file), '/Library/Java/Jav
    Execution halted
    ERROR: lazy loading failed for package ‘Replication’
    * removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/Replication/new/Replication.Rcheck/Replication’


    ### CRAN

    * installing *source* package ‘Replication’ ...
    ** package ‘Replication’ successfully unpacked and MD5 sums checked
    ** using staged installation
    ** R
    ** byte-compile and prepare package for lazy loading
    Error: .onLoad failed in loadNamespace() for 'rjags', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/Replication/rjags/libs/rjags.so':
      dlopen(/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/Replication/rjags/libs/rjags.so, 0x000A): Library not loaded: /usr/local/lib/libjags.4.dylib
      Referenced from: <82D1D1F8-CC85-3273-A9BC-B86843329438> /Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/Replication/rjags/libs/rjags.so
      Reason: tried: '/usr/local/lib/libjags.4.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64')), '/System/Volumes/Preboot/Cryptexes/OS/usr/local/lib/libjags.4.dylib' (no such file), '/usr/local/lib/libjags.4.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64')), '/Library/Frameworks/R.framework/Resources/lib/libjags.4.dylib' (no such file), '/Library/Java/Jav
    Execution halted
    ERROR: lazy loading failed for package ‘Replication’
    * removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/Replication/old/Replication.Rcheck/Replication’

SvB: I believe that none of these issues are attributable to `mice`.

### `problems.md`

    # adjustedCurves

    <details>

    * Version: 0.9.0
    * GitHub: https://github.com/RobinDenz1/adjustedCurves
    * Source code: https://github.com/cran/adjustedCurves
    * Date/Publication: 2022-09-22 08:40:13 UTC
    * Number of recursive dependencies: 167

    Run `revdepcheck::revdep_details(, "adjustedCurves")` for more info

    </details>

    ## Newly broken

    *   checking tests ...
          Running ‘testthat.R’
         ERROR
        Running the tests in ‘tests/testthat.R’ failed.
        Last 13 lines of output:
            5. ├─dplyr:::summarise.grouped_df(...)
            6. │ └─dplyr:::summarise_cols(.data, dplyr_quosures(...), caller_env = caller_env())
            7. │   ├─base::withCallingHandlers(...)
            8. │   └─dplyr:::map(quosures, summarise_eval_one, mask = mask)
            9. │     └─base::lapply(.x, .f, ...)
           10. │       └─dplyr (local) FUN(X[[i]], ...)
           11. │         └─mask$eval_all_summarise(quo)
           12. ├─adjustedCurves:::pool_p_values(p_val)
           13. └─base::.handleSimpleError(...)
           14.   └─dplyr (local) h(simpleError(msg, call))
           15.     └─rlang::abort(bullets, call = error_call, parent = skip_internal_condition(e))
          
          [ FAIL 7 | WARN 126 | SKIP 125 | PASS 1579 ]
          Error: Test failures
          Execution halted

    # bipd

    <details>

    * Version: 0.3
    * GitHub: NA
    * Source code: https://github.com/cran/bipd
    * Date/Publication: 2022-06-05 16:10:05 UTC
    * Number of recursive dependencies: 107

    Run `revdepcheck::revdep_details(, "bipd")` for more info

    </details>

    ## Newly broken

    *   checking dependencies in R code ...sh: line 1: 26776 Segmentation fault: 11  R_DEFAULT_PACKAGES=NULL '/Library/Frameworks/R.framework/Resources/bin/R' --vanilla --no-echo 2>&1 < '/var/folders/5_/g85d42yj50b6lrjq4rzjzg8w0000gn/T//RtmpKRyE8b/file66d821b07fdc'
         NOTE
        
         *** caught segfault ***
        address 0x6c6c65432f6c6163, cause 'invalid permissions'
        
        Traceback:
         1: dyn.load(file, DLLpath = DLLpath, ...)
         2: library.dynam(lib, package, package.lib)
         3: loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
         4: asNamespace(ns)
        ...
         7: withCallingHandlers(expr, message = function(c) if (inherits(c,     classes)) tryInvokeRestart("muffleMessage"))
         8: suppressMessages(loadNamespace(p))
         9: withCallingHandlers(expr, warning = function(w) if (inherits(w,     classes)) tryInvokeRestart("muffleWarning"))
        10: suppressWarnings(suppressMessages(loadNamespace(p)))
        11: doTryCatch(return(expr), name, parentenv, handler)
        12: tryCatchOne(expr, names, parentenv, handlers[[1L]])
        13: tryCatchList(expr, classes, parentenv, handlers)
        14: tryCatch(suppressWarnings(suppressMessages(loadNamespace(p))),     error = function(e) e)
        15: tools:::.check_packages_used(package = "bipd")
        An irrecoverable exception occurred. R is aborting now ...

    ## Newly fixed

    *   checking dependencies in R code ...sh: line 1: 26657 Segmentation fault: 11  R_DEFAULT_PACKAGES=NULL '/Library/Frameworks/R.framework/Resources/bin/R' --vanilla --no-echo 2>&1 < '/var/folders/5_/g85d42yj50b6lrjq4rzjzg8w0000gn/T//Rtmpi8gCya/file6689272a04f8'
         NOTE
        
         *** caught segfault ***
        address 0x6c6c65432f6c6163, cause 'invalid permissions'
        
        Traceback:
         1: dyn.load(file, DLLpath = DLLpath, ...)
         2: library.dynam(lib, package, package.lib)
         3: loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
         4: asNamespace(ns)
        ...
         7: withCallingHandlers(expr, message = function(c) if (inherits(c,     classes)) tryInvokeRestart("muffleMessage"))
         8: suppressMessages(loadNamespace(p))
         9: withCallingHandlers(expr, warning = function(w) if (inherits(w,     classes)) tryInvokeRestart("muffleWarning"))
        10: suppressWarnings(suppressMessages(loadNamespace(p)))
        11: doTryCatch(return(expr), name, parentenv, handler)
        12: tryCatchOne(expr, names, parentenv, handlers[[1L]])
        13: tryCatchList(expr, classes, parentenv, handlers)
        14: tryCatch(suppressWarnings(suppressMessages(loadNamespace(p))),     error = function(e) e)
        15: tools:::.check_packages_used(package = "bipd")
        An irrecoverable exception occurred. R is aborting now ...

#### Action taken

##### adjustedCurves

SvB: I contacted the maintainer on 14/11 and he explained that the error
is due to hard-coded values in some of the tests in `adjustedCurves`.
Since the random generator has changed in `mice 3.15.0`, these tests now
fail. I interpret the error as expected behaviour.

##### bipd

SvB: This problem seems to be a configuration or memory issue, not
related to `mice`. I took no further action.
