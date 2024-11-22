# bipd

<details>

* Version: 0.3
* GitHub: NA
* Source code: https://github.com/cran/bipd
* Date/Publication: 2022-06-05 16:10:05 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::revdep_details(, "bipd")` for more info

</details>

## Newly broken

*   checking dependencies in R code ...sh: line 1: 64105 Segmentation fault: 11  R_DEFAULT_PACKAGES=NULL '/Library/Frameworks/R.framework/Resources/bin/R' --vanilla --no-echo 2>&1 < '/var/folders/5_/g85d42yj50b6lrjq4rzjzg8w0000gn/T//RtmpcESnnw/filef7d530e954ed'
    ```
     NOTE
    
     *** caught segfault ***
    address 0x6c65432f6c61636f, cause 'invalid permissions'
    
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
    ```

## Newly fixed

*   checking dependencies in R code ...sh: line 1: 64022 Segmentation fault: 11  R_DEFAULT_PACKAGES=NULL '/Library/Frameworks/R.framework/Resources/bin/R' --vanilla --no-echo 2>&1 < '/var/folders/5_/g85d42yj50b6lrjq4rzjzg8w0000gn/T//RtmpPAGHHl/filef76c784da58a'
    ```
     NOTE
    
     *** caught segfault ***
    address 0x6c65432f6c61636f, cause 'invalid permissions'
    
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
    ```

# MatchThem

<details>

* Version: 1.0.1
* GitHub: https://github.com/FarhadPishgar/MatchThem
* Source code: https://github.com/cran/MatchThem
* Date/Publication: 2021-08-23 07:11:42 UTC
* Number of recursive dependencies: 63

Run `revdepcheck::revdep_details(, "MatchThem")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'cbind.Rd':
      ‘[mice:cbind.mids]{mice::cbind.mids}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# pre

<details>

* Version: 1.0.6
* GitHub: https://github.com/marjoleinF/pre
* Source code: https://github.com/cran/pre
* Date/Publication: 2023-02-12 22:50:02 UTC
* Number of recursive dependencies: 137

Run `revdepcheck::revdep_details(, "pre")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > test_check("pre")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 122 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test_pre_misc.R:94:3'): cvpre gives previous results with airquality data ──
      Error in `serverSocket(port = port)`: creation of server socket failed: port 11471 cannot be opened
      Backtrace:
          ▆
       1. └─parallel::makeCluster(2L) at test_pre_misc.R:94:2
       2.   └─parallel::makePSOCKcluster(names = spec, ...)
       3.     └─base::serverSocket(port = port)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 122 ]
      Error: Test failures
      Execution halted
    ```

