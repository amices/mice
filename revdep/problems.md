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

*   checking dependencies in R code ...sh: line 1: 69552 Segmentation fault: 11  R_DEFAULT_PACKAGES=NULL '/Library/Frameworks/R.framework/Resources/bin/R' --vanilla --no-echo 2>&1 < '/var/folders/5_/g85d42yj50b6lrjq4rzjzg8w0000gn/T//RtmpLrr3Tc/file10d343b141914'
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

*   checking dependencies in R code ...sh: line 1: 69510 Segmentation fault: 11  R_DEFAULT_PACKAGES=NULL '/Library/Frameworks/R.framework/Resources/bin/R' --vanilla --no-echo 2>&1 < '/var/folders/5_/g85d42yj50b6lrjq4rzjzg8w0000gn/T//RtmpwXaix2/file10cf9db24eb7'
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

