# bipd

<details>

* Version: 0.3
* GitHub: NA
* Source code: https://github.com/cran/bipd
* Date/Publication: 2022-06-05 16:10:05 UTC
* Number of recursive dependencies: 150

Run `revdepcheck::revdep_details(, "bipd")` for more info

</details>

## Newly broken

*   checking dependencies in R code ...sh: line 1: 58727 Segmentation fault: 11  R_DEFAULT_PACKAGES=NULL '/Library/Frameworks/R.framework/Resources/bin/R' --vanilla --no-echo 2>&1 < '/var/folders/5_/g85d42yj50b6lrjq4rzjzg8w0000gn/T//RtmpM38C1S/filee1cf761be8f7'
    ```
     NOTE
    
     *** caught segfault ***
    address 0x656d756c6f562f6d, cause 'invalid permissions'
    
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

*   checking dependencies in R code ...sh: line 1: 58721 Segmentation fault: 11  R_DEFAULT_PACKAGES=NULL '/Library/Frameworks/R.framework/Resources/bin/R' --vanilla --no-echo 2>&1 < '/var/folders/5_/g85d42yj50b6lrjq4rzjzg8w0000gn/T//Rtmpxl1LPe/filee1b045bcd2d0'
    ```
     NOTE
    
     *** caught segfault ***
    address 0x656d756c6f562f6d, cause 'invalid permissions'
    
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

## In both

*   checking running R code from vignettes ...
    ```
      ‘IPD-meta-analysis-with-missing-data.Rmd’ using ‘UTF-8’... failed
      ‘IPD-meta-analysis.Rmd’ using ‘UTF-8’... failed
      ‘Imputing-missing-values-in-IPD.Rmd’ using ‘UTF-8’... OK
     ERROR
    Errors in running code in vignettes:
    when running code in ‘IPD-meta-analysis-with-missing-data.Rmd’
      ...
    +     X <- with(current.data, apply(current.data[, c(" ..." ... [TRUNCATED] 
    
      When sourcing ‘IPD-meta-analysis-with-missing-data.R’:
    ...
    +     n.iter = 5000)
    
      When sourcing ‘IPD-meta-analysis.R’:
    Error: .onLoad failed in loadNamespace() for 'rjags', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object 
```

