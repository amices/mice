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
    ```
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
    ```

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
    ```
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
    ```

## Newly fixed

*   checking dependencies in R code ...sh: line 1: 26657 Segmentation fault: 11  R_DEFAULT_PACKAGES=NULL '/Library/Frameworks/R.framework/Resources/bin/R' --vanilla --no-echo 2>&1 < '/var/folders/5_/g85d42yj50b6lrjq4rzjzg8w0000gn/T//Rtmpi8gCya/file6689272a04f8'
    ```
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
    ```

