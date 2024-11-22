# autoReg

<details>

* Version: 0.3.3
* GitHub: https://github.com/cardiomoon/autoReg
* Source code: https://github.com/cran/autoReg
* Date/Publication: 2023-11-14 05:53:27 UTC
* Number of recursive dependencies: 212

Run `revdepcheck::revdep_details(, "autoReg")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘autoReg-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoReg
    > ### Title: Perform univariable and multivariable regression and stepwise
    > ###   backward regression automatically
    > ### Aliases: autoReg autoReg.lm autoReg.glm autoReg.coxph autoReg.survreg
    > 
    > ### ** Examples
    > 
    ...
      9. │   ├─base::withCallingHandlers(...)
     10. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
     11. │     └─mask$eval_all_mutate(quo)
     12. │       └─dplyr (local) eval()
     13. ├─2.5 %
     14. ├─rlang:::`$.rlang_data_pronoun`(.data, `2.5 %`)
     15. │ └─rlang:::data_pronoun_get(...)
     16. └─rlang:::abort_data_pronoun(x, call = y)
     17.   └─rlang::abort(msg, "rlang_error_data_pronoun_not_found", call = call)
    Execution halted
    ```

*   checking running R code from vignettes ...
    ```
      ‘Automatic_Regression_Modeling.Rmd’ using ‘UTF-8’... failed
      ‘Bootstrap_Prediction.Rmd’ using ‘UTF-8’... OK
      ‘Getting_started.Rmd’ using ‘UTF-8’... failed
      ‘Statiastical_test_in_gaze.Rmd’ using ‘UTF-8’... OK
      ‘Survival.Rmd’ using ‘UTF-8’... OK
     ERROR
    Errors in running code in vignettes:
    when running code in ‘Automatic_Regression_Modeling.Rmd’
      ...
                            virginica  (N=50)    Mean ± SD  6.6 ± 0.6  1.95 (1.75 to 2.14, p<.001)  1.91 (1.68 to 2.14, p<.001) 
    ...
                               Male    489 (52.9%)   463 (51.6%)  0.95 (0.79-1.14, p=.594)  0.95 (0.78-1.15, p=.589)                           
    ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
    
    > autoReg(fit, imputed = TRUE) %>% myft()
    
      When sourcing ‘Getting_started.R’:
    Error: ℹ In argument: `lower = exp(.data$`2.5 %`)`.
    Caused by error in `` .data$`2.5 %` ``:
    ! Column `2.5 %` not found in `.data`.
    Execution halted
    ```

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

*   checking dependencies in R code ...sh: line 1: 28198 Segmentation fault: 11  R_DEFAULT_PACKAGES=NULL '/Library/Frameworks/R.framework/Resources/bin/R' --vanilla --no-echo 2>&1 < '/var/folders/5_/g85d42yj50b6lrjq4rzjzg8w0000gn/T//Rtmp8uxG38/file6ad8527d0366'
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

*   checking dependencies in R code ...sh: line 1: 28180 Segmentation fault: 11  R_DEFAULT_PACKAGES=NULL '/Library/Frameworks/R.framework/Resources/bin/R' --vanilla --no-echo 2>&1 < '/var/folders/5_/g85d42yj50b6lrjq4rzjzg8w0000gn/T//Rtmpw6OWhC/file6a76216c4045'
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
      error: unable to load shared object '/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/bipd/rjags/libs/rjags.so':
      dlopen(/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/bipd/rjags/libs/rjags.so, 0x000A): Library not loaded: /usr/local/lib/libjags.4.dylib
      Referenced from: <CAF5E1DC-317A-34FE-988A-FB6F7C73D89E> /Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/bipd/rjags/libs/rjags.so
      Reason: tried: '/usr/local/lib/libjags.4.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64e' or 'arm64')), '/System/Volumes/Preboot/Cryptexes/OS/usr/local/lib/libjags.4.dylib' (no such file), '/usr/local/lib/libjags.4.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64e' or 'arm64')), '/Library/Frameworks/R.framework/Resources/lib/libjags.4.dylib' (no such file), '/Library/Java/
    Execution halted
    ```

# finalfit

<details>

* Version: 1.0.8
* GitHub: https://github.com/ewenharrison/finalfit
* Source code: https://github.com/cran/finalfit
* Date/Publication: 2024-07-24 15:20:01 UTC
* Number of recursive dependencies: 176

Run `revdepcheck::revdep_details(, "finalfit")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘finalfit-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: missing_predictorMatrix
    > ### Title: Create predictorMatrix for use with mice
    > ### Aliases: missing_predictorMatrix
    > 
    > ### ** Examples
    > 
    > library(mice)
    ...
     22. │             └─tidyselect:::walk_data_tree(new, data_mask, context_mask)
     23. │               └─tidyselect:::as_indices_sel_impl(...)
     24. │                 └─tidyselect:::as_indices_impl(...)
     25. │                   └─tidyselect:::chr_as_locations(x, vars, call = call, arg = arg)
     26. │                     └─vctrs::vec_as_location(...)
     27. └─vctrs (local) `<fn>`()
     28.   └─vctrs:::stop_subscript_oob(...)
     29.     └─vctrs:::stop_subscript(...)
     30.       └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ...
    ```
      ‘all_plots_examples.Rmd’ using ‘UTF-8’... OK
      ‘all_tables_examples.Rmd’ using ‘UTF-8’... OK
      ‘bootstrap.Rmd’ using ‘UTF-8’... OK
      ‘data_prep.Rmd’ using ‘UTF-8’... OK
      ‘export.Rmd’ using ‘UTF-8’... OK
      ‘finalfit.Rmd’ using ‘UTF-8’... OK
      ‘missing.Rmd’ using ‘UTF-8’... failed
      ‘survival.Rmd’ using ‘UTF-8’... OK
     ERROR
    Errors in running code in vignettes:
    ...
    > fits_pool = fits %>% pool()
    
    > colon_s %>% or_plot(dependent, explanatory, glmfit = fits_pool, 
    +     table_text_size = 4)
    Note: dependent includes missing data. These are dropped.
    
      When sourcing ‘missing.R’:
    Error: Can't select columns that don't exist.
    ✖ Column `2.5 %` doesn't exist.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        doc   4.9Mb
    ```

# pre

<details>

* Version: 1.0.7
* GitHub: https://github.com/marjoleinF/pre
* Source code: https://github.com/cran/pre
* Date/Publication: 2024-01-12 19:30:02 UTC
* Number of recursive dependencies: 150

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
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 127 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test_pre_misc.R:94:3'): cvpre gives previous results with airquality data ──
      Error in `serverSocket(port = port)`: creation of server socket failed: port 11508 cannot be opened
      Backtrace:
          ▆
       1. └─parallel::makeCluster(2L) at test_pre_misc.R:94:3
       2.   └─parallel::makePSOCKcluster(names = spec, ...)
       3.     └─base::serverSocket(port = port)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 127 ]
      Error: Test failures
      Execution halted
    ```

