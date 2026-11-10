# autoReg (0.3.5)

* GitHub: <https://github.com/cardiomoon/autoReg>
* Email: <mailto:cardiomoon@gmail.com>
* GitHub mirror: <https://github.com/cran/autoReg>

Run `revdepcheck::revdep_details(, "autoReg")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     Warning: Number of logged events: 1
     Error in `mutate()`:
     ℹ In argument: `lower = exp(.data$`2.5 %`)`.
     Caused by error in `` .data$`2.5 %` ``:
     ! Column `2.5 %` not found in `.data`.
     Backtrace:
          ▆
       1. ├─autoReg::autoReg(fit, uni = FALSE, imputed = TRUE)
       2. ├─autoReg:::autoReg.glm(fit, uni = FALSE, imputed = TRUE)
       3. │ └─autoReg::autoReg_sub(fit = x, ...)
       4. │   └─autoReg::imputedReg(fit2, data = data1, ...)
       5. │     └─... %>% ...
       6. ├─dplyr::mutate(...)
       7. ├─dplyr:::mutate.data.frame(...)
       8. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
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
     ...
     Errors in running code in vignettes:
     when running code in ‘Automatic_Regression_Modeling.Rmd’
       ...
       ..$ n           : chr [1:35] "(N=1)" "(N=3)" "(N=1)" "(N=4)" ...
      - attr(*, "yvars")= chr "Sepal.Length"
     
     > fit2 = imputedReg(fit1, m = 20, seed = 1234)
     
       When sourcing ‘Automatic_Regression_Modeling.R’:
     Error: ℹ In argument: `lower = .data$`2.5 %``.
     Caused by error in `` .data$`2.5 %` ``:
     ! Column `2.5 %` not found in `.data`.
     Execution halted
     when running code in ‘Getting_started.Rmd’
       ...
      - attr(*, "model")= chr "glm"
     
     > autoReg(fit, imputed = TRUE) %>% myft()
     Warning: Number of logged events: 5
     
       When sourcing ‘Getting_started.R’:
     Error: ℹ In argument: `lower = exp(.data$`2.5 %`)`.
     Caused by error in `` .data$`2.5 %` ``:
     ! Column `2.5 %` not found in `.data`.
     Execution halted
     ```

# bipd (0.3)

* Email: <mailto:swj8874@gmail.com>
* GitHub mirror: <https://github.com/cran/bipd>

Run `revdepcheck::revdep_details(, "bipd")` for more info

## Newly broken

*   checking dependencies in R code ...sh: line 1: 13687 Segmentation fault: 11  R_DEFAULT_PACKAGES=NULL '/Library/Frameworks/R.framework/Resources/bin/R' --vanilla --no-echo 2>&1 < '/var/folders/5_/g85d42yj50b6lrjq4rzjzg8w0000gn/T//RtmpM2y2Bj/file323367539f29'
     ```
      NOTE
     
      *** caught segfault ***
     address 0x2720657661682820, cause 'invalid permissions'
     
     Traceback:
      1: dyn.load(file, DLLpath = DLLpath, ...)
      2: library.dynam(lib, package, package.lib)
      3: loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
      4: asNamespace(ns)
      5: namespaceImportFrom(ns, loadNamespace(j <- i[[1L]], c(lib.loc,     .libPaths()), versionCheck = vI[[j]]), i[[2L]], from = package)
      6: loadNamespace(p)
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

*   checking dependencies in R code ...sh: line 1: 12569 Segmentation fault: 11  R_DEFAULT_PACKAGES=NULL '/Library/Frameworks/R.framework/Resources/bin/R' --vanilla --no-echo 2>&1 < '/var/folders/5_/g85d42yj50b6lrjq4rzjzg8w0000gn/T//RtmpDYDlp0/file2f8b6d997e06'
     ```
      NOTE
     
      *** caught segfault ***
     address 0x2720657661682820, cause 'invalid permissions'
     
     Traceback:
      1: dyn.load(file, DLLpath = DLLpath, ...)
      2: library.dynam(lib, package, package.lib)
      3: loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
      4: asNamespace(ns)
      5: namespaceImportFrom(ns, loadNamespace(j <- i[[1L]], c(lib.loc,     .libPaths()), versionCheck = vI[[j]]), i[[2L]], from = package)
      6: loadNamespace(p)
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
     ...
     Errors in running code in vignettes:
     when running code in ‘IPD-meta-analysis-with-missing-data.Rmd’
       ...
     +     X <- with(current.data, apply(current.data[, c(" ..." ... [TRUNCATED] 
     
       When sourcing ‘IPD-meta-analysis-with-missing-data.R’:
     Error: .onLoad failed in loadNamespace() for 'rjags', details:
       call: dyn.load(file, DLLpath = DLLpath, ...)
       error: unable to load shared object '/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/bipd/rjags/libs/rjags.so':
       dlopen(/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/bipd/rjags/libs/rjags.so, 0x000A): Library not loaded: /usr/local/lib/libjags.4.dylib
       Referenced from: <337070A2-BC15-3117-B643-96612554E437> /Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/bipd/rjags/libs/rjags.so
       Reason: tried: '/usr/local/lib/libjags.4.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64e' or 'arm64e.v1' or 'arm64' or 'arm64')), '/System/Volumes/Preboot/Cryptexes/OS/usr/local/lib/libjags.4.dylib' (no such file), '/usr/local/lib/libjags.4.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64e' or 'arm64e.v1' or 'arm64' or 'arm64')), '/Library/Frameworks/R.framework/Resources/
     Execution halted
     when running code in ‘IPD-meta-analysis.Rmd’
       ...
     +     n.iter = 5000)
     
       When sourcing ‘IPD-meta-analysis.R’:
     Error: .onLoad failed in loadNamespace() for 'rjags', details:
       call: dyn.load(file, DLLpath = DLLpath, ...)
       error: unable to load shared object '/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/bipd/rjags/libs/rjags.so':
       dlopen(/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/bipd/rjags/libs/rjags.so, 0x000A): Library not loaded: /usr/local/lib/libjags.4.dylib
       Referenced from: <337070A2-BC15-3117-B643-96612554E437> /Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/bipd/rjags/libs/rjags.so
       Reason: tried: '/usr/local/lib/libjags.4.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64e' or 'arm64e.v1' or 'arm64' or 'arm64')), '/System/Volumes/Preboot/Cryptexes/OS/usr/local/lib/libjags.4.dylib' (no such file), '/usr/local/lib/libjags.4.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64e' or 'arm64e.v1' or 'arm64' or 'arm64')), '/Library/Frameworks/R.framework/Resources/
     Execution halted
     ```

# broom.mixed (0.2.9.7)

* GitHub: <https://github.com/bbolker/broom.mixed>
* Email: <mailto:bolker@mcmaster.ca>
* GitHub mirror: <https://github.com/cran/broom.mixed>

Run `revdepcheck::revdep_details(, "broom.mixed")` for more info

## Newly broken

*   checking examples ...sh: line 1: 24508 Segmentation fault: 11  LANGUAGE=en _R_CHECK_INTERNALS2_=1 '/Library/Frameworks/R.framework/Resources/bin/R' --vanilla > 'broom.mixed-Ex.Rout' 2>&1 < 'broom.mixed-Ex.R'
     ```
     ...
     Loading required package: Matrix
     Loading required package: mediation
     Loading required package: MASS
     Loading required package: mvtnorm
     
      *** caught segfault ***
     address 0x7469686372612065, cause 'invalid permissions'
     
     Traceback:
      1: dyn.load(file, DLLpath = DLLpath, ...)
      2: library.dynam(lib, package, package.lib)
      3: loadNamespace(package, lib.loc)
      4: doTryCatch(return(expr), name, parentenv, handler)
      5: tryCatchOne(expr, names, parentenv, handlers[[1L]])
      6: tryCatchList(expr, classes, parentenv, handlers)
      7: tryCatch({    attr(package, "LibPath") <- which.lib.loc    ns <- loadNamespace(package, lib.loc)    env <- attachNamespace(ns, pos = pos, deps, exclude, include.only)}, error = function(e) {    P <- if (!is.null(cc <- conditionCall(e)))         paste(" in", deparse(cc)[1L])    else ""    msg <- gettextf("package or namespace load failed for %s%s:\n %s",         sQuote(package), P, conditionMessage(e))    if (logical.return && !quietly)         message(paste("Error:", msg), domain = NA)    else stop(msg, call. = FALSE, domain = NA)})
      8: library(pkg, character.only = TRUE, logical.return = TRUE, lib.loc = lib.loc,     quietly = quietly)
      9: .getRequiredPackages2(pkgInfo, quietly = quietly, lib.loc = c(lib.loc,     .libPaths()))
     10: library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,     warn.conflicts = warn.conflicts, quietly = quietly, mask.ok = mask.ok,     exclude = exclude, include.only = include.only, attach.required = attach.required)
     11: doTryCatch(return(expr), name, parentenv, handler)
     12: tryCatchOne(expr, names, parentenv, handlers[[1L]])
     13: tryCatchList(expr, classes, parentenv, handlers)
     14: tryCatch(library(package, lib.loc = lib.loc, character.only = TRUE,     logical.return = TRUE, warn.conflicts = warn.conflicts, quietly = quietly,     mask.ok = mask.ok, exclude = exclude, include.only = include.only,     attach.required = attach.required), error = function(e) e)
     15: require("mediation")
     An irrecoverable exception occurred. R is aborting now ...
     ```

## Newly fixed

*   checking examples ...sh: line 1: 24509 Segmentation fault: 11  LANGUAGE=en _R_CHECK_INTERNALS2_=1 '/Library/Frameworks/R.framework/Resources/bin/R' --vanilla > 'broom.mixed-Ex.Rout' 2>&1 < 'broom.mixed-Ex.R'
     ```
     ...
     Loading required package: Matrix
     Loading required package: mediation
     Loading required package: MASS
     Loading required package: mvtnorm
     
      *** caught segfault ***
     address 0x7469686372612065, cause 'invalid permissions'
     
     Traceback:
      1: dyn.load(file, DLLpath = DLLpath, ...)
      2: library.dynam(lib, package, package.lib)
      3: loadNamespace(package, lib.loc)
      4: doTryCatch(return(expr), name, parentenv, handler)
      5: tryCatchOne(expr, names, parentenv, handlers[[1L]])
      6: tryCatchList(expr, classes, parentenv, handlers)
      7: tryCatch({    attr(package, "LibPath") <- which.lib.loc    ns <- loadNamespace(package, lib.loc)    env <- attachNamespace(ns, pos = pos, deps, exclude, include.only)}, error = function(e) {    P <- if (!is.null(cc <- conditionCall(e)))         paste(" in", deparse(cc)[1L])    else ""    msg <- gettextf("package or namespace load failed for %s%s:\n %s",         sQuote(package), P, conditionMessage(e))    if (logical.return && !quietly)         message(paste("Error:", msg), domain = NA)    else stop(msg, call. = FALSE, domain = NA)})
      8: library(pkg, character.only = TRUE, logical.return = TRUE, lib.loc = lib.loc,     quietly = quietly)
      9: .getRequiredPackages2(pkgInfo, quietly = quietly, lib.loc = c(lib.loc,     .libPaths()))
     10: library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,     warn.conflicts = warn.conflicts, quietly = quietly, mask.ok = mask.ok,     exclude = exclude, include.only = include.only, attach.required = attach.required)
     11: doTryCatch(return(expr), name, parentenv, handler)
     12: tryCatchOne(expr, names, parentenv, handlers[[1L]])
     13: tryCatchList(expr, classes, parentenv, handlers)
     14: tryCatch(library(package, lib.loc = lib.loc, character.only = TRUE,     logical.return = TRUE, warn.conflicts = warn.conflicts, quietly = quietly,     mask.ok = mask.ok, exclude = exclude, include.only = include.only,     attach.required = attach.required), error = function(e) e)
     15: require("mediation")
     An irrecoverable exception occurred. R is aborting now ...
     ```

## In both

*   checking tests ...
     ```
       Running ‘test-all.R’/Library/Frameworks/R.framework/Resources/bin/BATCH: line 60: 24802 Segmentation fault: 11  ${R_HOME}/bin/R -f ${in} ${opts} ${R_BATCH_OPTIONS} > ${out} 2>&1
     
      ERROR
     Running the tests in ‘tests/test-all.R’ failed.
     Last 13 lines of output:
       29: withRestarts(tryCatch(withCallingHandlers({    eval(code, test_env)    new_expectations <- the$test_expectations > starting_expectations    if (snapshot_skipped) {        skip("On CRAN")    }    else if (!new_expectations && skip_on_empty) {        skip_empty()    }}, expectation = handle_expectation, packageNotFoundError = function(e) {    if (on_cran()) {        skip(paste0("{", e$package, "} is not installed."))    }}, snapshot_on_cran = function(cnd) {    snapshot_skipped <<- TRUE    invokeRestart("muffle_cran_snapshot")}, skip = handle_skip, warning = handle_warning, message = handle_message,     error = handle_error, interrupt = handle_interrupt), error = handle_fatal),     end_test = function() {    })
       30: test_code(code = exprs, env = env, reporter = get_reporter() %||%     StopReporter$new())
       31: source_file(path, env = env(env), desc = desc, shuffle = shuffle,     error_call = error_call)
       32: FUN(X[[i]], ...)
       33: lapply(test_paths, test_one_file, env = env, desc = desc, shuffle = shuffle,     error_call = error_call)
       34: doTryCatch(return(expr), name, parentenv, handler)
       35: tryCatchOne(expr, names, parentenv, handlers[[1L]])
       36: tryCatchList(expr, classes, parentenv, handlers)
       37: tryCatch(code, testthat_abort_reporter = function(cnd) {    cat(conditionMessage(cnd), "\n")    NULL})
       38: with_reporter(reporters$multi, lapply(test_paths, test_one_file,     env = env, desc = desc, shuffle = shuffle, error_call = error_call))
       39: test_files_serial(test_dir = test_dir, test_package = test_package,     test_paths = test_paths, load_helpers = load_helpers, reporter = reporter,     env = env, stop_on_failure = stop_on_failure, stop_on_warning = stop_on_warning,     desc = desc, load_package = load_package, shuffle = shuffle,     error_call = error_call)
       40: test_files(test_dir = path, test_paths = test_paths, test_package = package,     reporter = reporter, load_helpers = load_helpers, env = env,     stop_on_failure = stop_on_failure, stop_on_warning = stop_on_warning,     load_package = load_package, parallel = parallel, shuffle = shuffle)
       41: test_dir("testthat", package = package, reporter = reporter,     ..., load_package = "installed")
       42: test_check("broom.mixed")
       An irrecoverable exception occurred. R is aborting now ...
     ```

# pminternal (0.1.0)

* GitHub: <https://github.com/stephenrho/pminternal>
* Email: <mailto:steverho89@gmail.com>
* GitHub mirror: <https://github.com/cran/pminternal>

Run `revdepcheck::revdep_details(, "pminternal")` for more info

## Newly broken

*   checking running R code from vignettes ...
     ```
       ‘missing-data.Rmd’ using ‘UTF-8’... OK
       ‘pminternal.Rmd’ using ‘UTF-8’... failed
       ‘validate-examples.Rmd’ using ‘UTF-8’... OK
      ERROR
     Errors in running code in vignettes:
     when running code in ‘pminternal.Rmd’
       ...
     +     ]
     
     > mod <- glm(y ~ ., data = gusto, family = "binomial")
     
     > mod_iv <- validate(mod, B = 20)
     It is recommended that B >= 200 for bootstrap validation
     
       When sourcing ‘pminternal.R’:
     Error: creation of server socket failed: port 11349 cannot be opened
     Execution halted
     ```

# pre (1.0.9)

* GitHub: <https://github.com/marjoleinF/pre>
* Email: <mailto:m.fokkema@fsw.leidenuniv.nl>
* GitHub mirror: <https://github.com/cran/pre>

Run `revdepcheck::revdep_details(, "pre")` for more info

## Newly broken

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
       [ FAIL 1 | WARN 1 | SKIP 0 | PASS 130 ]
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test_pre_misc.R:94:3'): cvpre gives previous results with airquality data ──
       Error in `serverSocket(port = port)`: creation of server socket failed: port 11725 cannot be opened
       Backtrace:
           ▆
        1. └─parallel::makeCluster(2L) at test_pre_misc.R:94:3
        2.   └─parallel::makePSOCKcluster(names = spec, ...)
        3.     └─base::serverSocket(port = port)
       
       [ FAIL 1 | WARN 1 | SKIP 0 | PASS 130 ]
       Error:
       ! Test failures.
       Execution halted
     ```

