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

# basecamb (1.1.5)

* GitHub: <https://github.com/codeblue-team/basecamb>
* Email: <mailto:peter@kmarquardt.de>
* GitHub mirror: <https://github.com/cran/basecamb>

Run `revdepcheck::revdep_details(, "basecamb")` for more info

## Newly broken

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
       0 out of 5 global p-values were smaller than 0.05
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 83 ]
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test_cox.zph.mids.R:13:3'): cox.zph.mids handles cases correctly ──
       Expected `mean(cox.zph.mids(cox_mod, imputed_data))` to equal 0.6588979.
       Differences:
         `actual`: 0.672
       `expected`: 0.659
       
       
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 83 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# parameters (0.30.0)

* GitHub: <https://github.com/easystats/parameters>
* Email: <mailto:officialeasystats@gmail.com>
* GitHub mirror: <https://github.com/cran/parameters>

Run `revdepcheck::revdep_details(, "parameters")` for more info

## Newly broken

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
       • TODO: fix this test (1): 'test-model_parameters.lqmm.R:40:3'
       • empty test (2): 'test-wrs2.R:69:1', 'test-wrs2.R:81:1'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-pool_parameters.R:10:3'): pooled parameters ──────────────────
       Expected `pp$df_error` to equal `c(9.2225, 8.1903, 3.6727, 10.264, 6.4385)`.
       Differences:
         `actual`: 13.73 7.35 11.68  8.68 10.88
       `expected`:  9.22 8.19  3.67 10.26  6.44
       
       
       [ FAIL 1 | WARN 2 | SKIP 138 | PASS 925 ]
       Error:
       ! Test failures.
       Execution halted
     ```

