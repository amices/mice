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

# mfrmr (0.2.4)

* GitHub: <https://github.com/Ryuya-dot-com/mfrmr>
* Email: <mailto:ryuya.komuro.c4@tohoku.ac.jp>
* GitHub mirror: <https://github.com/cran/mfrmr>

Run `revdepcheck::revdep_details(, "mfrmr")` for more info

## Newly broken

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
         'test-anchor-migration.R:61:3', 'test-gmfrm-em.R:629:3',
         'test-gmfrm-public-workflow.R:76:3', 'test-gpcm-inference-extensions.R:22:3',
         'test-interaction-em.R:149:3', 'test-jml-public-workflow.R:123:3',
         'test-jml-public-workflow.R:139:3', 'test-marginal-fit-availability.R:179:3',
         'test-random-rater.R:202:3', 'test-random-rater.R:216:3',
         'test-random-rater.R:250:3', 'test-testlet.R:123:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-feature-clustering.R:239:3'): mice imputed logical and ordered features retain their meaning ──
       Expected `mfrm_cluster_imputed(x, bad, x$missing, 2)` to throw a error.
       
       [ FAIL 1 | WARN 0 | SKIP 13 | PASS 5256 ]
       Error:
       ! Test failures.
       Execution halted
     ```

## In both

*   checking running R code from vignettes ...
     ```
     ...
       ‘mfrmr-networks-and-reporting.Rmd’ using ‘UTF-8’... OK
       ‘mfrmr-portable-calibration.Rmd’ using ‘UTF-8’... OK
       ‘mfrmr-random-raters.Rmd’ using ‘UTF-8’... OK
       ‘mfrmr-rating-agreement.Rmd’ using ‘UTF-8’... OK
       ‘mfrmr-reporting-and-apa.Rmd’ using ‘UTF-8’... OK
       ‘mfrmr-response-imputation.Rmd’ using ‘UTF-8’... OK
       ‘mfrmr-screening-performance.Rmd’ using ‘UTF-8’... OK
       ‘mfrmr-testlet-applications.Rmd’ using ‘UTF-8’... failed
       ‘mfrmr-testlets.Rmd’ using ‘UTF-8’... OK
       ‘mfrmr-visual-diagnostics.Rmd’ using ‘UTF-8’... OK
       ‘mfrmr-workflow.Rmd’ using ‘UTF-8’... OK
      ERROR
     Errors in running code in vignettes:
     when running code in ‘mfrmr-testlet-applications.Rmd’
       ...
     
     > plan_results <- list()
     
     > for (model in names(fits)) for (plan in names(plans)) {
     +     key <- paste(model, plan, sep = ": ")
     +     plan_results[[key]] <- evaluate_parallel_p .... [TRUNCATED] 
     
       When sourcing ‘mfrmr-testlet-applications.R’:
     Error: could not find function "evaluate_parallel_plan"
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

# RNAseqCovarImpute (1.10.0)

* GitHub: <https://github.com/brennanhilton/RNAseqCovarImpute>
* Email: <mailto:brennanhilton@gmail.com>

Run `revdepcheck::revdep_details(, "RNAseqCovarImpute")` for more info

## Newly broken

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
       
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 3 ]
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-combine_rubins.R:25:5'): consistent results ──────────────────
       Expected `final_res$coef_combined[1]` to equal `-0.02099867`.
       Differences:
         `actual`: -0.0213
       `expected`: -0.0210
       
       
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 3 ]
       Error:
       ! Test failures.
       Execution halted
     ```

## In both

*   checking R code for possible problems ... NOTE
     ```
     ...
     combine_rubins: no visible binding for global variable
       ‘combined_p_bayes’
     limmavoom_imputed_data_list_helper: no visible binding for global
       variable ‘i’
     limmavoom_imputed_data_list_helper: no visible binding for global
       variable ‘lm_predictor’
     limmavoom_imputed_data_pca: no visible binding for global variable
       ‘probe’
     lowess_all_gene_bins: no visible binding for global variable ‘gene_bin’
     lowess_all_gene_bins: no visible binding for global variable ‘i’
     voom_master_lowess: no visible global function definition for ‘is’
     voom_master_lowess: no visible global function definition for ‘new’
     voom_master_lowess: no visible global function definition for ‘lowess’
     voom_master_lowess: no visible global function definition for
       ‘approxfun’
     voom_sx_sy: no visible global function definition for ‘is’
     voom_sx_sy: no visible global function definition for ‘new’
     Undefined global functions or variables:
       approxfun coef_combined combined_p combined_p_bayes gene_bin i is
       lm_predictor lowess new probe rubins_t_bayes
     Consider adding
       importFrom("methods", "is", "new")
       importFrom("stats", "approxfun", "lowess")
     to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
     contains 'methods').
     ```

*   checking Rd \usage sections ... NOTE
     ```
     Documented arguments not in \usage in Rd file 'impute_gene_bin_helper.Rd':
       ‘DGE’ ‘param’
     
     Functions with \usage entries need to have the appropriate \alias
     entries, and all their arguments documented.
     The \usage entries must correspond to syntactically valid R code.
     See chapter ‘Writing R documentation files’ in the ‘Writing R
     Extensions’ manual.
     ```

# RSquaredMI (0.2.0)

* GitHub: <https://github.com/karchjd/RsquaredMI>
* Email: <mailto:j.d.karch@fsw.leidenuniv.nl>
* GitHub mirror: <https://github.com/cran/RSquaredMI>

Run `revdepcheck::revdep_details(, "RSquaredMI")` for more info

## Newly broken

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
       Expected `res` to equal `exp`.
       Differences:
         `actual`: 0.48 0.47 0.50 0.48 0.46 0.47 0.48 0.48 0.48
       `expected`: 0.36 0.36 0.39 0.37 0.34 0.36 0.36 0.36 0.36
       
       Backtrace:
           ▆
        1. └─RSquaredMI (local) check_alternatives(res) at test-RsquaredMI.R:123:3
        2.   └─RSquaredMI (local) check_numbers(...) at test-RsquaredMI.R:29:3
        3.     └─testthat::expect_equal(res, exp, ignore_attr = TRUE) at test-RsquaredMI.R:2:3
       
       [ FAIL 26 | WARN 0 | SKIP 0 | PASS 4 ]
       Error:
       ! Test failures.
       Execution halted
     ```

