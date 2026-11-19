cran-comments
================

## Resubmission

This is a resubmission. The CRAN check with MKL reported three test
failures (“Objects equal but not identical”) in `test-cluster-types.R`
and `test-complete.R`. These tests compared floating-point results from
different code paths with `expect_identical()`. They now use
`expect_equal()`, so they no longer depend on the last bits produced by
the BLAS library.

## Release summary

`mice 3.20.0` is a major update of `mice 3.19.0`. It adds parallel
imputation through the `future` framework, progress reporting, new
imputation and pooling options, and many bug fixes. It also moves
`glmnet` and `mitml` from Imports to Suggests, and requires R \>= 4.1.0.

Some changes are intended and change results or output for existing
code, most notably a fix of the matching of tied predicted values in
predictive mean matching (#757), and the broom-style names
`conf.low`/`conf.high` of the confidence interval columns (#719). These
affect a few reverse dependencies, see below.

See <https://github.com/amices/mice/blob/master/NEWS.md>

## Test environments

``` r
read.dcf("DESCRIPTION", fields = "Version")[[1]]
```

    ## [1] "3.20.0"

``` r
R.Version()$version.string
```

    ## [1] "R version 4.6.2 (2026-10-29)"

## Local checks

NOTE: Run in OSX terminal, not in Rstudio terminal.

``` bash
R CMD build mice
R CMD check --as-cran mice_3.20.0.tar.gz

Status: OK
```

On R 4.6.2 (2026-10-29), platform aarch64-apple-darwin23 (macOS).

``` r
devtools::check(env_vars = c('_R_CHECK_DEPENDS_ONLY_' = "true"))
...
Status: OK
```

## Remote checks

### win-builder

Uploaded through <https://win-builder.r-project.org/upload.aspx> on 18
November 2026:

``` r
R Under development (unstable) (2026-11-17 r90926 ucrt)
Status: OK

R version 4.6.2 (2026-10-29 ucrt)
Status: OK
```

### Platform checks

We checked the following platforms:

``` r
macos-latest (release)
windows-latest (release)
ubuntu-latest (devel)
ubuntu-latest (release)
ubuntu-latest (oldrel-1)
```

Status: OK (GitHub Actions, 18 November 2026)

For details: <https://github.com/amices/mice/actions>

## Downstream dependencies

### Code

Note: Inactivate `credentials::set_github_pat()`

``` r
library(revdepcheck)
revdep_reset()
revdep_check(pkg = ".", num_workers = 10, quiet = FALSE)
```

``` r
# revdepcheck::revdep_summary()
```

## revdepcheck results

We checked 206 reverse dependencies (199 from CRAN + 7 from
Bioconductor), comparing R CMD check results across CRAN and dev
versions of this package.

- We saw 4 new problems in CRAN packages, and 1 in a Bioconductor
  package
- We failed to check 4 packages

All 5 new problems result from intended changes in this release. We
notified the maintainers of these packages by email on 13 November 2026.
`RSquaredMI`, which had the same problem, has since been updated on CRAN
and now passes.

- `autoReg` reads the confidence interval columns from
  `summary(pool(fit), conf.int = TRUE)` by their old `confint()`-style
  names (`` `2.5 %` ``/`` `97.5 %` ``), which this release renames to
  `conf.low`/`conf.high` (broom convention, \#719).
- `basecamb`, `parameters` and `RNAseqCovarImpute` (Bioconductor) have
  tests that compare against exact numbers from a seeded `mice()` run
  with the default method `pmm`. This release fixes a bug in the
  matching of tied predicted values (#757), so the imputations for a
  given seed change. The packages themselves are not at fault; the
  expected values need updating.
- `mfrmr` has a test that injects the value `2` into the imputations of
  a logical variable and expects an error. This release keeps logical
  variables logical in `complete()` (#764), so no error is raised.

The 4 packages we failed to check (`dynr`, `idem`, `pguIMP`, `rmsb`)
could not be installed locally because of configure, Stan and rJava
problems, unrelated to mice.

See <https://github.com/amices/mice/tree/master/revdep> for details.
