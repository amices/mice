cran-comments
================

## Resubmission

This is a second resubmission of `mice 3.20.0`. The code is the same as
in the first resubmission of 19 November 2026. In response to that
submission, CRAN advised us to resubmit after one or two weeks, so that
the maintainers of affected reverse dependencies could update their
packages. We reran the reverse dependency check on 26 November 2026; see
“Downstream dependencies” below.

Changes in the first resubmission, compared with the original
submission:

- The CRAN check with MKL reported three test failures (“Objects equal
  but not identical”) in `test-cluster-types.R` and `test-complete.R`.
  These tests compared floating-point results from different code paths
  with `expect_identical()`. They now use `expect_equal()`, so they no
  longer depend on the last bits produced by the BLAS library.
- The reverse dependency check found that `ggmice` failed because
  `mice::fluxplot()` could not load `MASS`. `MASS` was in `Suggests`,
  but `fluxplot()` and `mice.impute.polr()` use it unconditionally. In
  earlier versions `MASS` was always installed through `mitml`, which
  this release moves to `Suggests`. `MASS` now moves to `Imports`.
- Following this, we ran `R CMD check` with
  `_R_CHECK_DEPENDS_ONLY_=true`. Some examples and tests used `lme4`,
  `mitml`, `pan`, `ranger` or `survival` without checking for them. They
  are now conditional on these packages, and `nelsonaalen()` asks for
  `survival` on demand.

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

We reran the check on 26 November 2026 against the resubmitted version
(commit 2cb25c6). We checked 206 reverse dependencies (197 from CRAN + 9
from Bioconductor), comparing R CMD check results across CRAN and dev
versions of this package.

- We saw 3 new problems
- We failed to check 5 packages

All 3 new problems result from intended changes in this release. We
notified the maintainers of these packages by email on 13 November 2026.

- `autoReg` reads the confidence interval columns from
  `summary(pool(fit), conf.int = TRUE)` by their old `confint()`-style
  names (`` `2.5 %` ``/`` `97.5 %` ``), which this release renames to
  `conf.low`/`conf.high` (broom convention, \#719).
- `basecamb` and `parameters` have tests that compare against exact
  numbers from a seeded `mice()` run with the default method `pmm`. This
  release fixes a bug in the matching of tied predicted values (#757),
  so the imputations for a given seed change. The packages themselves
  are not at fault; the expected values need updating.

Since the first submission, three packages that the earlier check
flagged are resolved: `RSquaredMI` (0.2.1 on CRAN) and
`RNAseqCovarImpute` (Bioconductor 1.12.0) now pass, and `mfrmr` (0.3.2)
no longer depends on `mice`.

The CRAN incoming check found four more packages with changes to worse.
Our local check skipped vignettes and ran on macOS, so it missed them:

- `ggmice`: a bug in `mice`, fixed in this resubmission (see above).
- `smdi` (vignette) reads the old confidence interval column names, as
  `autoReg` does (#719).
- `ggeffects` has a test, skipped on macOS, that compares against exact
  numbers from a seeded `mice()` run with `pmm` (#757).
- `FuzzyImputationTest` subsets a matrix of imputed rows without
  `drop = FALSE`. Whether this fails depends on the imputed values,
  which change with the `pmm` fix (#757). The test passes locally with
  both the old and the new `mice`.

We notified the maintainers of `ggeffects`, `smdi` and
`FuzzyImputationTest` on 19 November 2026. All four pass in the rerun.

The 5 packages we failed to check (`dynr`, `idem`, `pguIMP`, `rmsb`,
`NADIA`) could not be installed locally because of configure, Stan,
rJava and Bioconductor (`sva`) installation problems, unrelated to mice.

See <https://github.com/amices/mice/tree/master/revdep> for details.
