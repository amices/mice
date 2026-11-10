cran-comments
================

## Release summary

`mice 3.19.10` adds new features, bug fixes, and enhancements. See <https://github.com/amices/mice/blob/master/NEWS.md>

## Test environments

``` r
packageVersion("mice")
```

    ## [1] '3.19.10'

``` r
R.Version()$version.string
```

    ## [1] "R version 4.5.2 (2025-10-31)"

## Local checks

NOTE: Run in OSX terminal, not in Rstudio terminal.

``` bash
env _R_CHECK_DEPENDS_ONLY_=true 
R CMD check mice_3.19.10.tar.gz

Status: OK
```

``` r
devtools::check(env_vars = c('_R_CHECK_DEPENDS_ONLY_' = "true"))
...
Status: OK
```

## Remote checks

### win-builder

``` r
devtools::check_win_devel()

Upload error: timeout
```

### Platform checks

We checked the following platforms:

``` r
macos-latest (release)
windows-latest (release)
ubuntu-latest (devel)
ubuntu-latest (release)
ubuntu-latest (oldrel)
```

Status: OK

For details: <https://github.com/amices/mice/actions>

## Downstream dependencies

### Code

Note: Inactivate `credentials::set_github_pat()`

``` r
library(revdepcheck)
revdep_reset()
revdep_check(pkg = ".", num_workers = 12, quiet = FALSE)
```

``` r
# revdepcheck::revdep_summary()
```

## revdepcheck results

We checked 177 reverse dependencies (171 from CRAN + 6 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

-   We saw 5 new problems
-   We failed to check 4 packages

Of the 5 new problems, only `autoReg` is caused by this release: it reads the confidence interval columns from `summary(pool(fit), conf.int = TRUE)` by their old `confint()`-style names (`` `2.5 %` ``/`` `97.5 %` ``), which this version renames to `conf.low`/`conf.high` (broom convention). We have contacted the maintainer about updating `autoReg` accordingly. The remaining new problems (`bipd`, `broom.mixed`, `pminternal`, `pre`) are unrelated environment flakiness (segfaults also seen as "newly fixed" in the same run, or local port conflicts from parallel checking) and not caused by changes in this package.

See <https://github.com/amices/mice/tree/master/revdep> for details.
