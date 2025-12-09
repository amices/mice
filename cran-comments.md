cran-comments
================

## Release summary

`mice 3.19.0` adds new features, bug fixes, and enhancements. See
<https://github.com/amices/mice/blob/master/NEWS.md>

## Test environments

``` r
packageVersion("mice")
```

    ## [1] '3.19.0'

``` r
R.Version()$version.string
```

    ## [1] "R version 4.5.2 (2025-10-31)"

## Local checks

NOTE: Run in OSX terminal, not in Rstudio terminal.

``` bash
env _R_CHECK_DEPENDS_ONLY_=true 
R CMD check mice_3.19.0.tar.gz

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

We checked 148 reverse dependencies (140 from CRAN + 8 from
Bioconductor), comparing R CMD check results across CRAN and dev
versions of this package.

- We saw 1 new problems
- We failed to check 5 packages

See <https://github.com/amices/mice/tree/master/revdep> for details.
