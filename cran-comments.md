cran-comments
================

## Release summary

`mice 3.18.0` adds new features, bug fixes, and documentation
improvements. See <https://github.com/amices/mice/blob/master/NEWS.md>

## Test environments

``` r
packageVersion("mice")
```

    ## [1] '3.18.0'

``` r
R.Version()$version.string
```

    ## [1] "R version 4.5.0 (2025-04-11)"

## Local checks

NOTE: Run in OSX terminal, not in Rstudio terminal.

``` bash
env _R_CHECK_DEPENDS_ONLY_=true 
R CMD check mice_3.18.0.tar.gz

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

### revdepcheck results

## revdepcheck results

We checked 135 reverse dependencies (129 from CRAN + 6 from
Bioconductor), comparing R CMD check results across CRAN and dev
versions of this package.

- We saw 1 new problems
- We failed to check 12 packages

Issues with CRAN packages are summarised below.

### New problems

(This reports the first line of each new failure)

- bipd checking dependencies in R code …sh: line 1: 68994 Segmentation
  fault: 11 R_DEFAULT_PACKAGES=NULL
  ‘/Library/Frameworks/R.framework/Resources/bin/R’ –vanilla –no-echo
  2\>&1 \<
  ‘/var/folders/5\_/g85d42yj50b6lrjq4rzjzg8w0000gn/T//RtmpRR1uev/file10a3acf967c2’

### Failed to check

- BGGM (NA)
- brms (NA)
- clusterMI (NA)
- dynr (NA)
- Hmisc (NA)
- logistf (NA)
- miceadds (NA)
- miceFast (NA)
- mixgb (NA)
- RefBasedMI (NA)
- rms (NA)
- rmsb (NA)

All problems were due to installation issues. As far as I call tell,
none of these problem is related to `mice`

See <https://github.com/amices/mice/tree/master/revdep> for details.
