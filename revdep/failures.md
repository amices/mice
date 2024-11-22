# brms

<details>

* Version: 2.22.0
* GitHub: https://github.com/paul-buerkner/brms
* Source code: https://github.com/cran/brms
* Date/Publication: 2024-09-23 13:00:29 UTC
* Number of recursive dependencies: 203

Run `revdepcheck::revdep_details(, "brms")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: 'cmdstanr', 'mice'
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  9.0Mb
      sub-directories of 1Mb or more:
        R     4.0Mb
        doc   3.6Mb
    ```

# MKinfer

<details>

* Version: 1.2
* GitHub: https://github.com/stamats/MKinfer
* Source code: https://github.com/cran/MKinfer
* Date/Publication: 2024-04-06 10:42:58 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::revdep_details(, "MKinfer")` for more info

</details>

## Newly broken

*   checking whether package ‘MKinfer’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/MKinfer/new/MKinfer.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mice’
    ```

## Installation

### Devel

```
* installing *source* package ‘MKinfer’ ...
** package ‘MKinfer’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘mice’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘MKinfer’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/MKinfer/new/MKinfer.Rcheck/MKinfer’


```
### CRAN

```
* installing *source* package ‘MKinfer’ ...
** package ‘MKinfer’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (MKinfer)


```
# rmsb

<details>

* Version: 1.1-1
* GitHub: NA
* Source code: https://github.com/cran/rmsb
* Date/Publication: 2024-07-08 11:10:03 UTC
* Number of recursive dependencies: 135

Run `revdepcheck::revdep_details(, "rmsb")` for more info

</details>

## In both

*   checking whether package ‘rmsb’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/rmsb/new/rmsb.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: 'cmdstanr', 'mice'
    ```

## Installation

### Devel

```
* installing *source* package ‘rmsb’ ...
** package ‘rmsb’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘rmsb’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/rmsb/new/rmsb.Rcheck/rmsb’


```
### CRAN

```
* installing *source* package ‘rmsb’ ...
** package ‘rmsb’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘rmsb’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/rmsb/old/rmsb.Rcheck/rmsb’


```
